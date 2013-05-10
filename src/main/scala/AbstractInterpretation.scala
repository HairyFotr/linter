package com.foursquare.lint

import scala.tools.nsc.{Global}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.symtab.Flags.{IMPLICIT, OVERRIDE, MUTABLE, CASE, LAZY, FINAL}
import com.foursquare.lint.global._

// Warning: Don't try too hard to understand this code, it's a mess and needs
// to be rewritten in a type-safe and transparent way.

class AbstractInterpretation(val global: Global, val unit: GUnit) {
  import global._

  //TODO: move these to utils
  def isUsed(tree: GTree, name: String): Boolean = {
    var used = 0

    /*
    //scala 2.10+
    for(Ident(id) <- t; if id.toString == name) used += 1
    //TODO: Only for select types, also, maybe this doesn't belong in all uses of isUsed (e.g. Assignment right after declaration)
    for(Select(Ident(id), func) <- t; if (func.toString matches "size|length|head|last") && (id.toString == name)) used -= 1
    */
    def findUsed(tree: GTree) {
      for(Ident(id) <- tree.children; if id.toString == name) used += 1
      //TODO: Only for select types, also, maybe this doesn't belong in all uses of isUsed (e.g. Assignment right after declaration)
      for(Select(Ident(id), func) <- tree.children; if (func.toString matches "size|length|head|last") && (id.toString == name)) used -= 1
      
      for(subTree <- tree.children; if (subTree != tree)) findUsed(subTree)
    }
    
    findUsed(tree)
    
    (used > 0)
  }
    
  def checkRegex(reg: String) {
    try {
      reg.r
    } catch {
      case e: java.util.regex.PatternSyntaxException =>
        unit.warning(treePosHolder.pos, "Regex pattern syntax warning: "+e.getDescription)
      case e: Exception =>
    }
  }

  object Values {
    lazy val empty = new Values()
    //def apply(low: Int, high: Int, name: String, isSeq: Boolean, actualSize: Int): Values = new Values(name = name, ranges = Set((low, high)), isSeq = isSeq, actualSize = actualSize)
    //def apply(s: Set[Int], name: String, isSeq: Boolean, actualSize: Int): Values = new Values(name = name, values = s, isSeq = isSeq, actualSize = actualSize)
    def apply(i: Int, name: String = ""): Values = new Values(name = name, values = Set(i))
    def apply(low: Int, high: Int): Values = new Values(ranges = Set((low, high)))
  }
  class Values(
      val ranges: Set[(Int, Int)] = Set[(Int, Int)](),
      val values: Set[Int] = Set[Int](),
      val name: String = "",
      val isSeq: Boolean = false,
      val actualSize: Int = -1
    ) {

    //TODO implement interval tree
    //println(this)
   
    def contains(i: Int): Boolean = (values contains i) || (ranges exists { case (low, high) => i >= low && i <= high })
    def apply(i: Int) = contains(i)
    def exists(func: Int => Boolean) = (values exists func) || (ranges exists { case (low, high) => (low to high) exists func })
    def forall(func: Int => Boolean) = (values forall func) && (ranges forall { case (low, high) => (low to high) forall func })
    
    def addRange(low: Int, high: Int): Values = new Values(ranges + (if(low > high) (high, low) else (low, high)), values, name, false, -1)
    def addValue(i: Int): Values = new Values(ranges, values + i, name, false, -1)
    def addSet(s: Set[Int]): Values = new Values(ranges, values ++ s, name, false, -1)
    def addName(s: String): Values = new Values(ranges, values, s, isSeq, actualSize)
    def addActualSize(s: Int): Values = new Values(ranges, values, name, isSeq, s)
    def toValues = new Values(values = values ++ ranges.flatMap { case (low, high) => (low to high) }, name = name, isSeq = isSeq, actualSize = actualSize)
    
    def isEmpty = this.size == 0
    def nonEmpty = this.size > 0
    def isValue = this.values.size == 1 && this.ranges.isEmpty // TODO: this is stupid and buggy, and woudn't exist if I didn't mix all types into one class
    def getValue = if(isValue) values.head else throw new Exception()
    def isValueForce = (this.values.size == 1 && this.ranges.isEmpty) || (ranges.size == 1 && this.size == 1)
    def getValueForce = if(isValue) values.head else if(ranges.size == 1 && this.size == 1) ranges.head._1 else throw new Exception()

    def max = (values ++ ranges.map(_._2)).max
    def min = (values ++ ranges.map(_._1)).min

    def dropValue(i: Int): Values = new Values(
      ranges.flatMap { case (low, high) =>
        if(i > low && i < high) List((low,i-1), (i+1,high)) else
        if(i == low && i < high) List((low+1,high)) else
        if(i > low && i == high) List((low,high-1)) else
        if(i == low && i == high) Nil else
        List((low, high))
      }, 
      values - i,
      name)

    //beware of some operations over ranges.
    def map(func: Int => Int, rangeSafe: Boolean = true): Values =
      if(rangeSafe) {
        new Values(
          ranges
            .map { case (low, high) => (func(low), func(high)) }
            .map { case (low, high) if low > high => (high, low); case (low, high) => (low, high) },
          values
            .map(func))
      } else {
        if(this.ranges.nonEmpty && this.size <= 100001) //ADD: this only checks small ranges, also can still get slow
          (new Values(values = values ++ ranges.flatMap { case (low, high) => (low to high) }, name = name, isSeq = isSeq, actualSize = actualSize)).map(func)
        else 
          Values.empty
      }
    
    def applyCond(condExpr: Tree): (Values, Values) = {//TODO: return (true, false) ValueSets in one go - applyInverseCond sucks //TODO: true and false cound be possible returns for error msgs
      //println("expr: "+showRaw(condExpr))
      var alwaysHold, neverHold = false
      val out = condExpr match {
        case Apply(Select(expr1, op), List(expr2)) if op == nme.ZAND => //&&
          this.applyCond(expr1)._1.applyCond(expr2)._1
          
        case Apply(Select(expr1, op), List(expr2)) if op == nme.ZOR => //&&
          val (left,right) = (this.applyCond(expr1)._1, this.applyCond(expr2)._1)
          new Values(
            left.ranges ++ right.ranges,
            left.values ++ right.values,
            this.name)

        /// String stuff
        case strFun @ Apply(Select(string, func), params) if string.tpe <:< definitions.StringClass.tpe =>
          computeExpr(strFun)
          
        case strFun @ Select(Apply(scala_augmentString, List(string)), func)
          if (scala_augmentString.toString endsWith "augmentString") =>

          computeExpr(strFun)

        //ADD: expr op expr that handles idents right
        case Apply(Select(Ident(v), op), List(expr)) if v.toString == this.name && this.nonEmpty && computeExpr(expr).isValue => 
        
          val value = computeExpr(expr).getValue
          val out = op match {
            case nme.EQ => 
              if(this.exists(_ == value)) {
                if(this.forall(_ == value)) alwaysHold = true
                Values(value).addName(name)
              } else { 
                neverHold = true
                Values.empty
              }
            case nme.NE => 
              if(this.exists(_ == value)) {
                val out = this.dropValue(value) 
                if(out.isEmpty) neverHold = true
                out
              } else {
                alwaysHold = true
                this
              }
            case nme.GT => new Values(
                ranges.flatMap { case (low, high) => if(low > value) Some((low, high)) else if(high > value) Some((value+1, high)) else None },
                values.filter { _ > value },
                this.name)
            case nme.GE => new Values(
                ranges.flatMap { case (low, high) => if(low >= value) Some((low, high)) else if(high >= value) Some((value, high)) else None },
                values.filter { _ >= value },
                this.name)
            case nme.LT => new Values(
                ranges.flatMap { case (low, high) => if(high < value) Some((low, high)) else if(low < value) Some((low, value-1)) else None },
                values.filter { _ < value },
                this.name)
            case nme.LE => new Values(
                ranges.flatMap { case (low, high) => if(high <= value) Some((low, high)) else if(low <= value) Some((low, value)) else None },
                values.filter { _ <= value },
                this.name)
            case a => 
              //println("applyCond: "+showRaw( a ));
              Values.empty
          }
          
          if(Set[Name](nme.GT, nme.GE, nme.LT, nme.LE) contains op) {
            if(out.isEmpty) neverHold = true
            if(out.size == this.size) alwaysHold = true
          }
          
          out          
        case Apply(Select(expr1, op), List(expr2)) =>
          val (left, right) = (computeExpr(expr1), computeExpr(expr2))
          var out = Values.empty.addActualSize(this.actualSize)
          op match {
            case nme.EQ | nme.NE =>
              if(left.isValue && right.isValue && left.getValue == right.getValue) {
                if(op == nme.EQ) alwaysHold = true else neverHold = true
                if(op == nme.EQ && (left.name == this.name || right.name == this.name)) out = this
              } else if(left.nonEmpty && right.nonEmpty && (left.min > right.max || right.min > left.max)) {
                if(op != nme.EQ) alwaysHold = true else neverHold = true
                if(op != nme.EQ && (left.name == this.name || right.name == this.name)) out = this
              } else if(right.name == this.name && left.isValueForce && op == nme.EQ) { //Yoda conditions 
                out = Values(left.getValueForce).addName(this.name)
              }

            case nme.GT | nme.LE => 
              if(left.isValue && right.isValue) {
                if(left.getValue > right.getValue) {
                  if(op == nme.GT) alwaysHold = true else neverHold = true
                  if(op == nme.GT && (left.name == this.name || right.name == this.name)) out = this
                } else {
                  if(op != nme.GT) alwaysHold = true else neverHold = true
                  if(op != nme.GT && (left.name == this.name || right.name == this.name)) out = this
                }
              } else if(left.nonEmpty && right.nonEmpty) {
                if(left.max <= right.min) {
                  if(op != nme.GT) alwaysHold = true else neverHold = true
                  if(op != nme.GT && (left.name == this.name || right.name == this.name)) out = this
                } else if(left.min > right.max) {
                  if(op == nme.GT) alwaysHold = true else neverHold = true
                  if(op == nme.GT && (left.name == this.name || right.name == this.name)) out = this
                }
              }
              
            case nme.GE | nme.LT => 
              if(left.isValue && right.isValue) {
                if(left.getValue >= right.getValue) {
                  if(op == nme.GE) alwaysHold = true else neverHold = true
                  if(op == nme.GE && (left.name == this.name || right.name == this.name)) out = this
                } else {
                  if(op != nme.GE) alwaysHold = true else neverHold = true
                  if(op != nme.GE && (left.name == this.name || right.name == this.name)) out = this
                }
              } else if(left.nonEmpty && right.nonEmpty) {
                if(left.max < right.min) {
                  if(op != nme.GE) alwaysHold = true else neverHold = true
                  if(op != nme.GE && (left.name == this.name || right.name == this.name)) out = this
                } else if(left.min >= right.max) {
                  if(op == nme.GE) alwaysHold = true else neverHold = true
                  if(op == nme.GE && (left.name == this.name || right.name == this.name)) out = this
                }
              }
              
            case _ =>
          }
          
          out
        case Select(expr, op) =>
          computeExpr(expr).applyUnary(op)
          Values.empty.addActualSize(this.actualSize)
          
        case expr =>
          //println("expr: "+showRaw(expr))
          computeExpr(expr)
          Values.empty.addActualSize(this.actualSize)
      }
      
      if(neverHold) unit.warning(condExpr.pos, "This condition will never hold.")
      if(alwaysHold) unit.warning(condExpr.pos, "This condition will always hold.")
      
      if(!isUsed(condExpr, this.name)) (this, this) else (out, if(neverHold) this else if(alwaysHold) Values.empty else this - out)
    }
    def -(v: Values) = { 
      if(this.isEmpty || v.isEmpty) {
        Values.empty
      } else {
        var out = this
        v map { a => out = out.dropValue(a); a }
        out
      }
    }
    
    def applyUnary(op: Name): Values = op match { 
      case nme.UNARY_+ => this
      case nme.UNARY_- => this.map(a => -a)
      case nme.UNARY_~ => this.map(a => ~a, rangeSafe = false)
      case signum if signum.toString == "signum" => this.map(a => math.signum(a))
      case abs if abs.toString == "abs" =>
        new Values(
          ranges
            .map { case (low, high) => (if(low > 0) low else 0, math.max(math.abs(low), math.abs(high))) }
            .map { case (low, high) if low > high => (high, low); case (low, high) => (low, high) },
          values.map(a => math.abs(a)))

      case size if (size.toString matches "size|length") && (this.actualSize != -1) => Values(this.actualSize)
      case head_last if (head_last.toString matches "head|last") => 
        //Only works for one element :)
        if(this.actualSize == 0) unit.warning(treePosHolder.pos, "Taking the "+head_last.toString+" of an empty collection.")
        if(this.actualSize == 1 && this.size == 1) Values(this.getValueForce) else Values.empty
      case tail_init if (tail_init.toString matches "tail|init") && this.actualSize != -1 => 
        if(this.actualSize == 0) {
          unit.warning(treePosHolder.pos, "Taking the "+tail_init.toString+" of an empty collection.")
          Values.empty
        } else {
          Values.empty.addActualSize(this.actualSize - 1)
        }
      
      case to if (to.toString matches "toIndexedSeq|toList|toSeq|toVector") => this //only immutable
      case distinct if (distinct.toString == "distinct") && this.actualSize != -1 => this.toValues.addActualSize(this.size)
      case id if (id.toString matches "reverse") => this //Will hold, while Set is used for values
      case max if (max.toString == "max") && this.nonEmpty => Values(this.max)
      case min if (min.toString == "min") && this.nonEmpty => Values(this.min)
      case sum if (sum.toString == "sum") && this.nonEmpty && this.toValues.size == this.actualSize => Values(this.toValues.values.sum)

      case empty if (empty.toString == "isEmpty") && (this.actualSize != -1) => 
        unit.warning(treePosHolder.pos, "This condition will " + (if(this.actualSize == 0) "always" else "never") + " hold.")
        Values.empty
      case empty if (empty.toString == "nonEmpty") && (this.actualSize != -1) => 
        unit.warning(treePosHolder.pos, "This condition will " + (if(this.actualSize > 0) "always" else "never") + " hold.")
        Values.empty

      case a => 
        //val raw = showRaw( treePosHolder );
        //println("applyUnary: op:"+op+" thisval:"+this+" tree:"+treePosHolder.toString+"\n"+raw);
        Values.empty
    }
    def apply(op: Name)(right: Values): Values = {
      val left = this

      type F = ((Int, Int) => Int, Boolean) // 2.9 typer fails at pattern matching :)

      val (func, isRangeSafe): F = op match {
          case nme.ADD => (_ + _, true): F
          case nme.SUB => (_ - _, true): F
          case nme.MUL => (_ * _, false): F //ADD: are all these really unsafe for ranges (where you only process first and last)
          case nme.AND => (_ & _, false): F
          case nme.OR  => (_ | _, false): F
          case nme.XOR => (_ ^ _, false): F
          case nme.LSL => (_ << _, false): F
          case nme.LSR => (_ >>> _, false): F
          case nme.ASR => (_ >> _, false): F
          case nme.MOD if right.size == 1 && right.getValueForce != 0 => (_ % _, false): F
          case nme.DIV if right.size == 1 && right.getValueForce != 0 => (_ / _, false): F
          case a if a.toString matches "apply|take|drop|map|max|min|contains" => ((a: Int, b: Int) => throw new Exception(), false): F //Foo, check below
          case _ => return Values.empty
      }
      
      if(left.isEmpty || right.isEmpty) {
        //ADD: x & 2^n is Set(2^n,0) and stuff like that :)
        if(left.contains(0) || right.contains(0)) {
          if(Set[Name](nme.MUL, nme.AND) contains op) Values(0) else Values.empty
        } else {
          Values.empty
        }
      } else if(op.toString == "apply") {
        //TODO: if you wanted actual values, you need to save seq type and refactor values from Set to Seq
        if(left.isSeq && left.actualSize == 1 && left.size == 1 && right.isValueForce && right.getValueForce == 0) Values(left.getValueForce) else left
      } else if(op.toString == "map") {
        right
      } else if(op.toString == "contains") {
        if(right.isValue) {
          if(left.contains(right.getValue)) {
            unit.warning(treePosHolder.pos, "This contains will always return true")
          } else {
            unit.warning(treePosHolder.pos, "This contains will never return true")
          }
        }
        Values.empty
      } else if(op.toString == "max") {
        if(left.isValue && right.isValue) { 
          Values(math.max(left.getValue, right.getValue))
        } else if(left.isValue && !right.isValue) {
          if(left.getValue >= right.max) {
            unit.warning(treePosHolder.pos, "This max will always return the first value")
            Values(left.getValue)
          } else if(left.getValue <= right.min) {
            unit.warning(treePosHolder.pos, "This max will always return the second value")
            right
          } else {
            Values.empty
          }
        } else if(!left.isValue && right.isValue) {
          if(right.getValue >= left.max) {
            unit.warning(treePosHolder.pos, "This max will always return the second value")
            Values(right.getValue)
          } else if(right.getValue <= left.min) {
            unit.warning(treePosHolder.pos, "This max will always return the first value")
            left
          } else {
            Values.empty
          }
        } else {
          Values.empty
        }          
      } else if(op.toString == "min") {
        if(left.isValue && right.isValue) {
          Values(math.min(left.getValue, right.getValue))
        } else if(left.isValue && !right.isValue) {
          if(left.getValue <= right.min) {
            unit.warning(treePosHolder.pos, "This min will always return the first value")
            Values(left.getValue)
          } else if(left.getValue > right.max) {
            unit.warning(treePosHolder.pos, "This min will always return the second value")
            right
          } else {
            Values.empty
          }
        } else if(!left.isValue && right.isValue) {
          if(right.getValue <= left.min) {
            unit.warning(treePosHolder.pos, "This min will always return the second value")
            Values(right.getValue)
          } else if(right.getValue > left.max) {
            unit.warning(treePosHolder.pos, "This min will always return the first value")
            left
          } else {
            Values.empty
          }
        } else {
          Values.empty
        }          
      } else if(op.toString == "take") {
        if(left.isSeq && left.actualSize != -1 && right.isValue) {
          if(right.getValueForce >= left.actualSize) {
            unit.warning(treePosHolder.pos, "This take is always unnecessary.")
            this 
          } else {
            if(right.getValueForce <= 0) unit.warning(treePosHolder.pos, "This collection will always be empty.")
            Values.empty.addName(name).addActualSize(math.max(0, right.getValueForce))
          }
        } else {
          Values.empty
        }
      } else if(op.toString == "drop") {
        if(left.isSeq && left.actualSize != -1 && right.isValue) {
          if(right.getValueForce <= 0) {
            unit.warning(treePosHolder.pos, "This drop is always unnecessary.")
            this
          } else {
            if(left.actualSize-right.getValueForce <= 0) unit.warning(treePosHolder.pos, "This collection will always be empty.")
            Values.empty.addName(name).addActualSize(math.max(0, left.actualSize-right.getValueForce))
          }
        } else { 
          Values.empty
        }
      } else if(left.isValue && right.isValue) {
        if(left.getValue == right.getValue && op == nme.SUB && !left.name.isEmpty) unit.warning(treePosHolder.pos, "Same expression on both sides of subtraction.")
        Values(func(left.getValue, right.getValue))
      } else if(!left.isValue && right.isValue) {
        left.map(a => func(a, right.getValue), rangeSafe = isRangeSafe)
      } else if(left.isValue && !right.isValue) {
        right.map(a => func(left.getValue, a), rangeSafe = isRangeSafe) 
      } else {
        //ADD: join ranges, but be afraid of the explosion :)
        if(left eq right) {
          op match {
            //case nme.ADD => left.map(a => a+a) //WRONG
            //case nme.MUL => left.map(a => a*a)
            case nme.DIV if(!left.contains(0)) => Values(1) //TODO: never gets executed?
            case nme.SUB | nme.XOR => 
              if(op == nme.SUB) unit.warning(treePosHolder.pos, "Same expression on both sides of subtraction.")
              Values(0)
            case nme.AND | nme.OR  => left
            case _ => Values.empty
          }
        } else {
          Values.empty
        }
      }
    }
    
    //approximate
    def size: Int = values.size + ranges.foldLeft(0)((acc, range) => acc + (range._2 - range._1) + 1)
    
    override def toString: String = "Values("+(if(name.size > 0) name+")(" else "")+(values.map(_.toString) ++ ranges.map(a => a._1+"-"+a._2)).mkString(",")+", "+isSeq+", "+actualSize+")"
  }
  
  val SeqLikeObject: Symbol = definitions.getModule(newTermName("scala.collection.GenSeq"))
  val SeqLikeClass: Symbol = definitions.getClass(newTermName("scala.collection.SeqLike"))
  val SeqLikeContains: Symbol = SeqLikeClass.info.member(newTermName("contains"))
  val SeqLikeApply: Symbol = SeqLikeClass.info.member(newTermName("apply"))
  val SeqLikeGenApply: Symbol = SeqLikeObject.info.member(newTermName("apply"))
  def methodImplements(method: Symbol, target: Symbol): Boolean = {
    method == target || method.allOverriddenSymbols.contains(target)
  }
  
  //TODO: extend with more functions... and TEST TEST TEST TEST
  def computeExpr(tree: Tree): Values = {
    treePosHolder = tree
    val out: Values = tree match {
      case Literal(Constant(value: Int)) => Values(value)
      //TODO: I don't think this ones work at all...
      case Select(This(typeT), termName) =>
        val name = termName.toString
        val n = (if(name.contains(".this.")) name.substring(name.lastIndexOf(".")+1) else name).trim
        vals(n)
      case Ident(termName) => 
        val name = termName.toString
        val n = (if(name.contains(".this.")) name.substring(name.lastIndexOf(".")+1) else name).trim
        //println(n+": "+vals(n))
        vals(n)
        
      // String size
      /*case Apply(Select(Ident(id), length), List()) if stringVals.exists(_.name == Some(id.toString)) && length.toString == "length" =>
        val exactValue = stringVals.find(_.name == Some(id.toString)).map(_.exactValue)
        exactValue.map(v => if(v.isDefined) Values(v.get.size) else Values.empty).getOrElse(Values.empty)
        
      case Select(Apply(Select(predef, augmentString), List(Ident(id))), size)
        if stringVals.exists(_.name == Some(id.toString)) && predef.toString == "scala.this.Predef" && augmentString.toString == "augmentString" && size.toString == "size" => 
        val exactValue = stringVals.find(_.name == Some(id.toString)).map(_.exactValue)
        exactValue.map(v => Values(v.size)).getOrElse(Values.empty)
*/
      /// String stuff (TODO: there's a copy up at applyCond)
      case strFun @ Apply(Select(string, func), params) if string.tpe <:< definitions.StringClass.tpe =>
        StringAttrs.stringFunc(string, func, params).right.getOrElse(Values.empty)
        
      case strFun @ Select(Apply(scala_augmentString, List(string)), func)
        if (scala_augmentString.toString endsWith "augmentString") =>
        
        StringAttrs.stringFunc(string, func).right.getOrElse(Values.empty)

      /// Division by zero
      case pos @ Apply(Select(_, op), List(expr)) if (op == nme.DIV || op == nme.MOD) && (computeExpr(expr).contains(0)) => 
        unit.warning(pos.pos, "You will likely divide by zero here.")
        Values.empty

      // Range
      case Apply(Select(Apply(scala_Predef_intWrapper, List(Literal(Constant(low: Int)))), to_until), List(Literal(Constant(high: Int)))) 
        if (scala_Predef_intWrapper.toString endsWith "Wrapper") && (to_until.toString matches "to|until") =>
        
        val high2 = if(to_until.toString == "to") high else high-1
        new Values(Set((low, high2)), Set(), "", isSeq = true, high2-low)

      case Apply(Select(Apply(scala_Predef_intWrapper, List(expr1)), to_until), List(expr2))
        if (scala_Predef_intWrapper.toString endsWith "Wrapper") => 
        
        if(to_until.toString == "to") {
          (expr1, expr2) match {
            case (Literal(Constant(a)), Apply(Select(expr, nme.SUB), List(Literal(Constant(1))))) => 
              if(expr match {
                case Ident(id) => true
                case Select(Ident(id), size) if size.toString matches "size|length" => true //size value
                case Apply(Select(Ident(id), size), List()) if size.toString matches "size|length" => true //size getter
                case Select(Apply(implicitWrapper, List(Ident(id))), size) if size.toString matches "size|length" => true//wrapped size
                case _ => false
              }) unit.warning(treePosHolder.pos, "Use (low until high) instead of (low to high-1)")
            case _ =>
          }
        }
        
        if((to_until.toString matches "to|until") && computeExpr(expr1).isValue && computeExpr(expr2).isValue) {
          val (low, high) = (computeExpr(expr1).getValue, computeExpr(expr2).getValue + (if(to_until.toString == "to") 0 else -1))
          
          new Values(Set((low, high)), Set(), "", isSeq = true, high-low)
        } else {
          Values.empty
        }

      case t @ Apply(TypeApply(genApply @ Select(_,_), _), genVals) if methodImplements(genApply.symbol, SeqLikeGenApply) && (!t.tpe.widen.toString.contains("collection.mutable.")) =>
        val values = genVals.map(v => computeExpr(v))
        if(values.forall(_.isValue)) new Values(Set(), values.map(_.getValue).toSet, "", isSeq = true, actualSize = values.size) else Values.empty.addActualSize(genVals.size)

      //Array isn't immutable, maybe later
      /*case Apply(Select(Select(scala, scala_Array), apply), genVals) if(scala_Array.toString == "Array") =>
        val values = genVals.map(v => computeExpr(v))
        if(values.forall(_.isValue)) new Values(Set(), values.map(_.getValue).toSet, "", isSeq = true, actualSize = values.size) else Values.empty.addActualSize(genVals.size)*/
      
      //TODO: is this for array or what?
      case Select(Apply(arrayOps @ Select(_, intArrayOps), List(expr)), op) if(arrayOps.toString == "scala.this.Predef.intArrayOps") => 
        computeExpr(expr).applyUnary(op)
      
      case Apply(TypeApply(t @ Select(expr, op), _), List(scala_math_Ordering_Int)) if scala_math_Ordering_Int.toString.endsWith("Int") => //.max .min
        computeExpr(t)

      case Apply(Select(scala_math_package, op), params) if scala_math_package.toString == "scala.math.`package`" =>
        op.toString match {
          case "abs" | "signum" if params.size == 1 => computeExpr(params.head).applyUnary(op)
          case "max" | "min"    if params.size == 2 => computeExpr(params(0))(op)(computeExpr(params(1)))
          case _ => Values.empty
        }

      case Apply(Select(scala_util_Random, nextInt), params) if nextInt.toString == "nextInt" => //if scala_util_Random.toString == "scala.util" => or rather the type
        if(params.size == 1) {
          val param = computeExpr(params.head)
          if(param.nonEmpty) {
            if(param.min <= 0) {
              unit.warning(treePosHolder.pos, "The parameter of this nextInt might be lower than 1 here.")
              Values.empty
            } else {
              Values(0, param.max-1)
            }
          } else {
            Values.empty
          }
        } else {
          //ADD: check what happens on +1, also if overflows are worthy of detection elsewhere :)
          //Values(Int.MinValue, Int.MaxValue)
          Values.empty
        }

      case Apply(TypeApply(Select(valName, op), _), List(scala_math_Numeric_IntIsIntegral)) if scala_math_Numeric_IntIsIntegral.toString == "math.this.Numeric.IntIsIntegral" && op.toString == "sum" =>
        computeExpr(valName).applyUnary(op)

      //List(Literal(Constant(1)), Literal(Constant(2)), Literal(Constant(3)), Literal(Constant(4)), Literal(Constant(0)), Literal(Constant(0))))
      case Select(expr, op) =>
        //println((expr, op, computeExpr(expr).applyUnary(op)))
        computeExpr(expr).applyUnary(op)

      case Apply(Select(expr1, op), List(expr2)) =>
        //println("BinaryOp: "+(op, expr1, expr2, (computeExpr(expr1))(op)(computeExpr(expr2))))
        (computeExpr(expr1))(op)(computeExpr(expr2))
      
      case Apply(Apply(TypeApply(Select(valName, map), List(_, _)), List(Function(List(ValDef(mods, paramName, _, EmptyTree)), expr))), _) if(map.toString == "map") => //List(TypeApply(Select(Select(This(newTypeName("immutable")), scala.collection.immutable.List), newTermName("canBuildFrom")), List(TypeTree()))))
        val backupVals = vals.map(a=> a).withDefaultValue(Values.empty)
        val backupStrs = stringVals.clone
        val res = computeExpr(valName)
        vals += paramName.toString -> res
        //println(">        "+vals)
        val out = computeExpr(expr)
        vals = backupVals
        stringVals = backupStrs
        //println(">        "+out)
        out
      
      case If(cond, expr1, expr2) =>
        //TODO: compute cond - if always/never holds, return that branch
        val backupVals = vals.map(a=> a).withDefaultValue(Values.empty)
        val backupStrs = stringVals.clone
        
        val e1 = computeExpr(expr1)
        vals = backupVals
        stringVals = backupStrs

        val e2 = computeExpr(expr2)
        vals = backupVals
        stringVals = backupStrs

        if(e1.isValue && e2.isValue) new Values(values = e1.values ++ e2.values) else Values.empty

      case a => 
        //val raw = showRaw( a ); if(!exprs.contains(raw) && raw.size < 700 && raw.size > "EmptyTree".size)println("computeExpr: "+treePosHolder.toString+"\n"+raw); exprs += raw
        //for(Ident(id) <- a) if(stringVals.exists(_.name == Some(id.toString))) {println("id: "+id+"  "+showRaw( a )); }
        //println("computeExpr: "+showRaw( a ))
        Values.empty
    }
    //println(out+"  "+showRaw(tree))
    out
  }
  //val exprs = collection.mutable.HashSet[String]()
      
  // go immutable
  var vals = collection.mutable.HashMap[String, Values]().withDefaultValue(Values.empty)
  var stringVals = collection.mutable.HashSet[StringAttrs]()
  var treePosHolder: GTree = null //ugly hack to get position for a few warnings
  
  def forLoop(tree: GTree) {
    treePosHolder = tree
    //TODO: actually anything that takes (A <: (a Number) => _), this is awful
    val funcs = "foreach|map|filter(Not)?|exists|find|flatMap|forall|groupBy|count|((drop|take)While)|(min|max)By|partition|span"

    val backupVals = vals.map(a=> a).withDefaultValue(Values.empty)
    val backupStrs = stringVals.clone
    
    val (param, values, body, func) = tree match {
      case Apply(TypeApply(Select(collection, func), _), List(Function(List(ValDef(_, param, _, _)), body))) if (func.toString matches funcs) =>
        //println(showRaw(collection))
        val values = computeExpr(collection).addName(param.toString)
        //println(values)
        //if(values.isEmpty) {
            //println("not a collection I know("+tree.pos+"): "+showRaw(collection))
            //return
        //}
        
        (param.toString, values, body, func.toString)
      case _ => 
        //println("not a loop("+tree.pos+"): "+showRaw(tree))
        return
    }
    
    if(values.nonEmpty) {
      vals += param -> values
      
      if(!isUsed(body, param) && func != "foreach") unit.warning(tree.pos, "Iterator value is not used in the body.")

      traverseBlock(body)
    }
    vals = backupVals.withDefaultValue(Values.empty)
    stringVals = backupStrs
  }
  
  val visitedBlocks = collection.mutable.HashSet[GTree]()
  
  // Just something quick I hacked together for an actual bug
  //implicit def String2StringAttrs(s: String) = new StringAttrs(exactValue = Some(s))
  object StringAttrs {
    //TODO: when merging, save known chunks - .contains then partially works
    def empty = new StringAttrs()
    
    /// Tries to execute string functions and return either a String or Int representation
    def stringFunc(string: Tree, func: Name, params: List[Tree] = List[Tree]()): Either[StringAttrs, Values] = {
      val str = StringAttrs(string)
      lazy val intParam = if(params.size == 1 && params.head.tpe <:< definitions.IntClass.tpe) computeExpr(params.head) else Values.empty
      lazy val stringParam = if(params.size == 1 && params.head.tpe <:< definitions.StringClass.tpe) StringAttrs(params.head) else empty

      //println((string, func, params, str, intParam))
      //println(str.exactValue)
      if(str == StringAttrs.empty) {
        Left(empty)
      } else func.toString match {
        case "size"|"length" if params.size == 0 => Right(str.exactValue.map(v => Values(v.size)).getOrElse(Values.empty))
        case "$plus" if params.size == 1 =>
          val param = params.head
          if(intParam.isValue) {
            //println((str, intParam))
            //println(Left(str + new StringAttrs(Some(intParam.getValue.toString))))
            Left(str + new StringAttrs(Some(intParam.getValue.toString)))
          } else {
            Left(str + StringAttrs(param))
          }
        case "$times" if params.size == 1 && computeExpr(params.head).isValue =>
          Left(str * computeExpr(params.head).getValue)

        case "init"       if str.exactValue.isDefined => Left(new StringAttrs(str.exactValue.map(_.init)))
        case "tail"       if str.exactValue.isDefined => Left(new StringAttrs(str.exactValue.map(_.tail)))
        case "capitalize" if str.exactValue.isDefined => Left(new StringAttrs(str.exactValue.map(_.capitalize)))
        case "distinct"   if str.exactValue.isDefined => Left(new StringAttrs(str.exactValue.map(_.distinct)))
        case "reverse"    if str.exactValue.isDefined => Left(new StringAttrs(str.exactValue.map(_.reverse)))
        
        //These come in (Char/String) versions
        case "stringPrefix" if params.size == 0 && str.exactValue.isDefined => Left(new StringAttrs(str.exactValue.map(_.stringPrefix)))
        case "stripLineEnd" if params.size == 0 && str.exactValue.isDefined => Left(new StringAttrs(str.exactValue.map(_.stripLineEnd)))
        case "stripMargin"  if params.size == 0 && str.exactValue.isDefined => Left(new StringAttrs(str.exactValue.map(_.stripMargin)))
        
        case "toUpperCase" => Left(new StringAttrs(str.exactValue.map(_.toUpperCase), None, str.minLength, str.trimmedMinLength))
        case "toLowerCase" => Left(new StringAttrs(str.exactValue.map(_.toLowerCase), None, str.minLength, str.trimmedMinLength))
        case "trim" => 
          val newExactValue = str.exactValue.map(_.trim)
          val newLength = str.exactValue.map(_.trim.size).getOrElse(str.getTrimmedMinLength)
          Left(new StringAttrs(newExactValue, None, newLength, newLength))
        case "nonEmpty"|"isEmpty" => 
          if(str.alwaysNonEmpty) unit.warning(treePosHolder.pos, "This string will never be empty.")
          if(str.exactValue.exists(_.isEmpty)) unit.warning(treePosHolder.pos, "This string will always be empty.")
          Left(empty)
        case "toInt" if str.exactValue.isDefined =>
          try {
            Right(Values(str.exactValue.get.toInt))
          } catch {
            case e: Exception =>
              unit.warning(treePosHolder.pos, "This String toInt conversion will likely fail.")
              Left(empty)
          }
        //str.func(Int)
        case f @ ("charAt"|"codePointAt"|"codePointBefore"|"substring"
                 |"apply"|"drop"|"take"|"dropRight"|"takeRight") if params.size == 1 && computeExpr(params.head).isValue =>
          val param = computeExpr(params.head).getValue
          lazy val string = str.exactValue.get //lazy to avoid None.get... didn't use monadic, because I was lazy

          //println((string, param))
          
          //TODO use reflection, dummy :)
          try {
            f match {
              case "charAt"|"apply"  => if(str.exactValue.isDefined) { string.charAt(param); Left(empty) } else if(param < 0) throw new IndexOutOfBoundsException else Left(empty)
              case "codePointAt"     => if(str.exactValue.isDefined) Right(Values(string.codePointAt(param))) else if(param < 0) throw new IndexOutOfBoundsException else Left(empty)
              case "codePointBefore" => if(str.exactValue.isDefined) Right(Values(string.codePointBefore(param))) else if(param < 1) throw new IndexOutOfBoundsException else Left(empty)
              case "substring"       => if(str.exactValue.isDefined) Left(new StringAttrs(Some(string.substring(param)))) else if(param < 0) throw new IndexOutOfBoundsException else Left(empty)
              case "drop"            => if(str.exactValue.isDefined) Left(new StringAttrs(Some(string.drop(param)))) else Left(empty)
              case "take"            => if(str.exactValue.isDefined) Left(new StringAttrs(Some(string.take(param)))) else Left(empty)
              case "dropRight"       => if(str.exactValue.isDefined) Left(new StringAttrs(Some(string.dropRight(param)))) else Left(empty)
              case "takeRight"       => if(str.exactValue.isDefined) Left(new StringAttrs(Some(string.takeRight(param)))) else Left(empty)
              case a => Left(empty)
            }
          } catch {
            case e: IndexOutOfBoundsException =>
              unit.warning(params.head.pos, "This index will likely cause an IndexOutOfBoundsException.")
              Left(empty)
            case e: Exception =>
              Left(empty)
          }
          
        //str.func(String)
        case f @ ("contains"|"startsWith"|"endsWith") if str.exactValue.isDefined && params.size == 1 && stringParam.exactValue.isDefined =>
          val (string, param) = (str.exactValue.get, stringParam.exactValue.get)
          
          f match {
            case "contains"   => unit.warning(params.head.pos, "This contains will always return "+string.contains(param)+".")
            case "startsWith" => unit.warning(params.head.pos, "This startsWith will always return "+string.startsWith(param)+".")
            case "endsWith"   => unit.warning(params.head.pos, "This endsWith will always return "+string.endsWith(param)+".")
            case _ =>
          }
          Left(empty)
          
        case _ =>
          Left(empty)
      }
    }

    
    def apply(tree: Tree): StringAttrs = {
      def traverse(tree: Tree): StringAttrs = tree match {
        case Literal(Constant(null)) => new StringAttrs(exactValue = Some("null"))
        case Literal(Constant(c)) => 
          if(stringVals.exists(_.exactValue == Some(c.toString))) {
            unit.warning(tree.pos, "You have defined that string as a val already, maybe use that?")
          }

          new StringAttrs(exactValue = Some(c.toString))
          
        case Ident(name) => stringVals.find(_.name.exists(_ == name.toString)).getOrElse(empty)
        case If(cond, expr1, expr2) =>
          val (e1, e2) = (traverse(expr1), traverse(expr2))
          
          new StringAttrs(minLength = math.min(e1.getMinLength, e2.getMinLength), trimmedMinLength = math.min(e1.getTrimmedMinLength, e2.getTrimmedMinLength))

        case Apply(augmentString, List(expr)) if(augmentString.toString == "scala.this.Predef.augmentString") =>
          StringAttrs(expr)
          
        /// Pass on functions on strings
        //TODO: maybe check if some return string and can be computed
        case Apply(Select(str, func), params) => 
          stringFunc(str, func, params).left.getOrElse(empty)

        case Select(Apply(scala_augmentString, List(string)), func) if (scala_augmentString.toString endsWith "augmentString") =>
          stringFunc(string, func).left.getOrElse(empty)
            
        case a => 
          //println(showRaw(a))
          empty
      }
      
      val a = traverse(tree)
      //println("tree: "+ a)
      a
    }
  }
  class StringAttrs(
      val exactValue: Option[String] = None,
      val name: Option[String] = None,
      val minLength: Int = 0,
      val trimmedMinLength: Int = 0) {
    
    def addName(name: String): StringAttrs = new StringAttrs(exactValue, Some(name), minLength, trimmedMinLength)
    
    //println(this)

    def alwaysNonEmpty = getMinLength > 0
    def getMinLength = math.max(minLength, exactValue.getOrElse("").length)
    def getTrimmedMinLength = math.max(trimmedMinLength, exactValue.getOrElse("").trim.length)
    
    def +(s: String) = 
      new StringAttrs(
        exactValue = if(this.exactValue.isDefined) Some(this.exactValue.get + s) else None,
        minLength = this.getMinLength + s.length,
        trimmedMinLength = this.getTrimmedMinLength + s.trim.length //ADD: can be made more exact
      )
    def +(s: StringAttrs) = 
      new StringAttrs(
        exactValue = if(this.exactValue.isDefined && s.exactValue.isDefined) Some(this.exactValue.get + s.exactValue.get) else None,
        minLength = this.getMinLength + s.getMinLength,
        trimmedMinLength = this.getTrimmedMinLength + s.getTrimmedMinLength //ADD: can be made more exact
      )
    def *(n: Int) = 
      new StringAttrs(
        exactValue = if(this.exactValue.isDefined) Some(this.exactValue.get*n) else None,
        minLength = this.getMinLength*n,
        trimmedMinLength = this.getTrimmedMinLength*n //ADD: can be made more exact
      )
    
    override def hashCode: Int = /*if(exactValue.isDefined) exactValue.hashCode else */exactValue.hashCode + name.hashCode + minLength + trimmedMinLength
    override def equals(that: Any): Boolean = that match {
      case s: StringAttrs => (this.exactValue.isDefined && s.exactValue.isDefined && this.exactValue.get == s.exactValue.get)
      case s: String => exactValue.exists(_ == s)
      case _ => false
    }
    
    override def toString = (exactValue, name, getMinLength, getTrimmedMinLength).toString
  }
 
  def traverseBlock(tree: GTree) {
    val backupVals = vals.map(a=> a).withDefaultValue(Values.empty)
    val backupStrs = stringVals.clone
    
    traverse(tree)
    
    vals = backupVals.withDefaultValue(Values.empty)
    stringVals = backupStrs
  }
  def traverse(tree: GTree) {
    if(visitedBlocks(tree)) return else visitedBlocks += tree
    treePosHolder = tree
    tree match {
      case forloop @ Apply(TypeApply(Select(collection, foreach_map), _), List(Function(List(ValDef(_, _, _, _)), _))) =>
        forLoop(forloop)
      
      /// Assertions checks
      case Apply(Select(scala_Predef, assert), List(condExpr)) 
        if (scala_Predef.tpe <:< definitions.PredefModule.tpe) && (assert.toString matches "assert|assume|require") => 
        
        // we can apply these conditions to vals - if they don't hold, it'll throw an exception anyway
        // and they'll reset at the end of the current block
        vals = vals.map(a => (a._1, a._2.applyCond(condExpr)._1)).withDefaultValue(Values.empty)
      
      /// String checks
      case s @ Literal(Constant(str: String)) if stringVals.find(_.exactValue == Some(str)).isDefined =>
        unit.warning(s.pos, "You have defined that string as a val already, maybe use that?")
        visitedBlocks += s

      case ValDef(m: Modifiers, valName, _, s @ Literal(Constant(str: String))) if(!m.hasFlag(MUTABLE) && !m.hasFlag(FINAL)) =>
        if(stringVals.exists(_.exactValue == Some(str))) unit.warning(s.pos, "You have defined that string as a val already, maybe use that?")
        //stringVals += str
        visitedBlocks += s
        
        val str2 = StringAttrs(s).addName(valName.toString)
        //println("str2: "+str2)
        if(str2.exactValue.isDefined || str2.getMinLength > 0) {
          stringVals += str2
        }
        //println("stringVals2: "+stringVals)

      case ValDef(m: Modifiers, valName, _, Literal(Constant(a: Int))) if(!m.hasFlag(MUTABLE)) =>
        val valNameStr = valName.toString.trim
        vals += valNameStr -> Values(a, valNameStr)
        //println(vals(valName.toString))

      case v@ValDef(m: Modifiers, valName, _, expr) if !m.hasFlag(MUTABLE) /*&& !m.hasFlag(LAZY)) && !computeExpr(expr).isEmpty*/ => //&& computeExpr(expr).isValue =>
        //ADD: aliasing... val a = i, where i is an iterator, then 1/i-a is divbyzero
        //ADD: isSeq and actualSize

        if(expr.tpe.toString == "String") {
          val str = StringAttrs(expr).addName(valName.toString)
          //println("str1: "+str)
          if(str.exactValue.isDefined || str.getMinLength > 0) {
            stringVals += str
            visitedBlocks += expr
          }
          //println("stringVals1: "+stringVals)
        }

        val valNameStr = valName.toString
        val res = computeExpr(expr).addName(valNameStr)
        vals += valNameStr -> res
       
        //println("newVal: "+computeExpr(expr).addName(valNameStr))
        //println("newVal: "+vals(valName.toString))
        //println(showRaw(expr))
        
        expr match {
          case e => //Block(_, _) | If(_,_,_) =>
            //println(expr)
            val backupVals = vals.map(a=> a).withDefaultValue(Values.empty)
            val backupStrs = stringVals.clone
            traverse(expr)
            vals = backupVals.withDefaultValue(Values.empty)
            stringVals = backupStrs
          //case _ =>
        }
      
      case Match(pat, cases) if pat.tpe.toString != "Any @unchecked" && cases.size >= 2 =>
        for(c <- cases) {
          val backupVals = vals.map(a=> a).withDefaultValue(Values.empty)
          val backupStrs = stringVals.clone
          //TODO: c.pat can override some variables
          traverse(c.body)
          vals = backupVals.withDefaultValue(Values.empty)
          stringVals = backupStrs
        }
      
      case If(condExpr, t, f) => 
        val backupVals = vals.map(a=> a).withDefaultValue(Values.empty)
        val backupStrs = stringVals.clone
        
        //println(vals)
        vals = backupVals.map(a => (a._1, a._2.applyCond(condExpr)._1)).withDefaultValue(Values.empty)
        //println(vals)
        //ADD: if always true, or always false pass the return value, e.g. val a = 1; val b = if(a == 1) 5 else 4
        t.foreach(traverse)

        vals = backupVals.withDefaultValue(Values.empty)
        vals = backupVals.map(a => (a._1, a._2.applyCond(condExpr)._2)).withDefaultValue(Values.empty)
        f.foreach(traverse)
        
        vals = backupVals.withDefaultValue(Values.empty)
        stringVals = backupStrs
        //println(vals)

      //case pos @ Apply(Select(Ident(seq), apply), List(indexExpr)) 
      case pos @ Apply(Select(seq, apply), List(indexExpr)) if methodImplements(pos.symbol, SeqLikeApply) =>
        //println(seq.toString)
        //println("indexExpr: "+computeExpr(indexExpr))
        //println(showRaw(indexExpr))
        if(vals.contains(seq.toString) && vals(seq.toString).actualSize != -1 && (computeExpr(indexExpr).exists { a => a >= vals(seq.toString).actualSize })) {
          unit.warning(pos.pos, "You will likely use a too large index for a collection here.")
        }
        if(computeExpr(indexExpr).exists { a => a < 0 }) {
          unit.warning(pos.pos, "You will likely use a negative index for a collection here.")
        }
      
      case DefDef(_, _, _, _, _, block) => 
        val backupVals = vals.map(a=> a).withDefaultValue(Values.empty)
        val backupStrs = stringVals.clone
        traverse(block)
        block match {
          //case Block(_, pos @ Literal(Constant(returnVal))) => 
            //unit.warning(pos.pos, "This function returns a constant.")
          case Block(_, pos @ Ident(returnVal)) if vals(returnVal.toString).size == 1 && !vals(returnVal.toString).isSeq => 
            unit.warning(pos.pos, "This function always returns the same value.")
          case _ =>
        }
        vals = backupVals.withDefaultValue(Values.empty)
        stringVals = backupStrs

      /// Invalid regex
      case Apply(java_util_regex_Pattern_compile, List(regExpr)) if java_util_regex_Pattern_compile.toString == "java.util.regex.Pattern.compile" =>
        treePosHolder = regExpr
        StringAttrs(regExpr).exactValue.foreach(checkRegex)
      
      case Apply(Select(str, func), List(regExpr)) if (str.tpe.widen <:< definitions.StringClass.tpe) && (func.toString matches "matches|split") =>
        treePosHolder = regExpr
        StringAttrs(regExpr).exactValue.foreach(checkRegex)
        
      case Apply(Select(str, func), List(regExpr, str2)) if (str.tpe.widen <:< definitions.StringClass.tpe) && (func.toString matches "replace(All|First)") =>
        treePosHolder = regExpr
        StringAttrs(regExpr).exactValue.foreach(checkRegex)

      case Select(Apply(scala_Predef_augmentString, List(regExpr)), r)
        if(scala_Predef_augmentString.toString.endsWith(".augmentString") && r.toString == "r") =>
        treePosHolder = regExpr
                
        StringAttrs(regExpr).exactValue.foreach(checkRegex)

      case b @ Block(_, _) => 
        //println("block: "+b)
        val backupVals = vals.map(a=> a).withDefaultValue(Values.empty)
        val backupStrs = stringVals.clone
        tree.children.foreach(traverse)
        vals = backupVals.withDefaultValue(Values.empty)
        stringVals = backupStrs

      /// Pass on expressions
      case pos @ Apply(Select(_, op), List(expr)) =>
        //if (op == nme.DIV || op == nme.MOD) && (computeExpr(expr).contains(0)) => 
        
        computeExpr(pos)
        tree.children.foreach(traverse)

      case _ => 
        //if(vals.nonEmpty)println("in: "+showRaw(tree))
        //if(vals.nonEmpty)println(">   "+vals);
        //if(showRaw(tree).startsWith("Literal") || showRaw(tree).startsWith("Constant"))println("in: "+showRaw(tree))
        tree.children.foreach(traverse)
    }
  }
}
