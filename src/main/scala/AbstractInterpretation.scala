package com.foursquare.lint

import scala.tools.nsc.{Global}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.symtab.Flags.{IMPLICIT, OVERRIDE, MUTABLE, CASE, LAZY}
import com.foursquare.lint.global._

class AbstractInterpretation(val global: Global, val unit: GUnit) {
  import global._

  def isUsed(t: GTree, name: String): Boolean = {
    var used = 0
    def usedTimes(t: GTree, name: String) { t match { //TODO: there are possible errors, but we're returning empty anyways :)
      case a if a.toString.drop(name.size) matches "[.](size|length|head|last)" => used -= 1 //ADD: other funcs that don't change a thing
      case a if a.toString == name => used += 1
      case a =>
        t.asInstanceOf[Tree].foreach(st => if(st != t) usedTimes(st, name))
    }}
    usedTimes(t, name)
    //println((name, used))
    (used > 0)
  }
    
  object Values {
    lazy val empty = new Values()
    //def apply(low: Int, high: Int, name: String, isSeq: Boolean, actualSize: Int): Values = new Values(name = name, ranges = Set((low, high)), isSeq = isSeq, actualSize = actualSize)
    //def apply(s: Set[Int], name: String, isSeq: Boolean, actualSize: Int): Values = new Values(name = name, values = s, isSeq = isSeq, actualSize = actualSize)
    def apply(i: Int, name: String = ""): Values = new Values(name = name, values = Set(i))
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
    def exists(func: Int => Boolean) = (values exists func) || (ranges exists { case (low, high) => Range(low, high+1) exists func })
    def forall(func: Int => Boolean) = (values forall func) && (ranges forall { case (low, high) => Range(low, high+1) forall func })
    
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
        if(this.ranges.nonEmpty && this.size <= 100001) //ADD: it's not that bad - it simply won't check the big ranges further.
          (new Values(values = values ++ ranges.flatMap { case (low, high) => (low to high) }, name = name, isSeq = isSeq, actualSize = actualSize)).map(func)
        else 
          Values.empty
      }
    
    def applyCond(condExpr: Tree): Values = {//TODO: return (true, false) ValueSets in one go - applyInverseCond sucks //TODO: true and false cound be possible returns for error msgs
      val out = condExpr match {
        case Apply(Select(expr1, op), List(expr2)) if op == nme.ZAND => //&&
          this.applyCond(expr1).applyCond(expr2)
          
        case Apply(Select(expr1, op), List(expr2)) if op == nme.ZOR => //&&
          val (left,right) = (this.applyCond(expr1), this.applyCond(expr2))
          new Values(
            left.ranges ++ right.ranges,
            left.values ++ right.values,
            this.name)

        //ADD: expr op expr that handles idents right
        case Apply(Select(Ident(v), op), List(expr)) if v.toString == this.name && this.nonEmpty && computeExpr(expr).isValue => 
          val value = computeExpr(expr).getValue
          val out = op match {
            case nme.EQ => 
              if(this.exists(_ == value)) {
                if(this.forall(_ == value)) unit.warning(condExpr.pos, "This condition will always hold.")
                Values(value).addName(name) 
              } else { 
                unit.warning(condExpr.pos, "This condition will never hold.")
                Values.empty
              }
            case nme.NE => 
              if(this.exists(_ == value)) {
                val out = this.dropValue(value) 
                if(out.isEmpty) unit.warning(condExpr.pos, "This condition will never hold.")
                out
              } else {
                unit.warning(condExpr.pos, "This condition will always hold.")
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
          
          if(Set(nme.GT, nme.GE, nme.LT, nme.LE)(op)) {
            if(out.isEmpty) unit.warning(condExpr.pos, "This condition will never hold.")
            if(out.size == this.size) unit.warning(condExpr.pos, "This condition will always hold.")
          }
          
          out          
        case Apply(Select(expr1, op), List(expr2)) =>
          val (left, right) = (computeExpr(expr1), computeExpr(expr2))
          var out = Values.empty.addActualSize(this.actualSize)
          op match {
            case nme.EQ | nme.NE =>
              if(left.isValue && right.isValue && left.getValue == right.getValue) {
                unit.warning(condExpr.pos, "This condition will " + (if(op == nme.EQ) "always" else "never") + " hold.")
                if(op == nme.EQ && (left.name == this.name || right.name == this.name)) out = this
              } else if(left.nonEmpty && right.nonEmpty && (left.min > right.max || right.min > left.max)) {
                unit.warning(condExpr.pos, "This condition will " + (if(op == nme.EQ) "never" else "always") + " hold.")
                if(op != nme.EQ && (left.name == this.name || right.name == this.name)) out = this
              } else if(right.name == this.name && left.isValueForce && op == nme.EQ) { //Yoda conditions 
                out = Values(left.getValueForce).addName(this.name)
              }

            case nme.GT | nme.LE => 
              if(left.isValue && right.isValue) {
                if(left.getValue > right.getValue) {
                  unit.warning(condExpr.pos, "This condition will " + (if(op == nme.GT) "always" else "never") + " hold.")
                  if(op == nme.GT && (left.name == this.name || right.name == this.name)) out = this
                } else {
                  unit.warning(condExpr.pos, "This condition will " + (if(op == nme.GT) "never" else "always") + " hold.")
                  if(op != nme.GT && (left.name == this.name || right.name == this.name)) out = this
                }
              } else if(left.nonEmpty && right.nonEmpty) {
                if(left.max <= right.min) {
                  unit.warning(condExpr.pos, "This condition will " + (if(op == nme.GT) "never" else "always") + " hold.")
                  if(op != nme.GT && (left.name == this.name || right.name == this.name)) out = this
                } else if(left.min > right.max) {
                  unit.warning(condExpr.pos, "This condition will " + (if(op == nme.GT) "always" else "never") + " hold.")
                  if(op == nme.GT && (left.name == this.name || right.name == this.name)) out = this
                }
              }
              
            case nme.GE | nme.LT => 
              if(left.isValue && right.isValue) {
                if(left.getValue >= right.getValue) {
                  unit.warning(condExpr.pos, "This condition will " + (if(op == nme.GE) "always" else "never") + " hold.")
                  if(op == nme.GE && (left.name == this.name || right.name == this.name)) out = this
                } else {
                  unit.warning(condExpr.pos, "This condition will " + (if(op == nme.GE) "never" else "always") + " hold.")
                  if(op != nme.GT && (left.name == this.name || right.name == this.name)) out = this
                }
              } else if(left.nonEmpty && right.nonEmpty) {
                if(left.max < right.min) {
                  unit.warning(condExpr.pos, "This condition will " + (if(op == nme.GE) "never" else "always") + " hold.")
                  if(op != nme.GE && (left.name == this.name || right.name == this.name)) out = this
                } else if(left.min >= right.max) {
                  unit.warning(condExpr.pos, "This condition will " + (if(op == nme.GE) "always" else "never") + " hold.")
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
          computeExpr(expr)
          Values.empty.addActualSize(this.actualSize)
      }
      
      if(!isUsed(condExpr, this.name)) this else out
    }
    def applyInverseCond(condExpr: Tree) = { //TODO: wow, really... copy paste?
      if(!isUsed(condExpr, this.name)) this else condExpr match {
        //ADD: grouping with && ||, etc... replace both sides with with Expr (but test - seems weird)
        case Apply(Select(Ident(v), op), List(expr)) if v.toString == this.name && computeExpr(expr).isValue => 
          val value = computeExpr(expr).getValue
          op match { //TODO: warn if some condition can never be true
            case nme.EQ => if(this.exists(a => a == value)) this.dropValue(value) else { Values.empty }
            case nme.NE => if(this.exists(a => a == value)) Values(value) else { Values.empty }
            case nme.LE => new Values(
                ranges.flatMap { case orig @ (low, high) => if(low > value) Some(orig) else if(high > value) Some((value+1, high)) else None },
                values.filter { a => a > value },
                this.name)
            case nme.LT => new Values(
                ranges.flatMap { case orig @ (low, high) => if(low >= value) Some(orig) else if(high >= value) Some((value, high)) else None },
                values.filter { a => a >= value },
                this.name)
            case nme.GE => new Values(
                ranges.flatMap { case orig @ (low, high) => if(high < value) Some(orig) else if(low < value) Some((low, value-1)) else None },
                values.filter { a => a < value },
                this.name)
            case nme.GT => new Values(
                ranges.flatMap { case orig @ (low, high) => if(high <= value) Some(orig) else if(low <= value) Some((low, value)) else None },
                values.filter { a => a <= value },
                this.name)
            case _ => 
              Values.empty
          }
        case _ => Values.empty.addActualSize(this.actualSize)
      }
    }
    
    def applyUnary(op: Name): Values = op match { 
      case nme.UNARY_+ => this
      case nme.UNARY_- => this.map(a => -a)
      case nme.UNARY_~ => this.map(a => ~a, rangeSafe = false)
      case abs if abs.toString == "abs" =>
        new Values(
          ranges
            .map { case (low, high) => (if(low > 0) low else 0, math.max(math.abs(low), math.abs(high))) }
            .map { case (low, high) if low > high => (high, low); case (low, high) => (low, high) },
          values.map(a => math.abs(a)))

      case size if (size.toString == "size" || size.toString == "length") && (this.actualSize != -1) => Values(this.actualSize)
      case head_last if (head_last.toString == "head|last") && this.actualSize == 1 && this.size == 1 => Values(this.getValueForce) //Only works for one element :)
      //case tail_init if (tail_init.toString == "tail|init") && this.actualSize != -1 => Values.empty.addActualSize(this.actualSize - 1) //TODO: doesn't work
      case to if (to.toString matches "toIndexedSeq|toList|toSeq|toVector") => this //only immutable
      case distinct if (distinct.toString == "distinct") && this.actualSize != -1 => this.toValues.addActualSize(this.size) //Will hold, while Set is used for values
      case id if (id.toString matches "reverse") => this //Will hold, while Set is used for values
      case max if (max.toString == "max") && this.nonEmpty => Values(this.max)
      case min if (min.toString == "min") && this.nonEmpty => Values(this.min)

      case empty if (empty.toString == "isEmpty") && (this.actualSize != -1) => 
        unit.warning(treePosHolder.pos, "This will " + (if(this.actualSize == 0) "always" else "never") + " hold.")
        Values.empty
      case empty if (empty.toString == "nonEmpty") && (this.actualSize != -1) => 
        unit.warning(treePosHolder.pos, "This will " + (if(this.actualSize > 0) "always" else "never") + " hold.")
        Values.empty

      case _ => Values.empty
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
          case a if a.toString matches "apply|take|drop|map" => ((a: Int, b: Int) => throw new Exception(), false): F //Foo, check below
          case _ => return Values.empty
      }
      
      if(left.isEmpty || right.isEmpty) {
        //ADD: x & 2^n is Set(2^n,0) and stuff like that :)
        if(left.contains(0) || right.contains(0)) {
          if(Set(nme.MUL, nme.AND).contains(op)) Values(0) else Values.empty
        } else {
          Values.empty
        }
      } else if(op.toString == "apply") {
        //TODO: if you wanted actual values, you need to save seq type and refactor values from Set to Seq
        if(left.isSeq && left.actualSize == 1 && left.size == 1 && right.isValue && right.getValueForce == 0) Values(left.getValueForce) else left
      } else if(op.toString == "map") {
        //println(" here")
        right
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
            case nme.SUB | nme.XOR => Values(0)
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
  
  //ADD: GenSeq, and then implement .head .tail, ...
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
    tree match {
      case Literal(Constant(value: Int)) => Values(value)
      case Select(This(typeT), termName) =>
        val name = termName.toString
        val n = (if(name.contains(".this.")) name.substring(name.lastIndexOf(".")+1) else name).trim
        vals(n)
        
      case Ident(termName) => 
        val name = termName.toString
        val n = (if(name.contains(".this.")) name.substring(name.lastIndexOf(".")+1) else name).trim
        vals(n)
      
      case Apply(Select(Apply(Select(scala_Predef, intWrapper), List(Literal(Constant(low: Int)))), to_until), List(Literal(Constant(high: Int)))) if (to_until.toString matches "to|until") =>
        val high2 = if(to_until.toString == "to") high else high-1
        new Values(Set((low, high2)), Set(), "", isSeq = true, high2-low)

      case Apply(Select(Apply(Select(scala_Predef, intWrapper), List(expr1)), to_until), List(expr2)) if (to_until.toString matches "to|until") && computeExpr(expr1).isValue && computeExpr(expr2).isValue =>
        val (low, high) = (computeExpr(expr1).getValue, computeExpr(expr2).getValue + (if(to_until.toString == "to") 0 else -1))
        new Values(Set((low, high)), Set(), "", isSeq = true, high-low)

      case t @ Apply(TypeApply(genApply @ Select(_,_), _), genVals) if methodImplements(genApply.symbol, SeqLikeGenApply) && (!t.tpe.widen.toString.contains("collection.mutable.")) =>
        //ADD: List(expr,2,3) - save size, even if you can't compute expr
        val values = genVals.map(v => computeExpr(v))
        if(values.forall(_.isValue)) new Values(Set(), values.map(_.getValue).toSet, "", isSeq = true, actualSize = values.size) else Values.empty.addActualSize(genVals.size)

      //Array isn't immutable, maybe later
      /*case Apply(Select(Select(scala, scala_Array), apply), genVals) if(scala_Array.toString == "Array") =>
        val values = genVals.map(v => computeExpr(v))
        if(values.forall(_.isValue)) new Values(Set(), values.map(_.getValue).toSet, "", isSeq = true, actualSize = values.size) else Values.empty.addActualSize(genVals.size)*/
      
      case Select(Apply(arrayOps @ Select(_, intArrayOps), List(expr)), op) if(arrayOps.toString == "scala.this.Predef.intArrayOps") => 
        computeExpr(expr).applyUnary(op)
      
      case Apply(TypeApply(t @ Select(expr, op), _), List(Select(scala_math_Ordering, _Int))) if _Int.toString.endsWith("Int") => //.max .min
        computeExpr(t)

      //List(Literal(Constant(1)), Literal(Constant(2)), Literal(Constant(3)), Literal(Constant(4)), Literal(Constant(0)), Literal(Constant(0))))
      case Select(expr, op) =>
        //println((expr, op, computeExpr(expr).applyUnary(op)))
        computeExpr(expr).applyUnary(op)
      
      case Apply(Select(scala_math_package, abs), List(expr)) if scala_math_package.toString == "scala.math.`package`" && abs.toString == "abs" => //TODO: scala and java abs
        computeExpr(expr).applyUnary(abs)

      case Apply(Select(expr1, op), List(expr2)) =>
        //println((op, expr1, expr2, (computeExpr(expr1))(op)(computeExpr(expr2))))
        (computeExpr(expr1))(op)(computeExpr(expr2))
      
      case Apply(Apply(TypeApply(Select(valName, map), List(_, _)), List(Function(List(ValDef(mods, paramName, _, EmptyTree)), expr))), _) if(map.toString == "map") => //List(TypeApply(Select(Select(This(newTypeName("immutable")), scala.collection.immutable.List), newTermName("canBuildFrom")), List(TypeTree()))))
        val backupVals = vals.map(a=> a).withDefaultValue(Values.empty)
        vals += paramName.toString -> computeExpr(valName)
        //println(">        "+vals)
        val out = computeExpr(expr)
        vals = backupVals
        //println(">        "+out)
        out
      
      case a => 
        //println("computeExpr: "+showRaw( a ));
        Values.empty
    }
  }
      
  var vals = collection.mutable.HashMap[String, Values]().withDefaultValue(Values.empty)
  var treePosHolder: GTree = null //ugly hack to get position for a few warnings
  
  def forLoop(tree: GTree) {
    treePosHolder = tree
    //TODO: actually anything that takes (A <: (a Number) => _), this is awful
    val funcs = "foreach|map|filter(Not)?|exists|find|flatMap|forall|groupBy|count|((drop|take)While)|(min|max)By|partition|span"

    val backupVals = vals.map(a=> a).withDefaultValue(Values.empty)
    
    val (param, values, body) = tree match {
      case Apply(TypeApply(Select(collection, foreach_map), _), List(Function(List(ValDef(_, param, _, _)), body))) if (foreach_map.toString matches funcs) =>
        val values = computeExpr(collection).addName(param.toString)
        //if(values.isEmpty) {
            //println("not a collection I know("+tree.pos+"): "+showRaw(collection))
            //return
        //}
        
        (param, values, body)
      case _ => 
        //println("not a loop("+tree.pos+"): "+showRaw(tree))
        return
    }
    
    if(values.nonEmpty) {
      vals += param.toString -> values

      traverseBlock(body)
    }
    vals = backupVals.withDefaultValue(Values.empty)
  }
  
  val visitedBlocks = collection.mutable.HashSet[GTree]()
 
  def traverseBlock(tree: GTree) {
    val backupVals = vals.map(a=> a).withDefaultValue(Values.empty)
    
    traverse(tree)
    
    vals = backupVals.withDefaultValue(Values.empty)
  }
  def traverse(tree: GTree) {
    if(visitedBlocks(tree)) return else visitedBlocks += tree
    treePosHolder = tree
    tree match {
      case forloop @ Apply(TypeApply(Select(collection, foreach_map), _), List(Function(List(ValDef(_, _, _, _)), _))) =>
        forLoop(forloop)
      
      case ValDef(m: Modifiers, valName, TypeTree(), Literal(Constant(a: Int))) if(!m.hasFlag(MUTABLE)) =>
        val valNameStr = valName.toString.trim
        vals += valNameStr -> Values(a, valNameStr)
        //println(vals(valName.toString))

      case ValDef(m: Modifiers, valName, TypeTree(), expr) if(!m.hasFlag(MUTABLE) && !m.hasFlag(LAZY)) /*&& !computeExpr(expr).isEmpty*/ => //&& computeExpr(expr).isValue =>
        //ADD: aliasing... val a = i, where i is an iterator, then 1/i-a is divbyzero
        //ADD: isSeq and actualSize
        val valNameStr = valName.toString.trim
        vals += valNameStr -> computeExpr(expr).addName(valNameStr)
        //println("newVal: "+vals(valName.toString))
      
      case Match(pat, cases) if pat.tpe.toString != "Any @unchecked" && cases.size >= 2 =>
        for(c <- cases) {
          val backupVals = vals.map(a=> a).withDefaultValue(Values.empty)
          //TODO: c.pat can override some variables
          traverse(c.body)
          vals = backupVals.withDefaultValue(Values.empty)
        }
      
      case If(condExpr, t, f) => 
        val backupVals = vals.map(a=> a).withDefaultValue(Values.empty)
        
        //println(vals)
        vals = backupVals.map(a => (a._1, a._2.applyCond(condExpr))).withDefaultValue(Values.empty)
        //println(vals)
        t.foreach(traverse)

        vals = backupVals.map(a => (a._1, a._2.applyInverseCond(condExpr))).withDefaultValue(Values.empty)
        f.foreach(traverse)
        
        vals = backupVals.withDefaultValue(Values.empty)
        //println(vals)

      case pos @ Apply(Select(_, op), List(expr)) if (op == nme.DIV || op == nme.MOD) && (computeExpr(expr).exists { a => a == 0 }) => 
        unit.warning(pos.pos, "You will likely divide by zero here.")

      case pos @ Apply(Select(Ident(seq), apply), List(indexExpr)) 
        if methodImplements(pos.symbol, SeqLikeApply) && vals.contains(seq.toString) && vals(seq.toString).actualSize != -1 && (computeExpr(indexExpr).exists { a => a >= vals(seq.toString).actualSize }) =>
        //println(seq.toString)
        //println(computeExpr(indexExpr))
        unit.warning(pos.pos, "You will likely use a too large index for a collection here.")

      case pos @ Apply(Select(seq, apply), List(indexExpr)) 
        if methodImplements(pos.symbol, SeqLikeApply) && (computeExpr(indexExpr).exists { a => a < 0 }) =>
        //println(computeExpr(indexExpr))
        unit.warning(pos.pos, "You will likely use a negative index for a collection here.")
      
      case DefDef(_, _, _, _, _, block) => 
        val backupVals = vals.map(a=> a).withDefaultValue(Values.empty)
        traverse(block)
        vals = backupVals.withDefaultValue(Values.empty)

      case a: AbstractInterpretation.this.global.Tree => 
        //if(vals.nonEmpty) println("in: "+showRaw(tree))
        //if(vals.nonEmpty) println(">   "+vals);
        tree.children.foreach(traverse)
    }
  }
}
