package com.foursquare.lint

import scala.tools.nsc.{Global}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.symtab.Flags.{IMPLICIT, OVERRIDE, MUTABLE, CASE, LAZY}
import com.foursquare.lint.global._

class AbstractInterpretation(val global: Global) {
  import global._

  object Values {
    lazy val empty = new Values()
    def apply(low: Int, high: Int, name: String): Values = new Values(name = name, ranges = Set((low, high)))
    def apply(s: Set[Int], name: String): Values = new Values(name = name, values = s)
    def apply(i: Int, name: String = ""): Values = new Values(name = name, values = Set(i))
  }
  class Values(
      val ranges: Set[(Int, Int)] = Set[(Int, Int)](),
      val values: Set[Int] = Set[Int](),
      val name: String = ""
    ) {
    //TODO implement interval tree
    //println(this)
   
    def contains(i: Int): Boolean = (values contains i) || (ranges exists { case (low, high) => i >= low && i <= high })
    def apply(i: Int) = contains(i)
    def exists(func: Int => Boolean) = (values exists func) || (ranges exists { case (low, high) => Range(low, high+1) exists func })
    //def forAll(func: Int => Boolean) = (values forAll func) || (ranges forAll { case (low, high) => Range(low, high+1) forAll func })
    
    def addRange(low: Int, high: Int): Values = new Values(ranges + (if(low > high) (high, low) else (low, high)), values, name)
    def addValue(i: Int): Values = new Values(ranges, values + i, name)
    def addSet(s: Set[Int]): Values = new Values(ranges, values ++ s, name)
    def addName(s: String): Values = new Values(ranges, values, s)
    
    def isEmpty = this.size == 0
    def isValue = this.values.size == 1 && this.ranges.size == 0
    def getValue = if(isValue) values.head else throw new Exception()
    def getValueForce = if(isValue) values.head else if(ranges.size == 1 && this.size == 1) ranges.head._1 else throw new Exception()

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

    def map(func: Int => Int): Values = {
      new Values(
        ranges
          .map { case (low, high) => (func(low), func(high)) }
          .map { case (low, high) if low > high => (high, low); case (low, high) => (low, high) },
        values
          .map(func))
    }
    
    def isUsed(t: Tree, name: String): Boolean = t match { //TODO: there are possible errors, but we're returning empty anyways :)
      //case List(a) if a.toString == name => true
      case a if a.toString == name => true
      //case List() => false
      case a =>
        t.foreach(st => if(st != t && isUsed(st, name)) return true)
        false
    }
    def applyCond(condExpr: Tree) = {
      if(!isUsed(condExpr, this.name)) this else condExpr match {
        case Apply(Select(Ident(v), op), List(Literal(Constant(value: Int)))) if v.toString == this.name => 
          op match {
            case a if a.toString == "$bang$eq" => if(this.exists(a => a == value)) this.dropValue(value) else Values.empty 
            case a if a.toString == "$eq$eq" => if(this.exists(a => a == value)) Values(value) else Values.empty 
            case nme.GT => new Values(
                ranges.flatMap { case orig @ (low, high) => if(low > value) Some(orig) else if(high > value) Some((value+1, high)) else None },
                values.filter { a => a > value },
                this.name)
            case nme.GE => new Values(
                ranges.flatMap { case orig @ (low, high) => if(low >= value) Some(orig) else if(high >= value) Some((value, high)) else None },
                values.filter { a => a >= value },
                this.name)
            case nme.LT => new Values(
                ranges.flatMap { case orig @ (low, high) => if(high < value) Some(orig) else if(low < value) Some((low, value-1)) else None },
                 values.filter { a => a < value },
                this.name)
            case nme.LE => new Values(
                ranges.flatMap { case orig @ (low, high) => if(high <= value) Some(orig) else if(low <= value) Some((low, value)) else None },
                values.filter { a => a <= value },
                this.name)
            case _ => Values.empty
          }
        case _ => Values.empty
      }
    }
    def applyInverseCond(condExpr: Tree) = { //TODO: wow, really... copy paste?
      if(!isUsed(condExpr, this.name)) this else condExpr match {
        //ADD: grouping with && ||, etc... replace both sides with with Expr (but test - seems weird)
        case Apply(Select(Ident(v), op), List(Literal(Constant(value: Int)))) if v.toString == this.name => 
          op match { //TODO: warn if some condition can never be true
            case a if a.toString == "$eq$eq" => if(this.exists(a => a == value)) this.dropValue(value) else Values.empty 
            case a if a.toString == "$bang$eq" => if(this.exists(a => a == value)) Values(value) else Values.empty 
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
            case _ => Values.empty
          }
        case _ => Values.empty
      }
    }
    
    def applyUnary(op: Name): Values = op match { 
      case nme.UNARY_+ => this
      case nme.UNARY_- => this.map(a=> -a)
      case nme.UNARY_~ => this.map(a=> ~a)
      case abs if abs.toString == "abs" =>
        new Values(
          ranges
            .map { case (low, high) => (if(low > 0) low else 0, math.max(math.abs(low), math.abs(high))) }
            .map { case (low, high) if low > high => (high, low); case (low, high) => (low, high) },
          values.map(a => math.abs(a)))
      case size if (size.toString == "size" || size.toString == "length") && ((this.ranges.size == 1 && this.values.size == 0) || (this.ranges.size == 0 && this.values.size >= 1)) =>
        Values(this.size)
      case head if (head.toString == "head") && (this.size == 1) => Values(this.getValueForce) //Only works for one element :) 
      case last if (last.toString == "last") && (this.size == 1) => Values(this.getValueForce) //Only works for one element :)
      case _ => Values.empty
    }
    def apply(op: Name)(right: Values): Values = {
      val left = this
      
      val func: (Int, Int) => Int = op match {
          case nme.ADD => _ + _
          case nme.SUB => _ - _
          case nme.MUL => _ * _
          case nme.AND => _ & _
          case nme.OR  => _ | _
          case nme.XOR => _ ^ _
          case nme.LSL => _ << _
          case nme.LSR => _ >>> _
          case nme.ASR => _ >> _
          case nme.MOD if right.isValue && right.getValue != 0 => _ % _
          case nme.DIV if right.isValue && right.getValue != 0 => _ / _
          case a if a.toString == "apply" => _ + _ //Foo, check below
          case _ => return Values.empty
      }
      
      if(left.isEmpty || right.isEmpty) {
        //ADD: x & 2^n is Set(2^n,0) and stuff like that :)
        if(left.contains(0) || right.contains(0)) {
          if(Set(nme.MUL, nme.AND).contains(op)) Values(0) else Values.empty
        } else {
          Values.empty
        }
      } else if(left.isValue && right.isValue) {
        Values(func(left.getValue, right.getValue))
      } else if(!left.isValue && right.isValue) {
        if(op.toString == "apply" && ((this.ranges.size == 1 && this.values.size == 0) || (this.ranges.size == 0 && this.values.size >= 1))) {
          //TODO: if you wanted actual values, you need to save seq type and refactor values from Set to Seq
          if(this.size == 1) Values(this.getValueForce) else this
          println(this)
          this
        } else {
          left.map(a => func(a, right.getValue))
        }
      } else if(left.isValue && !right.isValue) {
        right.map(a => func(left.getValue, a))
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
    
    override def toString: String = "Values("+(if(name.size > 0) name+")(" else "")+(values.map(_.toString) ++ ranges.map(a => a._1+"-"+a._2)).mkString(",")+")"
  }
  
  //ADD: GenSeq, and then implement .head .tail, ...
  val SeqLikeObject: Symbol = definitions.getModule(newTermName("scala.collection.GenTraversable"))
  val SeqLikeClass: Symbol = definitions.getClass(newTermName("scala.collection.SeqLike"))
  val SeqLikeContains: Symbol = SeqLikeClass.info.member(newTermName("contains"))
  val SeqLikeApply: Symbol = SeqLikeClass.info.member(newTermName("apply"))
  val SeqLikeGenApply: Symbol = SeqLikeObject.info.member(newTermName("apply"))
  def methodImplements(method: Symbol, target: Symbol): Boolean = {
    method == target || method.allOverriddenSymbols.contains(target)
  }
  
      
  var vals = collection.mutable.HashMap[String, Values]().withDefaultValue(Values.empty)
  def forLoop(tree: GTree, unit: GUnit) {
    //TODO: actually anything that takes (A <: (a Number) => _), this is awful
    val funcs = "foreach|map|filter(Not)?|exists|find|flatMap|forall|groupBy|count|((drop|take)While)|(min|max)By|partition|span"

    val backupVals = vals.map(a=> a).withDefaultValue(Values.empty)
    
    //TODO: extend with more functions... and TEST TEST TEST TEST
    def computeExpr(tree: Tree, curr: Values = Values.empty): Values = {
      tree match {
        case Literal(Constant(value: Int)) => Values(value)
        case Ident(termName) => vals(termName.toString)
        
        case Apply(Select(Apply(Select(scala_Predef, intWrapper), List(Literal(Constant(low: Int)))), to_until), List(Literal(Constant(high: Int)))) if (to_until.toString matches "to|until") =>
          (if(to_until.toString == "to") Values(low, high, "") else Values(low, high-1, ""))

        case Apply(Select(Apply(Select(scala_Predef, intWrapper), List(expr1)), to_until), List(expr2)) if (to_until.toString matches "to|until") && computeExpr(expr1, curr).isValue && computeExpr(expr2, curr).isValue =>
          val (low, high) = (computeExpr(expr1, curr).getValue, computeExpr(expr2, curr).getValue)
          (if(to_until.toString == "to") Values(low, high, "") else Values(low, high-1, ""))

        case Apply(TypeApply(genApply @ Select(_,_), _), genVals) if methodImplements(genApply.symbol, SeqLikeGenApply) =>
          //TODO: oh god, this isn't typesafe in any way
          //ADD: List(expr,2,3) - save size, even if you can't compute expr
          val values = genVals.map(v => computeExpr(v))
          if(values.forall(_.isValue)) Values(values.map(_.getValue).toSet, "") else Values.empty

        case Select(expr, op) => computeExpr(expr, curr).applyUnary(op)
        
        case Apply(Select(scala_math_package, abs), List(expr)) if scala_math_package.toString == "scala.math.`package`" && abs.toString == "abs" => //TODO: scala and java abs
          computeExpr(expr, curr).applyUnary(abs)

        case Apply(Select(expr1, op), List(expr2)) =>
          //println(op)
          (computeExpr(expr1, curr))(op)(computeExpr(expr2, curr))
        

        case _ => Values.empty
      }
    }
    
    val (param, values, body) = tree match {
      case Apply(TypeApply(Select(collection, foreach_map), _), List(Function(List(ValDef(_, param, _, _)), body))) if (foreach_map.toString matches funcs) =>
        val values = computeExpr(collection).addName(param.toString)
        if(values.isEmpty) {
            //println("not a collection I know("+tree.pos+"): "+showRaw(collection))
            //return
        }
        
        (param, values, body)
      case _ => 
        //println("not a loop("+tree.pos+"): "+showRaw(tree))
        return
    }
    vals += param.toString -> values

    def traverseFor(tree: GTree) {
      tree match {
        case forloop @ Apply(TypeApply(Select(collection, foreach_map), _), List(Function(List(ValDef(_, param, _, _)), body))) =>
          forLoop(forloop, unit)
        
        case ValDef(m: Modifiers, valName, TypeTree(), Literal(Constant(a: Int))) if(!m.hasFlag(MUTABLE)) =>
          //println(valName.toString)
          vals += valName.toString -> Values(a, valName.toString)

        case ValDef(m: Modifiers, valName, TypeTree(), expr) if(!m.hasFlag(MUTABLE) && !m.hasFlag(LAZY)) && !computeExpr(expr).isEmpty => //&& computeExpr(expr).isValue =>
          //ADD: aliasing... val a = i, where i is an iterator, then 1/i-a is divbyzero
          vals += valName.toString -> computeExpr(expr).addName(valName.toString)
          //println(vals(valName.toString))
        
        case If(condExpr, t, f) => 
          //println("in")
          //println(vals);
          val backupVals = vals.map(a=> a).withDefaultValue(Values.empty)
          
          vals = backupVals.map(a => (a._1, a._2.applyCond(condExpr))).withDefaultValue(Values.empty)
          //println(vals)
          t.foreach(traverseFor)

          vals = backupVals.map(a => (a._1, a._2.applyInverseCond(condExpr))).withDefaultValue(Values.empty)
          //println(vals)
          f.foreach(traverseFor)
          
          vals = backupVals.withDefaultValue(Values.empty)
          //println("out")

        case pos @ Apply(Select(_, op), List(expr)) if (op == nme.DIV || op == nme.MOD) && (computeExpr(expr).exists { a => a == 0 }) => 
          unit.warning(pos.pos, "During this loop you will likely divide by zero.")

        case pos @ Apply(Select(Ident(seq), apply), List(indexExpr)) 
          if methodImplements(pos.symbol, SeqLikeApply) && vals.contains(seq.toString) && (computeExpr(indexExpr).exists { a => a >= vals(seq.toString).size }) =>
          //println(seq.toString)
          //println(computeExpr(indexExpr))
          unit.warning(pos.pos, "During this loop you will likely use a too large index for a collection.")

        case pos @ Apply(Select(seq, apply), List(indexExpr)) 
          if methodImplements(pos.symbol, SeqLikeApply) && (computeExpr(indexExpr).exists { a => a < 0 }) =>
          //println(computeExpr(indexExpr))
          unit.warning(pos.pos, "During this loop you will likely use a negative index for a collection.")
        
        case _ => tree.children.foreach(traverseFor)
      }
    }
    
    traverseFor(body)
    vals = backupVals.withDefaultValue(Values.empty)
  }
 
}
