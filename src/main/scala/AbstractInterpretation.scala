package com.foursquare.lint

import scala.tools.nsc.{Global}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.symtab.Flags.{IMPLICIT, OVERRIDE, MUTABLE, CASE}
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
   
    def contains(i: Int) = (values contains i) || (ranges exists { case (low, high) => i >= low && i <= high })
    def apply(i: Int) = contains(i)
    def exists(func: Int => Boolean) = (values exists func) || (ranges exists { case (low, high) => Range(low, high+1) exists func })
    //def forAll(func: Int => Boolean) = (values forAll func) || (ranges forAll { case (low, high) => Range(low, high+1) forAll func })
    
    def addRange(low: Int, high: Int): Values = new Values(ranges + (if(low > high) (high, low) else (low, high)), values, name)
    def addValue(i: Int): Values = new Values(ranges, values + i, name)
    def addSet(s: Set[Int]): Values = new Values(ranges, values ++ s, name)
    
    def isEmpty = this.size == 0
    def isValue = this.size == 1
    def getValue = if(isValue) values.head else throw new Exception()

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
        //TODO: grouping with && ||, etc
        case Apply(Select(Ident(v), op), List(Literal(Constant(value: Int)))) if v.toString == this.name => 
          op match { //TODO: warn if some condition can never be true
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
        //TODO: grouping with && ||, etc
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
      case nme.UNARY_- => this.map(a=> -a)
      case abs if abs.toString == "abs" =>
        new Values(
          ranges
            .map { case (low, high) => (if(low > 0) low else 0, math.max(math.abs(low), math.abs(high))) }
            .map { case (low, high) if low > high => (high, low); case (low, high) => (low, high) },
          values.map(a => math.abs(a)))
      case _ => Values.empty
    }
    def apply(op: Name)(right: Values): Values = {
      val left = this
      
      val func: (Int, Int) => Int = op match {
          case nme.ADD => _ + _
          case nme.SUB => _ - _
          case nme.MUL => _ * _
          case nme.DIV if right.isValue && right.getValue != 0 => _ / _
          case _ => return Values.empty
      }
      
      val a = if(left.isEmpty || right.isEmpty) {
        Values.empty
      } else if(left.isValue && right.isValue) {
        Values(func(left.getValue, right.getValue))
      } else if(!left.isValue && right.isValue) {
        left.map(a => func(a, right.getValue))
      } else if(left.isValue && !right.isValue) {
        right.map(a => func(left.getValue, a))
      } else {
        Values.empty //TODO: join ranges, but be afraid of the explosion :)
      }
      
      a
    }
    
    //approximate
    def size: Int = values.size + ranges.foldLeft(0)((acc, range) => acc + (range._2 - range._1) + 1)
    
    override def toString: String = "Values("+(if(name.size > 0) name+")(" else "")+(values.map(_.toString) ++ ranges.map(a => a._1+"-"+a._2)).mkString(",")+")"
  }
  
  val SeqLikeObject: Symbol = definitions.getModule(newTermName("scala.collection.GenTraversable"))
  val SeqLikeClass: Symbol = definitions.getClass(newTermName("scala.collection.SeqLike"))
  val SeqLikeContains: Symbol = SeqLikeClass.info.member(newTermName("contains"))
  val SeqLikeApply: Symbol = SeqLikeClass.info.member(newTermName("apply"))
  val SeqLikeGenApply: Symbol = SeqLikeObject.info.member(newTermName("apply"))
  def methodImplements(method: Symbol, target: Symbol): Boolean = {
    method == target || method.allOverriddenSymbols.contains(target)
  }
      
  def forLoop(tree: GTree, unit: GUnit) {
    val (param, values, body) = tree match {
      case Apply(TypeApply(Select(collection, foreach_map), _), List(Function(List(ValDef(_, param, _, _)), body))) if (foreach_map.toString matches "foreach|map") =>
        val values = collection match {
          case Apply(Select(Apply(Select(scala_Predef, intWrapper), List(Literal(Constant(low: Int)))), to_until), List(Literal(Constant(high: Int)))) if (to_until.toString matches "to|until") =>
            (if(to_until.toString == "to") Values(low, high, param.toString) else Values(low, high-1, param.toString))
          //case Apply(genApply @ Select(_, _), vals) => //if methodImplements(SeqLikeGenApply, genApply.symbol) =>
          case Apply(TypeApply(genApply @ Select(_,_/*Select(This(newTypeName("immutable")), scala.collection.immutable.List), newTermName("apply")*/), _), vals) =>
            val values = vals.map(_.toString).filter(_ matches "-?[0-9]{1,10}").map(_.toInt).toSet
            //println(values)
            Values(values, param.toString)

          case _ => 
            //println("not a collection I know("+tree.pos+"): "+showRaw(collection));
            return
        }
        
        (param, values, body)
      case _ => println("not a loop"); return
    }
  
    var vals = collection.mutable.HashMap[String, Values](
      param.toString -> values
    ).withDefaultValue(Values.empty)

    //TODO: extend with more functions... and TEST TEST TEST TEST
    def computeExpr(tree: Tree, curr: Values = Values.empty): Values = {
      tree match {
        case Literal(Constant(value: Int)) => Values(value)
        case Ident(termName) => vals(termName.toString)
        
        case Select(expr, op) => computeExpr(expr, curr).applyUnary(op)
        
        case Apply(Select(scala_math_package, abs), List(expr)) if scala_math_package.toString == "scala.math.`package`" && abs.toString == "abs" => //TODO: scala and java abs
          computeExpr(expr, curr).applyUnary(abs)

        case Apply(Select(expr1, op), List(expr2)) =>
          (computeExpr(expr1, curr))(op)(computeExpr(expr2, curr))
        
        case _ => Values.empty
      }
    }

    def traverseFor(tree: GTree) {
      tree match {
        case ValDef(m: Modifiers, valName, TypeTree(), Literal(Constant(a: Int))) if(!m.hasFlag(MUTABLE)) =>
          //println(valName.toString)
          vals += valName.toString -> Values(a, valName.toString)
        
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
          
          vals = backupVals
          //println("out")

        case pos @ Apply(Select(_, op), List(expr)) if (op == nme.DIV || op == nme.MOD) && (computeExpr(expr).exists { a => a == 0 }) => 
          unit.warning(pos.pos, "During this loop you will likely divide by zero.")

        case pos @ Apply(Select(seq, apply), List(indexExpr)) 
          if methodImplements(pos.symbol, SeqLikeApply) && (computeExpr(indexExpr).exists { a => a < 0 }) =>
          //println(computeExpr(indexExpr))
          unit.warning(pos.pos, "During this loop you will likely use a negative index for a collection.")
        
        case _ => tree.children.foreach(traverseFor)
      }
    }
    
    traverseFor(tree)
  }
 
}
