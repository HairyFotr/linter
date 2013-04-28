/**
 *   Copyright 2012 Foursquare Labs, Inc.
 *
 *   Licensed under the Apache License, Version 2.0 (the "License");
 *   you may not use this file except in compliance with the License.
 *   You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

package com.foursquare.lint

import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.symtab.Flags.{IMPLICIT, OVERRIDE, MUTABLE, CASE}

package object global {
  type GTree = Global#Tree
  type GUnit = Global#CompilationUnit
}
class LinterPlugin(val global: Global) extends Plugin {
  import global._

  val name = "linter"
  val description = ""
  val components = List[PluginComponent](PreTyperComponent, LinterComponent, AfterLinterComponent)
  
  override val optionsHelp: Option[String] = Some("  -P:linter No options yet, just letting you know I'm here")

  private object PreTyperComponent extends PluginComponent {
    import global._

    val global = LinterPlugin.this.global

    override val runsAfter = List("parser")

    val phaseName = "linter-parsed"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: global.CompilationUnit) {
        new PreTyperTraverser(unit).traverse(unit.body)
      }
    }

    class PreTyperTraverser(unit: CompilationUnit) extends Traverser {
      override def traverse(tree: Tree) {
        tree match {
          //The fact that I don't track the whole chain means false negatives.
          case ValDef(m: Modifiers, varName, TypeTree(), value) if(m.hasFlag(MUTABLE)) =>
            varDecls += varName.toString.trim
            //println("vardecl |"+varName.toString.trim+"|")
          case Apply(Select(a, assign), _) if varDecls.contains(assign.toString.dropRight(4)) => 
            val varName = assign.toString.dropRight(4)
            varAssigns += varName.toString
            //println("varassign |"+varName+"|")
          case Assign(Ident(varName), _) =>
            varAssigns += varName.toString
            //println("varassign2 |"+varName+"|")
            //TODO: += and the like seem to not be detected
            
          case DefDef(mods: Modifiers, name, _, valDefs, typeTree, block) =>
            if(name.toString != "<init>" && !block.isEmpty && !mods.hasFlag(OVERRIDE)) {
              //Get the parameters, except the implicit ones
              val params = valDefs.flatMap(_.filterNot(_.mods.hasFlag(IMPLICIT))).map(_.name.toString).toBuffer
              if(!(name.toString == "main" && params.size == 1 && params.head == "args")) { // filter main method
                val used = for(Ident(name) <- tree if params contains name.toString) yield name.toString
                val unused = params -- used
                
                unused.size match { //TODO: scalaz is a good codebase for finding interesting false positives
                  case 0 =>
                  case 1 => unit.warning(tree.pos, "Parameter %s is not used in method %s" format (unused.mkString(", "), name))
                  case _ => unit.warning(tree.pos, "Parameters (%s) are not used in method %s" format (unused.mkString(", "), name))
                }
              }
            }

            if(mods.hasFlag(IMPLICIT) && typeTree.isEmpty && !(name.toString matches "((i?)to).*|.*To[A-Z].*")) unit.warning(tree.pos, "Implicit method %s needs explicit return type" format name)
          case _ => 
        }
        super.traverse(tree)
      }
    }
  }


  //used for simple var -> val cases
  val varDecls,varAssigns = collection.mutable.HashSet[String]() //TermName actually...

  private object LinterComponent extends PluginComponent {
    import global._

    implicit val global = LinterPlugin.this.global

    override val runsAfter = List("typer")

    val phaseName = "linter-typed"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: global.CompilationUnit) {
        new LinterTraverser(unit).traverse(unit.body)
      }
    }

    class LinterTraverser(unit: CompilationUnit) extends Traverser {
      import definitions.{AnyClass, ObjectClass, Object_==, OptionClass, SeqClass}
      
      val stringLiteralCount = collection.mutable.HashMap[String, Int]().withDefaultValue(0)
      //some common ones, and some play framework hacks
      val stringLiteralExceptions = """(\s*|GET|POST|[\/.{}(), ?x%+_-]{0,3})"""
      val stringLiteralFileExceptions = Set("routes_routing.scala", "routes_reverseRouting.scala")

      val JavaConversionsModule: Symbol = definitions.getModule(newTermName("scala.collection.JavaConversions"))
      val SeqLikeClass: Symbol = definitions.getClass(newTermName("scala.collection.SeqLike"))
      val SeqLikeContains: Symbol = SeqLikeClass.info.member(newTermName("contains"))
      val SeqLikeApply: Symbol = SeqLikeClass.info.member(newTermName("apply"))
      val OptionGet: Symbol = OptionClass.info.member(nme.get)
      
      val IsInstanceOf = AnyClass.info.member(nme.isInstanceOf_)
      val AsInstanceOf = AnyClass.info.member(nme.asInstanceOf_)
      val ToString: Symbol = AnyClass.info.member(nme.toString_)

      val DoubleClass: Symbol = definitions.getClass(newTermName("scala.Double"))
      val FloatClass: Symbol = definitions.getClass(newTermName("scala.Float"))

      def SeqMemberType(seenFrom: Type): Type = {
        SeqLikeClass.tpe.typeArgs.head.asSeenFrom(seenFrom, SeqLikeClass)
      }

      def isSubtype(x: Tree, y: Tree): Boolean = { x.tpe.widen <:< y.tpe.widen }
      def isSubtype(x: Tree, y: Type): Boolean = { x.tpe.widen <:< y.widen }

      def methodImplements(method: Symbol, target: Symbol): Boolean = {
        method == target || method.allOverriddenSymbols.contains(target)
      }

      def isGlobalImport(selector: ImportSelector): Boolean = {
        selector.name == nme.WILDCARD && selector.renamePos == -1
      }
      
      val abstractInterpretation = new AbstractInterpretation(global, unit)

      override def traverse(tree: Tree) { 
        //abstractInterpretation.traverseBlock(tree)
        tree match {
          case ValDef(m: Modifiers, varName, TypeTree(), value) if(m.hasFlag(MUTABLE)) =>
            varDecls += varName.toString.trim
            //println("vardecl |"+varName.toString.trim+"|")
          case Apply(Select(a, assign), _) if varDecls.contains(assign.toString.dropRight(4)) => 
            val varName = assign.toString.dropRight(4)
            varAssigns += varName.toString
            //println("varassign |"+varName+"|")
            
          case Assign(Ident(varName), _) =>
            varAssigns += varName.toString
            //println("varassign2 |"+varName+"|")

          case ClassDef(m: Modifiers, className, _, _) if m.hasFlag(CASE) => return // case classes cause false positives...

          case Select(fromFile, _) if fromFile.toString startsWith "scala.io.Source.fromFile" =>
          //TODO: Too hacky detection, also doesn't actually check if you close it - just that you don't use it as a oneliner
            val warnMsg = "You should close the file stream after use."
            unit.warning(fromFile.pos, warnMsg)
            return
            
          case If(cond1, _, If(cond2, _, _)) if cond1 equalsStructure cond2 =>
            unit.warning(cond2.pos, "The else-if has the same condition.")

          case Apply(Select(left, func), List(right)) if (func.toString matches "[$]amp[$]amp|[$]bar[$]bar") && (left equalsStructure right) =>        
            unit.warning(tree.pos, "Structurally the same expression on both sides of condition.")
          case Apply(Select(left, func), List(right)) if (func.toString matches "[$](greater|less|eq)([$]eq)?") && (left equalsStructure right) =>        
            unit.warning(tree.pos, "Structurally the same expression on both sides of comparison.")

          case Apply(Select(Literal(const), func), params) if params.size == 1 && (func.toString matches "[$](greater|less|eq)([$]eq)?") && (params.head match { case Literal(_) => false case _ => true })  =>
            unit.warning(tree.pos, "You are using Yoda conditions") // http://www.codinghorror.com/blog/2012/07/new-programming-jargon.html :)

          case Apply(Select(Literal(Constant(s: String)), func), params) =>
            func.toString match {
              case "$plus"|"equals"|"$eq$eq"|"toCharArray" => //false positives            
              case "length" => unit.warning(tree.pos, "Taking the size of a constant string")
              case _        => unit.warning(tree.pos, "Processing a constant string")
            }
            
          case Select(Apply(Select(predef, augmentString), List(Literal(Constant(s: String)))), size)
            if predef.toString == "scala.this.Predef" && augmentString.toString == "augmentString" && size.toString == "size" => 
            unit.warning(tree.pos, "Taking the size of a constant string")
            
          case Apply(Select(lhs, nme.EQ), List(rhs)) if isSubtype(lhs, DoubleClass.tpe) || isSubtype(lhs, FloatClass.tpe) || isSubtype(rhs, DoubleClass.tpe) || isSubtype(rhs, FloatClass.tpe) =>
            //val warnMsg = "Exact comparison of floating point values is potentially unsafe."
            //unit.warning(tree.pos, warnMsg)
          case Apply(Select(packageName, log), List(Apply(Select(Literal(const), plus), _)))
            if packageName.toString == "scala.math.`package`" && log.toString == "log" && plus.toString == "$plus" && (const == Constant(1.0) || const == Constant(1)) => 

            unit.warning(tree.pos, "Use math.log1p instead of math.log for added accuracy.")
            
          case Apply(eqeq @ Select(lhs, nme.EQ), List(rhs)) if methodImplements(eqeq.symbol, Object_==) && !isSubtype(lhs, rhs) && !isSubtype(rhs, lhs) =>
            val warnMsg = "Comparing with == on instances of different types (%s, %s) will probably return false."
            unit.warning(eqeq.pos, warnMsg.format(lhs.tpe.widen, rhs.tpe.widen))

          case Import(pkg, selectors) if pkg.symbol == JavaConversionsModule && selectors.exists(isGlobalImport) =>
            unit.warning(pkg.pos, "Conversions in scala.collection.JavaConversions._ are dangerous.")
          
          case Import(pkg, selectors) if selectors.exists(isGlobalImport) =>
            //TODO: Too much noise - maybe it would be useful to non-IDE if it printed a nice selector import replacement
            //unit.warning(pkg.pos, "Wildcard imports should be avoided. Favor import selector clauses.")

          case Apply(contains @ Select(seq, _), List(target)) if methodImplements(contains.symbol, SeqLikeContains) && !(target.tpe <:< SeqMemberType(seq.tpe)) =>
            val warnMsg = "%s.contains(%s) will probably return false."
            unit.warning(contains.pos, warnMsg.format(seq.tpe.widen, target.tpe.widen))

          //TODO: false positives in case class A(), and in the interpreter init
          //case aa @ Apply(a, List(b @ Apply(s @ Select(instanceOf,dd),ee))) if methodImplements(instanceOf.symbol, AsInstanceOf) =>
          //  println((aa,instanceOf))
          case instanceOf @ Select(a, func) if methodImplements(instanceOf.symbol, AsInstanceOf) =>   
            //TODO: too much noise, maybe detect when it's completely unnecessary
            //unit.warning(tree.pos, "Avoid using asInstanceOf[T] (use pattern matching, type ascription, etc).")

          case get @ Select(_, nme.get) if methodImplements(get.symbol, OptionGet) => 
            //TODO: if(x.isDefined) func(x.get) / if(x.isEmpty) ... else func(x.get), etc. are false positives
            //unit.warning(tree.pos, "Calling .get on Option will throw an exception if the Option is None.")

          case If(Apply(Select(_, nme.EQ), List(Literal(Constant(null)))), Literal(Constant(false)), Literal(Constant(true))) =>
            // Fixes both the null warning and if check for """case class A()""" ... (x$0.==(null) - see unapply AST of such case class)

          case Literal(Constant(null)) =>
            //TODO: Too much noise - limit in some way
            //unit.warning(tree.pos, "Using null is considered dangerous.")
          case s @ Literal(Constant(str: String)) =>
            //TODO: String interpolation gets broken down into parts and causes false positives
            //TODO: some quick benchmark showed string literals are actually more optimized than almost anything else, even final vals
            val threshold = 4

            stringLiteralCount(str) += 1
            if(stringLiteralCount(str) == threshold && !(stringLiteralFileExceptions.contains(unit.source.toString)) && !(str.matches(stringLiteralExceptions))) {
              //unit.warning(tree.pos, unit.source.path.toString)
              //Too much noise :)
              //unit.warning(tree.pos, """String literal """"+str+"""" appears multiple times.""")
            }
            
            implicit def String2StringAttrs(s: String) = new abstractInterpretation.StringAttrs(exactValue = Some(s))
            if(abstractInterpretation.stringVals contains str) {
              unit.warning(s.pos, "You have defined that string as a val already, maybe use that?")
              abstractInterpretation.visitedBlocks += s
            }

          case Match(Literal(Constant(a)), cases) =>
            //TODO: figure this, and similar if rules, for some types of val x = Literal(Constant(...)) declarations
            
            val returnVal = //try to detect what it'll return
              cases 
                .map { ca => (ca.pat, ca.body) } 
                .find { case (Literal(Constant(c)), _) => c == a; case _ => false}
                .map { _._2 } 
                .orElse { if(cases.last.pat.toString == "_") Some(cases.last.body) else None } 
                .map { s => " will always return " + s }
                .getOrElse("")
            
            unit.warning(tree.pos, "Pattern matching on a constant value " + a /*+ returnVal*/ + ".")

          case Match(pat, cases) if pat.tpe.toString != "Any @unchecked" && cases.size >= 2 =>
            //"Any @unchecked" seems to happen on the matching structures of actors - and all cases return true
            //TODO: there seems to be more of this - see failing test for Futures in testCaseChecks

            //Checking if matching on Option or Boolean
            var optionCase, booleanCase = false
            val (optionCaseReg, booleanCaseReg) = ("(Some[\\[].*[\\]]|None[.]type)", "Boolean[(](true|false)[)]") //TODO: Hacky hack hack -_-, sorry
            def checkCase(caseTree: CaseDef) {
              val caseStr = caseTree.pat.toString
              val caseTypeStr = caseTree.pat.tpe.toString
              //println((caseStr, caseTypeStr))

              optionCase |= (caseTypeStr matches optionCaseReg)
              booleanCase |= (caseTypeStr matches booleanCaseReg)  
            }
            def printCaseWarning() {
              if(cases.size == 2) {
                if(optionCase) {
                  //TODO: too much noise, and some cases are perfectly fine - try detecting all the exact cases from link
                  //unit.warning(tree.pos, "There are probably better ways of handling an Option. (see: http://blog.tmorris.net/posts/scalaoption-cheat-sheet/)")
                } else if(booleanCase) {
                  //TODO: case something => ... case _ => ... is also an if in a lot of cases
                  unit.warning(tree.pos, "This is probably better written as an if statement.")
                }
              }
            }
            
            //Checking for duplicate case bodies
            case class Streak(streak: Int, tree: CaseDef)
            var streak = Streak(0, cases.head)
            def checkStreak(c: CaseDef) {
              if((c.body equalsStructure streak.tree.body) && !(c.body.children == List())) {
                streak = Streak(streak.streak + 1, c)
              } else {
                printStreakWarning()
                streak = Streak(1, c)
              }
            }
            def printStreakWarning() {
              if(streak.streak == cases.size) {
                //This one always turns out to be a false positive
                //unit.warning(tree.pos, "All "+cases.size+" cases will return "+cases.head.body+", regardless of pattern value") 
              } else if(streak.streak > 1) {
                //TODO: should check actual cases
                //unit.warning(streak.tree.body.pos, streak.streak+" neighbouring cases are identical, and could be merged.")
              }
            }

            for(c <- cases) {
              checkCase(c)
              checkStreak(c)
            }

            printStreakWarning()
            printCaseWarning()

          case If(cond, Literal(Constant(true)), Literal(Constant(false))) =>
            //TODO: Can I get the unprocessed condition string?
            unit.warning(cond.pos, "Remove the if and just use the condition.")
          case If(cond, Literal(Constant(false)), Literal(Constant(true))) =>
            unit.warning(cond.pos, "Remove the if and just use the negated condition.")
          case If(cond, a, b) if (a equalsStructure b) && (a.children.nonEmpty) =>
            //TODO: empty if statement (if(...) { }) triggers this - change warning for that case
            unit.warning(cond.pos, "Both if statement branches have the same structure.")

          case If(cond @ Literal(Constant(a: Boolean)), _, _) => 
            //TODO: try to figure out things like (false && a > 5 && ...) (btw, this works if a is a final val)
            //TODO: there are people still doing breakable { while
            val warnMsg = "This condition will always be "+a+"."
            unit.warning(cond.pos, warnMsg)
          case Apply(Select(Literal(Constant(false)), term), _) if term == nme.ZAND =>
            val warnMsg = "This part of boolean expression will always be false."
            unit.warning(tree.pos, warnMsg)
          case Apply(Select(Literal(Constant(true)), term), _) if term == nme.ZOR =>
            val warnMsg = "This part of boolean expression will always be true."
            unit.warning(tree.pos, warnMsg)
            
          case If(_, If(_, body, Literal(Constant(()))), Literal(Constant(()))) =>
            unit.warning(tree.pos, "These two ifs can be merged")
          
          case Apply(Select(Select(scala_package, _BigDecimal), apply), List(Literal(Constant(d:Double))))
            if scala_package.toString == "scala.`package`" && _BigDecimal.toString == "BigDecimal" && apply.toString == "apply" =>
            unit.warning(tree.pos, "Possible loss of precision - use a string constant")
          case Apply(Select(math_BigDecimal, apply_valueOf), List(Literal(Constant(d:Double)))) 
            if math_BigDecimal.toString.endsWith("math.BigDecimal") && (apply_valueOf.toString matches "apply|valueOf") =>
            unit.warning(tree.pos, "Possible loss of precision - use a string constant")
          case Apply(Select(New(java_math_BigDecimal), nme.CONSTRUCTOR), List(Literal(Constant(d: Double)))) 
            if java_math_BigDecimal.toString == "java.math.BigDecimal" =>
            unit.warning(tree.pos, "Possible loss of precision - use a string constant")
            
          case pos @ Apply(Select(expr1, op), List(expr2)) if (op == nme.DIV) => 
            val warnMsg = "Did you mean to use the signum function here? (signum also avoids division by zero)"
            (expr1, expr2) match {
              case (expr1, Apply(abs, List(expr2))) if abs.toString == "scala.math.`package`.abs" && expr1.toString == expr2.toString => unit.warning(pos.pos, warnMsg)
              case (Apply(abs, List(expr1)), expr2) if abs.toString == "scala.math.`package`.abs" && expr1.toString == expr2.toString => unit.warning(pos.pos, warnMsg)
              case _ => //OK
            }

          //ignores "Assignment right after declaration..." in case class hashcode
          case DefDef(mods, name, _, _, _, Block(block, last)) if name.toString == "hashCode" && {
            (block :+ last) match { 
              case ValDef(modifiers, id1, _, _) :: Assign(id2, _) :: _ => true
              case _ => false
            }} => return //

          case ClassDef(mods, name, tparams, impl) =>
            abstractInterpretation.traverseBlock(impl)

          case ModuleDef(mods, name, impl) => 
            abstractInterpretation.traverseBlock(impl)

          case blockElem @ Block(init, last) =>
            abstractInterpretation.traverseBlock(blockElem)

            val block = init :+ last
            //TODO: var v; ...non v related stuff...; v = 4 <-- this is the same thing, really
            (block zip block.tail) foreach { 
              case (ValDef(modifiers, id1, _, _), Assign(id2, assign)) if id1.toString == id2.toString && !abstractInterpretation.isUsed(assign, id2.toString)=>
                unit.warning(id2.pos, "Assignment right after declaration is most likely a bug (unless you side-effect like a boss)")

              //case (Assign(id1, _), Assign(id2, _)) if id1.toString == id2.toString => // stricter
              case (Assign(id1, _), Assign(id2, assign)) if id1.toString == id2.toString && !abstractInterpretation.isUsed(assign, id2.toString) =>
                unit.warning(id2.pos, "Two subsequent assigns are most likely a bug (unless you side-effect like a boss)")

              //TODO: move to def analysis - this is only for those blocks
              //case (_, l @ Return(_)) if l eq last =>
              //  unit.warning(l.pos, "Scala has implicit return, so you don't need 'return'")
              //  true              
              case (v @ ValDef(_, id1, _, _), l @ Ident(id2)) if id1.toString == id2.toString && (l eq last) =>
                unit.warning(v.pos, "You don't need that temp variable.")

              case (If(cond1, _, _), If(cond2, _, _)) if cond1 equalsStructure cond2 =>
                unit.warning(cond1.pos, "Two subsequent ifs have the same condition")

              case (s1, s2) if s1 equalsStructure s2 =>
                unit.warning(s1.pos, "You're doing the exact same thing twice.")

              case _ =>
            }


          case forloop @ Apply(TypeApply(Select(collection, foreach_map), _), List(Function(List(ValDef(_, param, _, _)), body))) if { //if (foreach_map.toString matches "foreach|map") && {
            abstractInterpretation.forLoop(forloop)
            false
          } => //

          case DefDef(_, _, _, _, _, block) => 
            abstractInterpretation.traverseBlock(block)

          case pos @ Apply(Select(seq, apply), List(Literal(Constant(index: Int)))) 
            if methodImplements(pos.symbol, SeqLikeApply) && index < 0 =>
            unit.warning(pos.pos, "Using a negative index for a collection.")

          // cannot check double/float, as typer will automatically translate it to Infinity
          case divByZero @ Apply(Select(rcvr, op), List(Literal(Constant(0))))
            if (op == nme.DIV || op == nme.MOD) 
              &&(rcvr.tpe <:< definitions.ByteClass.tpe
              ||rcvr.tpe <:< definitions.ShortClass.tpe
              ||rcvr.tpe <:< definitions.IntClass.tpe
              ||rcvr.tpe <:< definitions.LongClass.tpe) =>
            unit.warning(divByZero.pos, "Literal division by zero.")

          case _ =>
        }
        super.traverse(tree)
      }
    }
  }
  
  private object AfterLinterComponent extends PluginComponent {
    import global._

    implicit val global = LinterPlugin.this.global

    override val runsAfter = List("linter-typed")

    val phaseName = "linter-typed-after"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: global.CompilationUnit) {
        new AfterLinterTraverser(unit).traverse(unit.body)
      }
    }

    class AfterLinterTraverser(unit: CompilationUnit) extends Traverser {
      override def traverse(tree: Tree) {
        val maybeVals = (varDecls -- varAssigns)
        if(maybeVals.nonEmpty) unit.warning(tree.pos, "[experimental] These vars might secretly be vals: grep -rnP --include=*.scala 'var ([(][^)]*)?("+maybeVals.mkString("|")+")'")
        varDecls.clear
      }
    }
  }
}
