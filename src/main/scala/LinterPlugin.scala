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
  val description = "a static analysis compiler plugin"
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
          case DefDef(mods: Modifiers, name, _, valDefs, typeTree, block) =>
            /// Unused method parameters
            //TODO: This nags, when a class overrides a method, but doesn't mark it as such - check out late flags: http://harrah.github.io/browse/samples/compiler/scala/tools/nsc/symtab/Flags.scala.html
            if(name.toString != "<init>" && !block.isEmpty && !mods.hasFlag(OVERRIDE)) {
              //Get the parameters, except the implicit ones
              val params = valDefs.flatMap(_.filterNot(_.mods.hasFlag(IMPLICIT))).map(_.name.toString).toBuffer

              //TODO: Put into utils
              def isBlockEmpty(block: Tree) = block match {
                case Literal(Constant(a: Unit)) => true
                case Ident(qmarks) if qmarks.toString == "$qmark$qmark$qmark" => true
                case Select(scala_Predef, qmarks) if qmarks.toString == "$qmark$qmark$qmark" => true
                case Throw(_) => true
                //case a if a.isEmpty || a.children.isEmpty => true
                case _ => false
              }
              
              if(!(name.toString == "main" && params.size == 1 && params.head == "args") && !isBlockEmpty(block)) { // filter main method
                val used = for(Ident(name) <- tree if params contains name.toString) yield name.toString
                val unused = params -- used
                
                //TODO: scalaz is a good codebase for finding interesting false positives
                //TODO: macro impl is special case?
                unused.size match {
                  case 0 => //
                  case 1 => unit.warning(tree.pos, "Parameter %s is not used in method %s. (Add override if that's the reason)" format (unused.mkString(", "), name))
                  case _ => unit.warning(tree.pos, "Parameters (%s) are not used in method %s. (Add override if that's the reason)" format (unused.mkString(", "), name))
                }
              }
              
              /// Recursive call with exactly the same params
              //TODO: doesn't cover shadowing (of either the params, or the method shadowing/overriding)
              for (
                call @ Apply(Ident(funcCall), funcParams) <- block; 
                if (funcCall.toString == name.toString)
                && (funcParams.forall(_.isInstanceOf[Ident]))
                && (funcParams.map(_.toString).toList == params.map(_.toString).toList)
              ) {
                unit.warning(call.pos, "Possible infinite recursive call. (Except if params are mutable, or the names are shadowed)")
              }
            }
            

            /// Implicit method needs explicit return type
            //if(mods.hasFlag(IMPLICIT) && typeTree.isEmpty && !(name.toString matches "((i?)to).+|.*(To|2)[A-Z].*")) unit.warning(tree.pos, "Implicit method %s needs explicit return type" format name)
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

      //TODO: These probably aleady exist in definitions...
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
          /// Workaround: case class generated code triggers a lot of the checks...
          case ClassDef(mods, _, _, _) if mods.hasFlag(CASE) => return
          /// Workaround: suppresse a null warning and a "remove the if" check for """case class A()""" - see case class unapply's AST)
          case If(Apply(Select(_, nme.EQ), List(Literal(Constant(null)))), Literal(Constant(false)), Literal(Constant(true))) => return
          /// WorkAround: ignores "Assignment right after declaration..." in case class hashcode
          case DefDef(mods, name, _, _, _, Block(block, last)) if name.toString == "hashCode" && {
            (block :+ last) match { 
              case ValDef(modifiers, id1, _, _) :: Assign(id2, _) :: _ => true
              case _ => false
            }} => return

          /// Tracks variable assignment (for unused, vars that could be vals, ...), and checks for self-assignments: a = a
          //TODO: Scala 2.11 will have unused variables and vars->vals behind the -Xlint flag
          case a @ Apply(Select(type1, varSetter), List(Select(type2, varName))) if (type1 equalsStructure type2) && (varSetter.toString == varName.toString+"_$eq") =>
            unit.warning(a.pos, "Assigning a variable to itself?")
          case ValDef(m: Modifiers, varName, TypeTree(), value) if(m.hasFlag(MUTABLE)) =>
            varDecls += varName.toString.trim
            //println("vardecl |"+varName.toString.trim+"|")
          case Apply(Select(a, assign), _) if varDecls.contains(assign.toString.dropRight(4)) => //drop "_$eq" - setter
            val varName = assign.toString.dropRight(4)
            varAssigns += varName.toString
            //println("varassign |"+varName+"|")
          case Assign(l, r) =>
            if(l equalsStructure r) {
              unit.warning(l.pos, "Assigning a variable to itself?")
            } else l match {
              case Ident(varName) => 
                varAssigns += varName.toString
                //println("varassign2 |"+varName+"|")
              case _ => 
                //println(showRaw(l))
            }

          /// Checks if you read from a file without closing it: scala.io.Source.fromFile(file).mkString
          //TODO: Only checks one-liners where you chain it - doesn't actually check if you close it
          case Select(fromFile, _) if fromFile.toString startsWith "scala.io.Source.fromFile" =>
            val warnMsg = "You should close the file stream after use."
            unit.warning(fromFile.pos, warnMsg)
            return
            
          /// Some numeric checks
          case Apply(Select(lhs, nme.EQ), List(rhs))
            if isSubtype(lhs, DoubleClass.tpe) || isSubtype(lhs, FloatClass.tpe) || isSubtype(rhs, DoubleClass.tpe) || isSubtype(rhs, FloatClass.tpe) =>
            
            //unit.warning(tree.pos, "Exact comparison of floating point values is potentially unsafe.")
            
          /// log1p and expm -- see http://www.johndcook.com/blog/2010/06/07/math-library-functions-that-seem-unnecessary/
          //TODO: maybe make checks to protect against potentially wrong fixes, e.g. log1p(a + 1) or log1p(a - 1)
          // also, check 1-exp(x) and other negated versions
          case Apply(log, List(Apply(Select(Literal(Constant(one)), nme.ADD), _))) if log.toString == "scala.math.`package`.log" && one == 1 => 
            unit.warning(tree.pos, "Use math.log1p(x) instead of math.log(1 + x) for added accuracy (if x is near 0")
          case Apply(log, List(Apply(Select(_, nme.ADD), List(Literal(Constant(one)))))) if log.toString == "scala.math.`package`.log" && one == 1 => 
            unit.warning(tree.pos, "Use math.log1p(x) instead of math.log(x + 1) for added accuracy (if x is near 0")
          case Apply(Select(Apply(exp, _), nme.SUB), List(Literal(Constant(one)))) if exp.toString == "scala.math.`package`.exp" && one == 1 => 
            unit.warning(tree.pos, "Use math.expm1(x) instead of math.exp(x) - 1 for added accuracy (if x is near 1).")
          case Apply(Select(Literal(Constant(mone)), nme.ADD), List(Apply(exp, _))) if exp.toString == "scala.math.`package`.exp" && mone == -1 =>
            unit.warning(tree.pos, "Use math.expm1(x) instead of -1 + math.exp(x) for added accuracy (if x is near 1).")

          case Apply(sqrt, List(Apply(pow, List(expr, Literal(Constant(two)))))) if two == 2 && sqrt.toString == "scala.math.`package`.sqrt" && pow.toString == "scala.math.`package`.pow" =>
            unit.warning(tree.pos, "Use abs instead of sqrt(pow(_, 2)).")

          case Apply(math_sqrt, List(Apply(Select(expr1, nme.MUL), List(expr2))))
            if (expr1 equalsStructure expr2) && math_sqrt.toString == "scala.math.`package`.sqrt" =>
            
            unit.warning(tree.pos, "Use abs instead of sqrt(x*x).")

          /// Signum function checks
          case pos @ Apply(Select(expr1, op), List(expr2)) if (op == nme.DIV) && ((expr1, expr2) match {
              case (expr1, Apply(abs, List(expr2))) if abs.toString == "scala.math.`package`.abs" && expr1.toString == expr2.toString => true
              case (Apply(abs, List(expr1)), expr2) if abs.toString == "scala.math.`package`.abs" && expr1.toString == expr2.toString => true
              case (expr1, Select(Apply(wrapper @ Select(_, _), List(expr2)), abs)) if wrapper.toString.endsWith("Wrapper") && abs.toString == "abs" && expr1.toString == expr2.toString => true
              case (Select(Apply(wrapper @ Select(_, _), List(expr1)), abs), expr2) if wrapper.toString.endsWith("Wrapper") && abs.toString == "abs" && expr1.toString == expr2.toString => true
              case _ => false
            }) =>
            
            unit.warning(pos.pos, "Did you mean to use the signum function here? (signum also avoids division by zero errors)")

          /// BigDecimal checks
          // BigDecimal(0.1)
          case Apply(Select(bigDecimal, apply_valueOf), List(c @ Literal(Constant(d:Double))))
            if (bigDecimal.toString == "scala.`package`.BigDecimal" || bigDecimal.toString.endsWith("math.BigDecimal")) && (apply_valueOf.toString matches "apply|valueOf") =>
            
            def warn() = unit.warning(tree.pos, "Possible loss of precision - use a string constant")
            
            //TODO: Scala BigDecimal constructor isn't as bad as the Java one... still fails with 0.555555555555555555555555555
            try {
              val p = c.pos
              //TODO: There must be a less hacky way...
              var token = p.lineContent.substring(p.column -1).takeWhile(_.toString matches "[-+0-9.edfEDF]").toLowerCase
              if(!token.isEmpty) {
                if(token.last == 'f' || token.last == 'd') token = token.dropRight(1)
                //println((token, d))
                if(BigDecimal(token) != BigDecimal(d)) {
                  warn()
                }
              } else {
                warn()
              }
            } catch {
              case e: java.lang.UnsupportedOperationException =>
                // Some trees don't have positions
                warn()
              case e: java.lang.NumberFormatException =>
                warn()
            }
          // new java.math.BigDecimal(0.1)
          case Apply(Select(New(java_math_BigDecimal), nme.CONSTRUCTOR), List(Literal(Constant(d: Double)))) 
            if java_math_BigDecimal.toString == "java.math.BigDecimal" =>
            unit.warning(tree.pos, "Possible loss of precision - use a string constant")
          
          /// Comparing with == on instances of different types: 5 == "5"
          //TODO: Scala 2.10 has a similar check "comparing values of types Int and String using `==' will always yield false"
          case Apply(eqeq @ Select(lhs, nme.EQ), List(rhs)) if methodImplements(eqeq.symbol, Object_==) && !isSubtype(lhs, rhs) && !isSubtype(rhs, lhs) && lhs.tpe.widen.toString != "Null" && rhs.tpe.widen.toString != "Null" =>
            val warnMsg = "Comparing with == on instances of different types (%s, %s) will probably return false."
            unit.warning(eqeq.pos, warnMsg.format(lhs.tpe.widen, rhs.tpe.widen))

          /// Some import checks
          case Import(pkg, selectors) if (pkg.symbol == JavaConversionsModule) && (selectors exists isGlobalImport) =>
            unit.warning(pkg.pos, "Conversions in scala.collection.JavaConversions._ are dangerous.")
          
          //case Import(pkg, selectors) if selectors exists isGlobalImport =>
            //TODO: Too much noise - maybe it would be useful to non-IDE if it printed a nice selector import replacement, e.g. import mutable._ => import mutable.{HashSet,ListBuffer}
            //unit.warning(pkg.pos, "Wildcard imports should be avoided. Favor import selector clauses.")

          /// Collection.contains on different types: List(1,2,3).contains("2")
          case Apply(contains @ Select(seq, _), List(target)) if methodImplements(contains.symbol, SeqLikeContains) && !(target.tpe <:< SeqMemberType(seq.tpe)) =>
            val warnMsg = "%s.contains(%s) will probably return false."
            unit.warning(contains.pos, warnMsg.format(seq.tpe.widen, target.tpe.widen))

          /// Warn about using .asInstanceOf[T]
          //TODO: false positives in case class A(), and in the interpreter init
          //case aa @ Apply(a, List(b @ Apply(s @ Select(instanceOf,dd),ee))) if methodImplements(instanceOf.symbol, AsInstanceOf) =>
            //println((aa,instanceOf))
          //case instanceOf @ Select(a, func) if methodImplements(instanceOf.symbol, AsInstanceOf) =>   
            //TODO: too much noise, maybe detect when it's completely unnecessary
            //unit.warning(tree.pos, "Avoid using asInstanceOf[T] (use pattern matching, type ascription, etc).")

          /// Calling Option.get is potentially unsafe
          //TODO: if(x.isDefined) func(x.get) / if(x.isEmpty) ... else func(x.get), etc. are false positives -- those could be detected in abs-interpreter
          //case get @ Select(_, nme.get) if methodImplements(get.symbol, OptionGet) => 
            //unit.warning(tree.pos, "Calling .get on Option will throw an exception if the Option is None.")

          /// Nag about using null
          //TODO: Too much noise - limit in some way
          case Literal(Constant(null)) =>
            //unit.warning(tree.pos, "Using null is considered dangerous.")

          /// String checks
          case Literal(Constant(str: String)) =>
            /// Repeated string literals
            //TODO: String interpolation gets broken down into parts and causes false positives
            //TODO: a quick benchmark showed string literals are actually more optimized than almost anything else, even final vals
            val threshold = 4

            stringLiteralCount(str) += 1
            if(stringLiteralCount(str) == threshold && !(stringLiteralFileExceptions.contains(unit.source.toString)) && !(str.matches(stringLiteralExceptions))) {
              //TODO: Too much noise :)
              //unit.warning(tree.pos, unit.source.path.toString)
              //unit.warning(tree.pos, """String literal """"+str+"""" appears multiple times.""")
            }
            
            //TODO: Doesn't this already reach the abs-interpreter anyway?
            if(abstractInterpretation.stringVals.exists(_.exactValue == Some(str))) {
              unit.warning(tree.pos, "You have defined that string as a val already, maybe use that?")
              abstractInterpretation.visitedBlocks += tree
            }

          /// Processing a constant string: "hello".size
          case Apply(Select(pos @ Literal(Constant(s: String)), func), params) =>
            func.toString match {
              case "$plus"|"equals"|"$eq$eq"|"toCharArray"|"matches"|"getBytes" => //ignore
              case "length" => unit.warning(pos.pos, "Taking the length of a constant string")
              case _        => unit.warning(pos.pos, "Processing a constant string")
            }
          case Select(Apply(Select(predef, augmentString), List(pos @ Literal(Constant(s: String)))), size)
            if predef.toString == "scala.this.Predef" && augmentString.toString == "augmentString" && size.toString == "size" => 
            unit.warning(pos.pos, "Taking the size of a constant string")

          /// Pattern Matching checks
          case Match(pat, cases) if pat.tpe.toString != "Any @unchecked" && cases.size >= 2 =>
            // Workaround: "Any @unchecked" seems to happen on the matching structures of actors - and all cases return true

            //TODO: Put into utils
            def isLiteral(t:Tree) = t match {
              case Literal(_) => true
              case _ => false
            }

            /// Pattern Matching on a constant value
            //TODO: move to abs-interpreter
            if(isLiteral(pat)) {
              /*val returnValue = 
                cases 
                  .map { ca => (ca.pat, ca.body) } 
                  .find { case (Literal(Constant(c)), _) => c == a; case _ => false}
                  .map { _._2 } 
                  .orElse { if(cases.last.pat.toString == "_") Some(cases.last.body) else None } 
                  .map { s => " will always return " + s }
                  .getOrElse("")*/
              
              unit.warning(tree.pos, "Pattern matching on a constant value.")
            }
            
            /// Checks if pattern matching on Option or Boolean
            var optionCase, booleanCase = false
            val (optionCaseReg, booleanCaseReg) = ("(Some[\\[].*[\\]]|None[.]type)", "Boolean[(](true|false)[)]") //TODO: Hacky hack hack -_-, sorry. use tpe <:< definitions.xxx.tpe
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

            /// Checking for duplicate case bodies
            case class Streak(streak: Int, tree: CaseDef)
            var streak = Streak(0, cases.head)
            def checkStreak(c: CaseDef) {
              if((c.body equalsStructure streak.tree.body) && isLiteral(c.pat) && !(c.body.children == List())) {
                streak = Streak(streak.streak + 1, c)
              } else {
                printStreakWarning()
                streak = Streak(1, c)
              }
            }
            def printStreakWarning() {
              if(streak.streak == cases.size) {
                //This one always turns out to be a false positive :)
                //unit.warning(tree.pos, "All "+cases.size+" cases will return "+cases.head.body+", regardless of pattern value") 
              } else if(streak.streak > 1) {
                //TODO: should check actual cases - only simple values can be merged
                unit.warning(streak.tree.body.pos, streak.streak+" neighbouring cases are identical, and could be merged.")
              }
            }

            for(c <- cases) {
              checkCase(c)
              checkStreak(c)
            }

            printStreakWarning()
            printCaseWarning()

          /// If checks
          case If(cond1, _, If(cond2, _, _)) if cond1 equalsStructure cond2 =>
            unit.warning(cond2.pos, "This else-if has the same condition as the previous if.")

          case Apply(Select(left, func), List(right)) if (func.toString matches "[$]amp[$]amp|[$]bar[$]bar") && (left equalsStructure right) =>        
            unit.warning(tree.pos, "Same expression on both sides of condition.")
          case Apply(Select(left, func), List(right)) if (func.toString matches "[$](greater|less|eq|bang)([$]eq)?") && (left equalsStructure right) =>
            if(left.tpe <:< definitions.DoubleClass.tpe || left.tpe <:< definitions.FloatClass.tpe) {
              /// Use variable.isNaN instead of (variable != variable)
              unit.warning(tree.pos, "Same expression on both sides of comparison. (Use .isNan instead)")
            } else {
              unit.warning(tree.pos, "Same expression on both sides of comparison.")
            }

          /// Yoda conditions (http://www.codinghorror.com/blog/2012/07/new-programming-jargon.html): if(6 == a) ...
          case Apply(Select(Literal(Constant(const)), func), params) if (params.size) == 1 && (func.toString matches "[$](greater|less|eq)([$]eq)?") && (params.head match { case Literal(_) => false case _ => true })  =>
            unit.warning(tree.pos, "You are using Yoda conditions")

          case If(cond, Literal(Constant(true)), Literal(Constant(false))) =>
            unit.warning(cond.pos, "Remove the if and just use the condition.")
          case If(cond, Literal(Constant(false)), Literal(Constant(true))) =>
            unit.warning(cond.pos, "Remove the if and just use the negated condition.")
          case If(cond, a, b) if (a equalsStructure b) && (a.children.nonEmpty) => //TODO: empty if statement (if(...) { }) triggers this - issue warning for that case?
            //TODO: test if a single statement counts as children.nonEmpty
            unit.warning(a.pos, "If statement branches have the same structure.")
          case If(cond, a, If(cond2, b, c)) if (a.children.nonEmpty && ((a equalsStructure b) || (a equalsStructure c))) || (b.children.nonEmpty && (b equalsStructure c)) =>
            unit.warning(a.pos, "If statement branches have the same structure.")

          //Ignore: ignores while(true)... I mean, one can't accidentally use while(true), can they? :)
          /*case LabelDef(whileName, List(), If(cond @ Literal(Constant(a: Boolean)), _, _)) =>
            //TODO: doesn't actually ignore, but that test is trivial anyway, commenting both
          
          case If(cond @ Literal(Constant(a: Boolean)), _, _) => 
            //TODO: there are people still doing breakable { while(true) {... don't warn on while(true)?
            val warnMsg = "This condition will always be "+a+"."
            unit.warning(cond.pos, warnMsg)*/
          case Apply(Select(Literal(Constant(false)), term), _) if term == nme.ZAND =>
            val warnMsg = "This part of boolean expression will always be false."
            unit.warning(tree.pos, warnMsg)
          case Apply(Select(Literal(Constant(true)), term), _) if term == nme.ZOR =>
            val warnMsg = "This part of boolean expression will always be true."
            unit.warning(tree.pos, warnMsg)
            
          case If(_, If(_, body, Literal(Constant(()))), Literal(Constant(()))) =>
            unit.warning(tree.pos, "These two ifs can be merged into one.")
          
          
          /// Abstract interpretation, and multiple-statement checks
          case ClassDef(mods, name, tparams, impl) =>
            abstractInterpretation.traverseBlock(impl)

          case ModuleDef(mods, name, impl) => 
            abstractInterpretation.traverseBlock(impl)

          case Function(params, body) =>
            abstractInterpretation.traverseBlock(body)
          
          case blockElem @ Block(init, last) =>
            abstractInterpretation.traverseBlock(blockElem)

            val block = init :+ last
            //TODO: var v; ...non v related stuff...; v = 4 <-- this is the same thing, really
            
            /// Checks on two subsequent statements
            (block zip block.tail) foreach { 
              case (ValDef(modifiers, id1, _, _), Assign(id2, assign)) if id1.toString == id2.toString && !abstractInterpretation.isUsed(assign, id2.toString)=>
                unit.warning(id2.pos, "Assignment right after declaration is most likely a bug (unless you side-effect like a boss)")

              //case (Assign(id1, _), Assign(id2, _)) if id1.toString == id2.toString => // stricter
              case (Assign(id1, _), Assign(id2, assign)) if id1.toString == id2.toString && !abstractInterpretation.isUsed(assign, id2.toString) =>
                unit.warning(id2.pos, "Two subsequent assigns are most likely a bug (unless you side-effect like a boss)")

              case (Assign(id1, id2), Assign(id2_, id1_)) if(id1 equalsStructure id1_) && (id2 equalsStructure id2_) =>
                unit.warning(id1_.pos, "Did you mean to swap these two variables?")

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

          /// find(...).isDefined is better written as exists(...)
          case Select(Apply(pos @ Select(collection, find), func), isDefined) 
            if find.toString == "find" && isDefined.toString == "isDefined" && (collection.toString.startsWith("scala.") || collection.toString.startsWith("immutable.")) =>
            
            unit.warning(pos.pos, "Use exists(...) instead of find(...).isDefined")

          /// flatMap(if() x else Nil/None) is better written as filter(...)
          case Apply(TypeApply(Select(collection, flatMap), _), List(Function(List(ValDef(flags, param, _, _)), If(cond, e1, e2))))
            if flatMap.toString == "flatMap" =>

            // swap branches, to save some copy-pasting of huge patterns below
            val (expr1, expr2) = if((e1.toString endsWith ".Nil") || (e1.toString endsWith ".None")) (e1, e2) else (e2, e1)

            (expr1, expr2) match {
              case (nil,Apply(TypeApply(Select(collection, apply), _), List(Ident(id))))
                if ((collection.toString startsWith "scala.collection.immutable.") || (collection.toString startsWith "immutable."))
                && (nil.toString endsWith ".Nil") 
                && (id.toString == param.toString) =>
                
                unit.warning(tree.pos, "Use filter(x => cond) instead of this flatMap(x => if(cond) ... else ...)")

              //TODO: this check makes scalac 2.9 pattern matcher explode
              /*case (Apply(option2Iterable1, List(none)),Apply(option2Iterable2, List(Apply(TypeApply(Select(some, apply), _), List(Ident(id))))))
                if (none.toString == "scala.None")
                && (some.toString == "scala.Some")
                && (id.toString == param.toString) =>
                
                unit.warning(tree.pos, "Use filter(x => cond) instead of this flatMap(x => if(cond) ... else ...)")*/
                
              case a => 
                //println((expr1, expr2))
                //println((showRaw(expr1), showRaw(expr2)))
            }

          case forloop @ Apply(TypeApply(Select(collection, foreach_map), _), List(Function(List(ValDef(_, param, _, _)), body))) if { //if (foreach_map.toString matches "foreach|map") && {
            abstractInterpretation.forLoop(forloop)
            false
          } => //

          case DefDef(_, _, _, _, _, block) => 
            abstractInterpretation.traverseBlock(block)

          //TODO: these two are probably superdeded by abs-interpreter
          case pos @ Apply(Select(seq, apply), List(Literal(Constant(index: Int)))) 
            if methodImplements(pos.symbol, SeqLikeApply) && index < 0 =>
            unit.warning(pos.pos, "Using a negative index for a collection.")

          case divByZero @ Apply(Select(rcvr, op), List(Literal(Constant(zero))))
            // cannot check double/float, as typer will automatically translate it to Infinity
            if (zero == 0) && (op == nme.DIV || op == nme.MOD) 
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
        //if(maybeVals.nonEmpty) unit.warning(tree.pos, "[experimental] These vars might secretly be vals: grep -rnP --include=*.scala 'var ([(][^)]*)?("+maybeVals.mkString("|")+")'")
        varDecls.clear
      }
    }
  }
}
