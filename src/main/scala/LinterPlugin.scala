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
import collection.mutable

class LinterPlugin(val global: Global) extends Plugin {
  import global._
  import Utils._

  val name = "linter"
  val description = "a static analysis compiler plugin"
  val components = List[PluginComponent](PreTyperComponent, PostTyperComponent, PostRefChecksComponent)
  
  override val optionsHelp: Option[String] = Some("  -P:linter No options yet, just letting you know I'm here")

  val inferred = mutable.HashSet[Position]() // Used for a scala 2.9 hack (can't find out which types are inferred)

  private object PreTyperComponent extends PluginComponent {
    val global = LinterPlugin.this.global
    import global._
    
    override val runsAfter = List("parser")
    
    val phaseName = "linter-parsed"
    
    private val sealedTraits = mutable.Map[Name, Tree]()
    private val usedTraits = mutable.Set[Name]()
    private var inTrait = false
    private def resetTraits() {
      sealedTraits.clear()
      usedTraits.clear()
      inTrait = false
    }
    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: global.CompilationUnit) {
        if(!unit.isJava) {
          resetTraits()
          new PreTyperTraverser(unit).traverse(unit.body)
          //println((sealedTraits, usedTraits))
          for(unusedTrait <- sealedTraits.filterNot(st => usedTraits.exists(_.toString == st._1.toString))) {
            //TODO: It might still be used in some type-signature somewhere... see scalaz
            warn(unusedTrait._2.pos, "This sealed trait is never extended.")(unit)
          }
        }
      }
    }
    
    class PreTyperTraverser(unit: CompilationUnit) extends Traverser {
      implicit val unitt = unit

      override def traverse(tree: Tree) {
        //if(showRaw(tree).contains("Hello"))println(showRaw(tree))
        tree match {
          /// Unused sealed traits (Idea by edofic)
          case ClassDef(mods, name, _, Template(extendsList, _, body)) if !mods.isSealed && mods.isTrait =>
            for(Ident(traitName) <- extendsList) usedTraits += traitName
            inTrait = true
            for(stmt <- body) traverse(stmt)
            inTrait = false
            return

          // typeparams (3rd param) are sometimes used for type-checking hacks
          case ClassDef(mods, name, List(), Template(extendsList, _, body)) if mods.isSealed && mods.isTrait && !inTrait =>
            sealedTraits += name -> tree
            for(Ident(traitName) <- extendsList if traitName.toString != name.toString) usedTraits += traitName
            for(stmt <- body) traverse(stmt)
            return
            
          case ClassDef(mods, _, _, Template(extendsList, _, body)) => //if mods.hasFlag(CASE) => 
            for(Ident(traitName) <- extendsList) usedTraits += traitName
            for(stmt <- body) traverse(stmt)
            return
            
          case ModuleDef(mods, name, Template(extendsList, _, body)) =>
            for(Ident(traitName) <- extendsList) usedTraits += traitName
            for(stmt <- body) traverse(stmt)
            return

          
          /// Warn on Nothing/Any or M[Nothing/Any] (idea by OlegYch)
          case ValDef(mods, _, tpe, _) 
            if !mods.isParameter 
            && tpe.toString == "<type ?>" =>
            
            inferred += tpe.pos

          //case DefDef(mods: Modifiers, name, _, valDefs, typeTree, body) =>
            //if(name.toString != "<init>" && !body.isEmpty && !mods.isAnyOverride) {
              /// Recursive call with exactly the same params
              //TODO: Currenlty doesn't cover shadowing or mutable changes of params, or the method shadowing/overriding
              /*for (
                call @ Apply(Ident(funcCall), funcParams) <- body; 
                if (funcCall.toString == name.toString)
                && (funcParams.forall(_.isInstanceOf[Ident]))
                && (funcParams.map(_.toString).toList == params.map(_.toString).toList)
              ) warn(call, "Possible infinite recursive call. (Except if params are mutable, or the names are shadowed)")
              */
            //}

            /// Implicit method needs explicit return type
            //TODO: I'd add a bunch of exceptions for when the return type is actually clear:
            // when name includes name in a clear way
            // when the body is just new Type, or Type.apply(...): Type
            /*if(mods.isImplicit && typeTree.isEmpty && !(name.toString matches "((i?)to).+|.*(To|2)[A-Z].*")) {
              warn(tree, "Implicit method %s needs explicit return type" format name)
            }*/
          case _ => 
        }
        super.traverse(tree)
      }
    }
  }

  private object PostTyperComponent extends PluginComponent {
    val global = LinterPlugin.this.global
    import global._

    override val runsAfter = List("typer")

    val phaseName = "linter-typed"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: global.CompilationUnit) {
        if(!unit.isJava) {
          new PostTyperTraverser(unit).traverse(unit.body)
        }
      }
    }

    class PostTyperTraverser(unit: CompilationUnit) extends Traverser {
      implicit val unitt = unit
      import definitions.{AnyClass, NothingClass, ObjectClass, Object_==}
      import definitions.{OptionClass, SeqClass, TraversableClass, ListClass, StringClass}
      import definitions.{DoubleClass, FloatClass, CharClass, ByteClass, ShortClass, IntClass, LongClass, BooleanClass}
      
      val JavaConversionsModule: Symbol = definitions.getModule(newTermName("scala.collection.JavaConversions"))
      val SeqLikeClass: Symbol = definitions.getClass(newTermName("scala.collection.SeqLike"))
      val SeqLikeContains: Symbol = SeqLikeClass.info.member(newTermName("contains"))
      val SeqLikeApply: Symbol = SeqLikeClass.info.member(newTermName("apply"))

      val OptionGet: Symbol = OptionClass.info.member(nme.get)
      
      val IsInstanceOf = AnyClass.info.member(nme.isInstanceOf_)
      val AsInstanceOf = AnyClass.info.member(nme.asInstanceOf_)
      val ToString: Symbol = AnyClass.info.member(nme.toString_)

      def SeqMemberType(seenFrom: Type): Type = {
        SeqLikeClass.tpe.typeArgs.head.asSeenFrom(seenFrom, SeqLikeClass)
      }

      def isSubtype(x: Tree, y: Tree): Boolean = { x.tpe.widen <:< y.tpe.widen }
      def isSubtype(x: Tree, y: Type): Boolean = { x.tpe.widen <:< y.widen }
      
      def isIntegerType(tpe: Type): Boolean =
          (tpe <:< ByteClass.tpe
        || tpe <:< ShortClass.tpe
        || tpe <:< IntClass.tpe
        || tpe <:< LongClass.tpe)
      def isIntegerType(x: Tree): Boolean = isIntegerType(x.tpe.widen)

      def isFloatingType(tpe: Type): Boolean =
          (tpe <:< FloatClass.tpe 
        || tpe <:< DoubleClass.tpe)
      def isFloatingType(x: Tree): Boolean = isFloatingType(x.tpe.widen)
        
      def methodImplements(method: Symbol, target: Symbol): Boolean = {
        method == target || method.allOverriddenSymbols.contains(target)
      }

      def isGlobalImport(selector: ImportSelector): Boolean = {
        selector.name == nme.WILDCARD && selector.renamePos == -1
      }
      
      def isOptionOption(t: Tree): Boolean = 
        (t.tpe.widen.baseClasses.exists(_.tpe =:= OptionClass.tpe) 
        && t.tpe.widen.typeArgs.exists(_.widen.baseClasses.exists(_.tpe =:= OptionClass.tpe)))
      
      def isLiteral(t:Tree): Boolean = t match {
        case Literal(_) => true
        case _ => false
      }
      
      def getUsed(tree: Tree): Set[String] = (for(Ident(id) <- tree) yield id.toString).toSet
      def getAssigned(tree: Tree): Set[String] = {
        (for(Assign(Ident(id), _) <- tree) yield id.toString).toSet
        //TODO: non-local stuff (for(Apply(Select(id, setter), List(_)) <- tree; if setter.toString endsWith "_$eq") yield setter.dropRight(4)).toSet
      }

      def hasIntegerDivision(tree: Tree): Boolean = { //dat code soup... also still possible to have false positives... f = list(1/i), where list: List[Int]
        def hasIntDiv: Boolean = tree exists {
          case Apply(Select(param1, nme.DIV), List(param2)) if isIntegerType(param1) && isIntegerType(param2) => true
          case _ => false
        }
        def isFloatConversion: Boolean = tree match { // for Int expressions... .toDouble is implicitly added
          case Select(num, toFloat) if (toFloat.toString matches "to(Float|Double)") && isIntegerType(num) => true
          case _ => false
        }
        def hasIntOpFloat: Boolean = tree exists { // detect implicit widening, e.g. float + int
          case Apply(Select(param1, op), List(param2)) 
            if (isIntegerType(param1) && isFloatingType(param2)) || (isFloatingType(param1) && isIntegerType(param2)) => true
          case _ => false
        }

        hasIntDiv && (isFloatConversion || hasIntOpFloat)
      }
  
      // Just a way to make the Tree/Name-String comparisons more readable
      abstract class RichToStr[T](n: T) {
        def is(str: String): Boolean = n.toString == str
        def isAny(str: String*): Boolean = str.exists(n.toString == _)
        def startsWith(str: String): Boolean = n.toString startsWith str
        def startsWithAny(str: String*): Boolean = str.exists(n.toString startsWith _)
        def endsWith(str: String): Boolean = n.toString endsWith str
        def endsWithAny(str: String*): Boolean = str.exists(n.toString endsWith _)
      }
      // (scala 2.9 implicit class)
      implicit def richTree(n: Tree): RichToStr[Tree] = new RichToStr(n) {}
      implicit def richName(n: Name): RichToStr[Name] = new RichToStr(n) {}
      
      // Returns the string subtree of a string.length/size subtree
      def getStringFromLength(t: Tree): Option[Tree] = t match {
        case Apply(Select(str, length), List()) 
          if str.tpe <:< StringClass.tpe
          && (length is "length") =>
          
          Some(str)
          
        case Select(Apply(scala_Predef_augmentString, List(str)), size)
          if str.tpe <:< StringClass.tpe 
          && (size is "size") && (scala_Predef_augmentString is "scala.this.Predef.augmentString") =>
          
          Some(str)
          
        case _ =>
          None
      }
      
      val abstractInterpretation = new AbstractInterpretation(global)

      override def traverse(tree: Tree) { 
        //TODO: the matchers are broken up for one reason only - Scala 2.9 pattern matcher :)
        //abstractInterpretation.traverseBlock(tree)
        ///Workarounds:
        tree match {
          /// Workaround: case class generated code triggers a lot of the checks...
          case ClassDef(mods, _, _, _) if mods.isCase => return
          /// Workaround: suppresse a null warning and a "remove the if" check for """case class A()""" - see case class unapply's AST)
          case If(Apply(Select(_, nme.EQ), List(Literal(Constant(null)))), Literal(Constant(false)), Literal(Constant(true))) => return
          /// WorkAround: ignores "Assignment right after declaration..." in case class hashcode
          case DefDef(mods, name, _, _, _, Block(block, last)) if (name is "hashCode") && {
            (block :+ last) match { 
              case ValDef(modifiers, id1, _, _) :: Assign(id2, _) :: _ => true
              case _ => false
            }} => return
          case _ =>
        }
        tree match {
          /// Some numeric checks
          /*case Apply(Select(lhs, nme.EQ), List(rhs))
            if isSubtype(lhs, DoubleClass.tpe) || isSubtype(lhs, FloatClass.tpe) || isSubtype(rhs, DoubleClass.tpe) || isSubtype(rhs, FloatClass.tpe) =>
            
            warn(tree, "Exact comparison of floating point values is potentially unsafe.")*/
            
          /// log1p and expm -- see http://www.johndcook.com/blog/2010/06/07/math-library-functions-that-seem-unnecessary/
          //TODO: maybe make checks to protect against potentially wrong fixes, e.g. log1p(a + 1) or log1p(a - 1)
          // also, check 1-exp(x) and other negated versions
          case Apply(log, List(Apply(Select(Literal(Constant(1)), nme.ADD), _))) if log is "scala.math.`package`.log" => 
            warn(tree, "Use math.log1p(x) instead of math.log(1 + x) for added accuracy when x is near 0")
          case Apply(log, List(Apply(Select(_, nme.ADD), List(Literal(Constant(1)))))) if log is "scala.math.`package`.log" => 
            warn(tree, "Use math.log1p(x) instead of math.log(x + 1) for added accuracy when x is near 0")
            
          case Apply(Select(Apply(exp, _), nme.SUB), List(Literal(Constant(1)))) if exp is "scala.math.`package`.exp" => 
            warn(tree, "Use math.expm1(x) instead of math.exp(x) - 1 for added accuracy when x is near 0.")
          case Apply(Select(Literal(Constant(-1)), nme.ADD), List(Apply(exp, _))) if exp is "scala.math.`package`.exp" =>
            warn(tree, "Use math.expm1(x) instead of -1 + math.exp(x) for added accuracy when x is near 0.")

          /// Use abs instead of doing it manually
          case Apply(sqrt, List(Apply(pow, List(expr, Literal(Constant(2))))))
            if (sqrt is "scala.math.`package`.sqrt") && (pow is "scala.math.`package`.pow") =>
            
            warn(tree, "Use abs instead of sqrt(pow(_, 2)).")

          case Apply(math_sqrt, List(Apply(Select(expr1, nme.MUL), List(expr2))))
            if (expr1 equalsStructure expr2) && (math_sqrt is "scala.math.`package`.sqrt") =>
            
            warn(tree, "Use abs instead of sqrt(x*x).")

          /// Use xxx.isNaN instead of (xxx != xxx)
          case Apply(Select(left, func), List(right))
            if isFloatingType(left)
            && (func == nme.EQ || func == nme.NE)
            && ((left equalsStructure right) || (right equalsStructure Literal(Constant(Double.NaN))) || (right equalsStructure Literal(Constant(Float.NaN)))) =>
            
            if(left equalsStructure right) 
              warn(tree, "Use .isNan instead of comparing to itself.")
            else
              warn(tree, "Use .isNan instead of comparing to NaN, which is wrong.")

          /// Signum function checks
          case pos @ Apply(Select(expr1, nme.DIV), List(expr2)) if ((expr1, expr2) match {
              case (expr1, Apply(abs, List(expr2))) if (abs is "scala.math.`package`.abs") && (expr1 equalsStructure expr2) => true
              case (Apply(abs, List(expr1)), expr2) if (abs is "scala.math.`package`.abs") && (expr1 equalsStructure expr2) => true
              case (expr1, Select(Apply(wrapper, List(expr2)), abs)) if (wrapper endsWith "Wrapper") && (abs is "abs") && (expr1 equalsStructure expr2) => true
              case (Select(Apply(wrapper, List(expr1)), abs), expr2) if (wrapper endsWith "Wrapper") && (abs is "abs") && (expr1 equalsStructure expr2) => true
              case _ => false
            }) =>
            
            warn(pos, "Did you mean to use the signum function here? (signum also avoids division by zero errors)")
            
          case _ => 
        }
        
        tree match {
          /// BigDecimal checks
          // BigDecimal(0.1) //TODO: someday move to interpreter - detect this for known values
          case Apply(Select(bigDecimal, apply_valueOf), (c @ Literal(Constant(literal: Double))) :: context)
            if ((bigDecimal is "scala.`package`.BigDecimal") || (bigDecimal endsWith "math.BigDecimal")) && (apply_valueOf isAny ("apply", "valueOf")) =>
            
            val warnMsg = "Possible loss of precision - use a string constant"
            
            try {
              val p = c.pos
              //TODO: There must be a less hacky way...
              var token = p.lineContent.substring(p.column -1).takeWhile(_.toString matches "[-+0-9.edfEDF]").toLowerCase
              if(!token.isEmpty) {
                if(token.last == 'f' || token.last == 'd') token = token.dropRight(1)
                //println((token, literal))
                if(BigDecimal(token) != BigDecimal(literal)) warn(tree, warnMsg)
              } else {
                warn(tree, warnMsg)
              }
            } catch {
              case e: java.lang.UnsupportedOperationException =>
                // Some trees don't have positions
                warn(tree, warnMsg)
              case e: java.lang.NumberFormatException =>
                warn(tree, warnMsg)
            }
          case Apply(Select(bigDecimal, apply_valueOf), (c @ Literal(Constant(literal: String))) :: context)
            if ((bigDecimal is "scala.`package`.BigDecimal") || (bigDecimal endsWith "math.BigDecimal")) && (apply_valueOf isAny ("apply", "valueOf")) =>
            
            val (warnMsg, mathContext) = 
              if(context.size == 1 && context.head.tpe <:< definitions.getClass(newTermName("java.math.MathContext")).tpe) {
                import java.math.MathContext
                def getMathContext(t: Tree): Option[MathContext] = t match {
                  case Apply(Select(New(mc), nme.CONSTRUCTOR), (Literal(Constant(precision: Int)) :: roundingMode)) =>
                    Some(new MathContext(precision)) // I think roundingMode is irrelevant, because the check will warn anyway
                  case _ =>
                    None
                }
                
                ("Possible loss of precision - use a larger MathContext", getMathContext(context.head))
              } else {
                ("Possible loss of precision - add a custom MathContext", None)
              }
            
            try {
              // Ugly hack to get around a few different representations: 0.005, 5E-3, ...
              def crop(s: String): String = s.replaceAll("[.]|[Ee].*$|(0[.])?0*", "")
              
              val bd = if(mathContext.isDefined) BigDecimal(literal, mathContext.get) else BigDecimal(literal)
              if(crop(bd.toString) != crop(literal)) warn(tree, warnMsg)
            } catch {
              case e: java.lang.NumberFormatException =>
                warn(tree, "This BigDecimal constructor will likely throw a NumberFormatException.")
            }

          // new java.math.BigDecimal(0.1)
          case Apply(Select(New(java_math_BigDecimal), nme.CONSTRUCTOR), List(Literal(Constant(d: Double)))) 
            if java_math_BigDecimal is "java.math.BigDecimal" =>
            
            warn(tree, "Possible loss of precision - use a string constant")
          
          case _ =>
        }
        
        tree match {
          /// Checks for self-assignments: a = a
          case Assign(left, right) if left equalsStructure right =>
            warn(left, "Assigning a variable to itself?")
          case Apply(Select(type1, varSetter), List(Select(type2, varName))) 
            if (type1 equalsStructure type2) && (varSetter.toString == varName.toString+"_$eq") =>
            warn(type1, "Assigning a variable to itself?")

          /// Checks if you read from a file without closing it: scala.io.Source.fromFile(file).mkString
          //TODO: Only checks one-liners where you chain it - doesn't actually check if you close it
          //TODO: Possibly skipps checking code in higher order functions later on
          case Select(fromFile, _) if fromFile startsWith "scala.io.Source.fromFile" =>
            warn(fromFile, "You should close the file stream after use.")
            return
            
          /// Comparing with == on instances of different types: 5 == "5"
          //TODO: Scala 2.10 has a similar check "comparing values of types Int and String using `==' will always yield false"
          case Apply(eqeq @ Select(lhs, nme.EQ), List(rhs))
            if methodImplements(eqeq.symbol, Object_==)
            && !isSubtype(lhs, rhs) && !isSubtype(rhs, lhs)
            && lhs.tpe.widen.toString != "Null" && rhs.tpe.widen.toString != "Null" =>
            
            val warnMsg = "Comparing with == on instances of different types (%s, %s) will probably return false."
            warn(eqeq, warnMsg.format(lhs.tpe.widen, rhs.tpe.widen))

          /// Some import checks
          case Import(pkg, selectors) if (pkg.symbol == JavaConversionsModule) && (selectors exists isGlobalImport) =>
            warn(pkg, "Implicit conversions in collection.JavaConversions are dangerous. Consider using collection.JavaConverters")
          
          //case Import(pkg, selectors) if selectors exists isGlobalImport =>
            //TODO: Too much noise - maybe it would be useful to non-IDE users if it printed 
            // a nice selector import replacement, e.g. import mutable._ => import mutable.{HashSet,ListBuffer}
            
            //warn(pkg, "Wildcard imports should be avoided. Favor import selector clauses.")

          /// Collection.contains on different types: List(1,2,3).contains("2")
          case Apply(contains @ Select(seq, _), List(target)) 
            if methodImplements(contains.symbol, SeqLikeContains) 
            && !(target.tpe <:< SeqMemberType(seq.tpe)) =>
            
            warn(contains, "%s.contains(%s) will probably return false.".format(seq.tpe.widen, target.tpe.widen))

          /// Warn about using .asInstanceOf[T]
          //TODO: false positives in case class A(), and in the interpreter init
          //case aa @ Apply(a, List(b @ Apply(s @ Select(instanceOf,dd),ee))) if methodImplements(instanceOf.symbol, AsInstanceOf) =>
            //println((aa,instanceOf))
          //case instanceOf @ Select(a, func) if methodImplements(instanceOf.symbol, AsInstanceOf) =>   
            //TODO: too much noise, maybe detect when it's completely unnecessary
            //warn(tree, "Avoid using asInstanceOf[T] (use pattern matching, type ascription, etc).")

          /// Calling Option.get is potentially unsafe
          //TODO: if(x.isDefined) func(x.get) / if(x.isEmpty) ... else func(x.get), etc. are false positives -- those could be detected in abs-interpreter
          //case get @ Select(_, nme.get) if methodImplements(get.symbol, OptionGet) => 
            //warn(tree, "Calling .get on Option will throw an exception if the Option is None.")

          /// Nag about using null
          //TODO: Too much noise - limit in some way
          //case Literal(Constant(null)) =>
            //warn(tree, "Using null is considered dangerous.")

          /// String checks
          /*case Literal(Constant(str: String)) =>
            /// Repeated string literals
            //TODO: String interpolation gets broken down into parts and causes false positives
            //TODO: a quick benchmark showed string literals are actually more optimized than almost anything else, even final vals
            val threshold = 4

            stringLiteralCount(str) += 1
            if(stringLiteralCount(str) == threshold && !(stringLiteralFileExceptions.contains(unit.source.toString)) && !(str.matches(stringLiteralExceptions))) {
              //TODO: Too much noise :)
              warn(tree, """String literal """"+str+"""" appears multiple times.""")
            }*/
            
          /// str.substring("sdfsdf".length) -> str.stripPrefix("sdfsdf")
          //TODO: not an exact replacement, also performance questions
          // maybe .drop("sdfsdf".length)
          /*case Apply(Select(str, substring), List(strLen))
            if str.tpe <:< definitions.StringClass.tpe            
            && substring.toString == "substring"
            && getStringFromLength(strLen).isDefined =>
            
            warn(tree, "Use x.stripPrefix(y), instead of x.substring(y.length)")
          */  
          /// str.substring(0, str.length - "sdfsdf".length) -> str.stripPrefix("sdfsdf")
          //TODO: not an exact replacement, also performance questions
          // maybe .dropRight("sdfsdf".length)
          /*
          case Apply(Select(str1, substring), List(Literal(Constant(0)), Apply(Select(str1Len, nme.SUB), List(str2Len))))
            if str1.tpe <:< definitions.StringClass.tpe
            && substring.toString == "substring"
            && getStringFromLength(str1Len).isDefined && getStringFromLength(str2Len).isDefined
            && (str1 equalsStructure getStringFromLength(str1Len).get) =>
          
            warn(tree, "Use x.stripSuffix(y), instead of x.substring(x.length-y.length)")
          */
          /// Processing a constant string: "hello".size
          /*case Apply(Select(pos @ Literal(Constant(s: String)), func), params) =>
            func.toString match {
              case "$plus"|"equals"|"$eq$eq"|"toCharArray"|"matches"|"getBytes" => //ignore
              case "length" => warn(pos, "Taking the length of a constant string")
              case _        => warn(pos, "Processing a constant string")
            }
          case Select(Apply(Select(predef, augmentString), List(pos @ Literal(Constant(s: String)))), size)
            if predef is "scala.this.Predef" && augmentString.toString == "augmentString" && size.toString == "size" => 
            warn(pos, "Taking the size of a constant string")
          */
          /// Pattern Matching checks
          case Match(pat, cases) if (pat match { case Typed(_, _) => false; case _ => true }) && pat.tpe.toString != "Any @unchecked" && cases.size >= 2 =>
            // Workaround: "Any @unchecked" seems to happen on the matching structures of actors - and all cases return true
            // Workaround: Typed (or Annotated) seems to happen in for loop pattern matching, which doesn't work right for at least for checkUsage

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
              
              warn(tree, "Pattern matching on a constant value.")
            }
            
            /// Checks if pattern matching on Option or Boolean
            var optionCase, booleanCase = false
            //TODO: Hacky hack hack -_-, sorry. use tpe <:< definitions.xxx.tpe
            val (optionCaseReg, booleanCaseReg) = ("(Some[\\[].*[\\]]|None[.]type)", "Boolean[(](true|false)[)]")
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
                  //see: http://blog.tmorris.net/posts/scalaoption-cheat-sheet/
                  //TODO: too much noise, and some cases are perfectly fine - try detecting all the exact cases from link
                  //warn(tree, "There are probably better ways of handling an Option.")
                } else if(booleanCase) {
                  //TODO: case something => ... case _ => ... is also an if in a lot of cases
                  warn(tree, "This is probably better written as an if statement.")
                }
              }
            }

            /// Checking for duplicate case bodies
            // only if isLiteral(c.pat), because other types can't easily be merged
            case class Streak(streak: Int, tree: CaseDef)
            var streak = Streak(0, cases.head)
            def checkStreak(c: CaseDef) {
              if((c.body equalsStructure streak.tree.body) && isLiteral(c.pat) && (c.guard == EmptyTree && streak.tree.guard == EmptyTree) && (c.body != EmptyTree)) {
                streak = Streak(streak.streak + 1, c)
              } else {
                printStreakWarning()
                streak = Streak(1, c)
              }
            }
            def printStreakWarning() {
              if(streak.streak == cases.size) {
                //This one always turns out to be a false positive :)
                //warn(tree, "All "+cases.size+" cases will return "+cases.head.body+", regardless of pattern value") 
              } else if(streak.streak > 1) {
                warn(streak.tree.body, "Bodies of "+streak.streak+" neighbouring cases are identical, and could be merged.")
              }
            }
            
            /// Checking for unused variables in pattern matching
            def checkUsage(c: CaseDef) {
              //TODO: use for self-testing from time to time - ~100 warnings currently :/
              /*val binds = for(b @ Bind(name, _) <- c.pat; if !name.toString.startsWith("_")) yield (b, name.toString)
              for(unused <- binds.filter { case (b, name) => !abstractInterpretation.isUsed(c, name)}) {
                println(showRaw(pat))
                warn(unused._1, "Unused value in pattern matching, use _ instead. (or prefix with _ to get rid of me)")
              }*/
            }

            /// Detect unreachable cases 
            //TODO: move to abs. interpreter to detect impossible guards
            //TODO: if there is a case (x,y) without a guard, it will make a latter case (x,y) with a guard unreachable
            val pastCases = mutable.ListBuffer[CaseDef]()
            def checkUnreachable(c: CaseDef) {
              //adapted from scala/reflect/internal/Trees.scala to cover wildcards in CaseDef
              def correspondsWildcardStructure(thiz: CaseDef, that: CaseDef): Boolean = {
                val wildcards = mutable.HashSet[(Name, Name)]()//enumerate wildcard aliases
                
                def correspondsStructure(thiz: Tree, that: Tree): Boolean = {
                  (thiz eq that) || ((thiz.productArity == that.productArity) && {
                    def equals0(this0: Any, that0: Any): Boolean = (this0, that0) match {
                      case (x: Name, y: Name) if wildcards.contains((x, y)) => 
                        true
                      case (x: Tree, y: Tree) => 
                        (x eq y) || correspondsStructure(x, y)
                      case (xs: List[_], ys: List[_]) => 
                        (xs corresponds ys)(equals0)
                      case _ => 
                        this0 == that0
                    }
                    def compareOriginals(): Boolean = (thiz, that) match {
                      case (x: TypeTree, y: TypeTree) if x.original != null && y.original != null =>
                        correspondsStructure(x.original, y.original)
                      case _ =>
                        true
                    }
                    
                    ((thiz, that) match {
                      case (b1 @ Bind(x, Ident(nme.WILDCARD)), b2 @ Bind(y, Ident(nme.WILDCARD))) =>
                        if(b1.tpe =:= b2.tpe) {
                          wildcards += ((x, y))
                          true
                        } else {
                          false
                        }
                      case _ =>
                        thiz.productIterator zip that.productIterator forall { case (x, y) => equals0(x, y) }
                    }) && compareOriginals()
                  })
                }
                
                (correspondsStructure(thiz.pat, that.pat) && correspondsStructure(thiz.guard, that.guard))
              }

              if(pastCases exists { p => correspondsWildcardStructure(p, c) })
                warn(c.pos, "Identical case detected above - this will never match.")
              else
                pastCases += c
            }

            for(c <- cases) {
              checkCase(c)
              checkStreak(c)
              checkUsage(c)
              checkUnreachable(c)
            }
            
            printStreakWarning()
            printCaseWarning()

          /// If checks
          case Apply(Select(left, func), List(right)) 
            if (func == nme.ZAND || func == nme.ZOR) 
            && (left.symbol == null || !left.symbol.isMethod)
            && (left equalsStructure right) 
            && tree.tpe.widen <:< BooleanClass.tpe =>
            
            warn(tree, "Same expression on both sides of boolean statement.")
           
          /// Same expression on both sides of comparison.
          case Apply(Select(left, func), List(right)) 
            if (func.toString matches "[$](greater|less|eq|bang)([$]eq)?") && (left equalsStructure right) =>
            
            warn(tree, "Same expression on both sides of comparison.")

          /// Yoda conditions (http://www.codinghorror.com/blog/2012/07/new-programming-jargon.html): if(6 == a) ...
          //workaround for ranges, where it's acceptable - if(6 < a && a < 10) ...
          case Apply(Select(
            yoda @ Apply(Select(Literal(Constant(const1)), func1), List(notLiteral1)), opLogic), 
            List(Apply(Select(notLiteral2, func2), List(arg2))))
            if (func1.toString matches "[$](greater|less)([$]eq)?") && !isLiteral(notLiteral1)
            && (func2.toString matches "[$](greater|less)([$]eq)?")  && (notLiteral1 equalsStructure notLiteral2)
            && (func1.toString.take(5) == func2.toString.take(5)) => // cheap way of saying < and <= can appear in range together
            
            nowarnPositions += yoda.pos
            
          case Apply(Select(Literal(Constant(const)), func), List(notLiteral)) if (func.toString matches "[$](greater|less|eq)([$]eq)?") && !isLiteral(notLiteral) =>
            warn(tree, "You are using Yoda conditions")

          /// Unnecessary Ifs
          case If(cond, Literal(Constant(true)), Literal(Constant(false))) =>
            warn(cond, "Remove the if and just use the condition.")
          case If(cond, Literal(Constant(false)), Literal(Constant(true))) =>
            warn(cond, "Remove the if and just use the negated condition.")
          case If(cond, a, b) if (a equalsStructure b) && (a.children.nonEmpty) => 
            //TODO: empty if statement (if(...) { }) triggers this - issue warning for that case?
            //TODO: test if a single statement counts as children.nonEmpty
            warn(a, "If statement branches have the same structure.")
          case If(cond, a, If(cond2, b, c)) 
            if (a.children.nonEmpty && ((a equalsStructure b) || (a equalsStructure c))) 
            || (b.children.nonEmpty && (b equalsStructure c)) =>
            //TODO: could be made recursive, but probably no need
            
            warn(a, "If statement branches have the same structure.")

          /// Find repeated (sub)conditions that will never hold
          // caches conditions separated by OR, and checks all subconditions separated by either AND or OR
          case If(condition, _, e) if {
            def getSubConds(cond: Tree)(op: Name): List[Tree] =
              List(cond) ++ (cond match {
                case Apply(Select(left, opp), List(right)) if op == opp =>
                  getSubConds(left)(op) ++ getSubConds(right)(op)
                case _ =>
                  Nil
              })
            lazy val conds = mutable.ListBuffer(getSubConds(condition)(nme.ZOR):_*)
            def elseIf(tree: Tree) {
              tree match {
                case If(cond, _, e) => 
                  val subCondsOr = getSubConds(cond)(nme.ZOR)
                  val subCondsAnd = getSubConds(cond)(nme.ZAND)

                  for(newCond <- (subCondsOr ++ subCondsAnd); 
                      oldCond <- conds; if newCond equalsStructure oldCond)
                    warn(newCond, "This condition has appeared earlier in the if-else chain, and will never hold here.")

                  conds ++= subCondsOr

                  elseIf(e)
                case _ =>
              }
            }
            elseIf(e)
            false
          } => //Fallthrough

          //Ignore: ignores while(true)... I mean, one can't accidentally use while(true), can they? :)
          /*case LabelDef(whileName, List(), If(cond @ Literal(Constant(a: Boolean)), _, _)) =>
            //TODO: doesn't actually ignore, but that test is trivial anyway, commenting both
          case If(cond @ Literal(Constant(bool: Boolean)), _, _) => 
            //TODO: there are people still doing breakable { while(true) {... don't warn on while(true)?
            warn(cond, "This condition will always be "+bool+".")*/
            
          //TODO: Move to abstract interpreter once it handles booleans
          /*case Apply(Select(Literal(Constant(false)), nme.ZAND), _) =>
            warn(tree, "This part of boolean expression will always be false.")
          case Apply(Select(_, nme.ZAND), List(lite @ Literal(Constant(false)))) =>
            warn(lite, "This part of boolean expression will always be false.")
          case Apply(Select(Literal(Constant(true)), nme.ZOR), _) =>
            warn(tree, "This part of boolean expression will always be true.")
          case Apply(Select(_, nme.ZOR), List(lite @ Literal(Constant(true)))) =>
            warn(lite, "This part of boolean expression will always be true.")*/
            
          /// if(cond1) { if(cond2) ... } is the same as if(cond1 && cond2) ...
          case If(_, If(_, body, Literal(Constant(()))), Literal(Constant(()))) =>
            warn(tree, "These two nested ifs can be merged into one.")
          
          /// Abstract interpretation, and multiple-statement checks
          //TODO: make abs-interpreter good enough to handle the whole units and even some cross-unit stuff
          // probably multipass
          case ClassDef(mods, name, tparams, impl) =>
            abstractInterpretation.traverseBlock(impl)

          case ModuleDef(mods, name, impl) => 
            abstractInterpretation.traverseBlock(impl)

          case Function(params, body) =>
            abstractInterpretation.traverseBlock(body)
                    
          case blockElem @ Block(init, last) =>
            abstractInterpretation.traverseBlock(blockElem)

            val block = init :+ last

            /// Check for unused variable values
            sealed trait AssignStatus
            case class Unknown() extends AssignStatus
            case class Unused() extends AssignStatus
            case class Used() extends AssignStatus
            
            val assigns = mutable.HashMap[Name, AssignStatus]().withDefaultValue(Unknown())
            def checkAssigns(tree: Tree, onlySetUsed: Boolean) {
              tree match {
                // TODO: It could check if it gets set in all branches - Ignores currently
                case If(cond, t, f) =>
                  tree.children foreach { t => checkAssigns(t, onlySetUsed = true) }
                case Match(pat, cases) =>
                  tree.children foreach { t => checkAssigns(t, onlySetUsed = true) }
                
                case ValDef(mods, id, _, right) if mods.isMutable =>
                  //TODO: shadowing warning doesn't work, even if I make sure each tree is visited once, and even if I don't traverse inner Blocks
                  //if(assigns contains id) warn(tree, "Variable "+id.toString+" is being shadowed here.")
                  checkAssigns(right, onlySetUsed)
                  //Two exceptions: null and None are sometimes legit init values without use:
                  if(right isAny ("null", "scala.None"))
                    assigns(id) = Used()
                  else if(!onlySetUsed)
                    assigns(id) = Unused()
                    
                case Assign(ident @ Ident(id), right) =>
                  checkAssigns(right, onlySetUsed)
                  if(!onlySetUsed) assigns(id) match {
                    case Unused() =>
                      if(!(ident.tpe <:< BooleanClass.tpe || ident.tpe <:< IntClass.tpe)) //Ignore Boolean and Int
                        warn(tree, "Variable "+id.toString+" has an unused value before this reassign.")
                    case Used() =>
                      assigns(id) = Unused()
                    case Unknown() =>
                  }
                  
                case Ident(id) =>
                  assigns(id) = Used()
                  
                case tree =>
                  //for(Ident(id) <- tree; if assigns(id) == Unused()) assigns(id) == Used()
                  tree.children foreach { t => checkAssigns(t, onlySetUsed) }
              }
            }
            
            block foreach { t => checkAssigns(t, onlySetUsed = false) }
            /// Warnings when exiting a block - don't seem to work right
            //val unused = (assigns filter { _._2 == Unused() } map { _._1.toString.trim } mkString ", ")
            //if(!unused.isEmpty) warn(block.last, "Variable(s) "+unused+" have an unused value before here.")

            /// Checks on two subsequent statements
            val blockPairs = (block zip block.tail)
            blockPairs foreach {
              case (Assign(id1, id2), Assign(id2_, id1_)) if(id1 equalsStructure id1_) && (id2 equalsStructure id2_) =>
                warn(id1_, "Did you mean to swap these two variables?")

              /// "...; val x = value; x }" at the end of a method - usually I do this for debug outputs
              // this could be generalized in the new unused value code above 
              //case (v @ ValDef(_, id1, _, _), l @ Ident(id2)) if id1.toString == id2.toString && (l eq last) =>
              //  warn(v, "You don't need that temp variable.")

              case (i1 @ If(cond1, _, _), i2 @ If(cond2, _, _)) if (cond1 equalsStructure cond2) && (i1 match {
                case If(Ident(_), _, _) => //Ignore single booleans - probably debug/logging
                  false 
                case If(Select(Ident(_), nme.UNARY_!), _, _) => //Ignore single booleans - probably debug/logging
                  false
                case If(cond, t, f) if (getUsed(cond) & (getAssigned(t) ++ getAssigned(f))).nonEmpty => //Ignore if assigning variables which appear in condition
                  false
                case If(cond, notBlock, empty) 
                  if !notBlock.isInstanceOf[Block] && !notBlock.isInstanceOf[ValDef] 
                  && (empty equalsStructure Literal(Constant(()))) => //Ignore simple things - probably debug/logging
                  false
                case _ =>
                  true
                }) =>
                  
                warn(cond2, "Two subsequent ifs have the same condition")

              case (s1, s2) if (s1 equalsStructure s2) && !(s1 is "scala.this.Predef.println()") =>
                warn(s2, "You're doing the exact same thing twice.")

              case _ =>
            }

          case forloop @ Apply(TypeApply(Select(collection, func), _), List(Function(List(ValDef(_, param, _, _)), body))) =>
            abstractInterpretation.forLoop(forloop)

          case DefDef(_, _, _, _, _, block) => 
            abstractInterpretation.traverseBlock(block)

          //TODO: these two are probably superseded by abs-interpreter
          case pos @ Apply(Select(seq, apply), List(Literal(Constant(index: Int)))) 
            if methodImplements(pos.symbol, SeqLikeApply) && index < 0 =>
            warn(pos, "Using a negative index for a collection.")

          // cannot check double/float, as typer will automatically translate it to Infinity
          case divByZero @ Apply(Select(rcvr, op), List(Literal(Constant(0))))
            if (op == nme.DIV || op == nme.MOD)
            && (rcvr.tpe <:< ByteClass.tpe
             || rcvr.tpe <:< ShortClass.tpe
             || rcvr.tpe <:< IntClass.tpe
             || rcvr.tpe <:< LongClass.tpe) =>
            warn(divByZero, "Literal division by zero.")

          case _ =>
        }

        def containsAnyType(tpe: Type): Boolean = (tpe =:= AnyClass.tpe || tpe.typeArgs.exists(_ =:= AnyClass.tpe))
        def containsNothingType(tpe: Type): Boolean = (tpe =:= NothingClass.tpe || tpe.typeArgs.exists(_ =:= NothingClass.tpe))

        tree match {
          /// an Option of an Option
          //TODO: make stricter if you want, but Ident(_) could get annoying if someone out there is actually using this :)
          case ValDef(_, _, _, value) if isOptionOption(value) =>
            warn(tree, "Why would you need an Option of an Option?")

          case ValDef(mods, name, tpe, body) 
            if !mods.isParameter 
            && !(name.toString.trim matches "res[0-9]+") //workaround for REPL
            && ((tpe.toString contains "Any") || (tpe.toString contains "Nothing")) // Gets rid of Stuff[_]
            && (containsAnyType(tpe.tpe) || containsNothingType(tpe.tpe))
            && (inferred contains tpe.pos) 
            && !(body.isInstanceOf[New])=>
            
            val exceptions = body match {
              case Apply(Select(New(_), nme.CONSTRUCTOR), _) => true
              case TypeApply(Select(_, asInstanceOf), _) if asInstanceOf is "asInstanceOf" => true
              case Apply(TypeApply(Select(collection, apply), List(typeTree: TypeTree)), elems) 
                if (apply is "apply") && !(typeTree.original == null) => true
              case Ident(_) => true              
              case _ => false
            }
            
            if(!exceptions)
              warn(tree, "Inferred type "+tpe.tpe+". This might not be what you intended.")

          /// Putting null into Option (idea by Smotko)
          case DefDef(_, name, _, _, tpe, body) if (tpe.toString matches "Option\\[.*\\]") &&
            (body match {
              case n @ Literal(Constant(null)) => warn(n, "You probably meant None, not null."); true;
              case Block(_, n @ Literal(Constant(null))) => warn(n, "You probably meant None, not null."); true;
              case _ => false
            }) => //
          case ValDef(_, name, tpe, body) if (tpe.toString matches "Option\\[.*\\]") &&
            (body match {
              case n @ Literal(Constant(null)) => warn(n, "You probably meant None, not null."); true;
              case Block(_, n @ Literal(Constant(null))) => warn(n, "You probably meant None, not null."); true;
              case _ => false
            }) => //
          case Assign(left, right) if (left.tpe.toString matches "Option\\[.*\\]") &&
            (right match {
              case n @ Literal(Constant(null)) => warn(n, "You probably meant None, not null."); true;
              case Block(_, n @ Literal(Constant(null))) => warn(n, "You probably meant None, not null."); true;
              case _ => false
            }) => //
          
          /// null checking instead of Option wrapping
          case If(Apply(Select(left, op), List(Literal(Constant(null)))), t, f) 
            if (op == nme.EQ && (t is "scala.None") && (f match {
              case Apply(TypeApply(scala_Some_apply, _), List(some)) if (left equalsStructure some) && (scala_Some_apply startsWith "scala.Some.apply") => true
              case _ => false
            }))
            || (op == nme.NE && (f is "scala.None") && (t match {
              case Apply(TypeApply(scala_Some_apply, _), List(some)) if (left equalsStructure some) && (scala_Some_apply startsWith "scala.Some.apply") => true
              case _ => false
            })) =>

            warn(tree, "Use Option(...), which automatically wraps null to None")
          
          /// Comparing to None
          /*case Apply(Select(opt, op), List(scala_None)) if (op == nme.EQ || op == nme.NE) && (scala_None is "scala.None") =>
            warn(tree, "Use .isDefined instead of comparing to None")*/

          /// orElse(Some(...)).get is better written as getOrElse(...)
          case Select(Apply(TypeApply(Select(opt, orElse), _), List(Apply(scala_Some_apply, List(value)))), get)
            if (orElse is "orElse") && (get is "get") && (scala_Some_apply startsWith "scala.Some.apply") =>
            
            warn(scala_Some_apply, "Use getOrElse(...) instead of orElse(Some(...)).get")

          /// if(opt.isDefined) opt.get else something is better written as getOrElse(something)
          //TODO: improve the warning text, and curb the code duplication
          case If(Select(opt1, isDefined), getCase @ Select(opt2, get), elseCase) //duplication
            if (isDefined is "isDefined") && (get is "get") && (opt1 equalsStructure opt2) && !(elseCase.tpe.widen <:< NothingClass.tpe) =>
            
            if(elseCase equalsStructure Literal(Constant(null))) {
              warn(opt2, "Use opt.orNull or opt.getOrElse(null) instead of if(opt.isDefined) opt.get else null")
            } else if(getCase.tpe.widen <:< elseCase.tpe.widen) {
              warn(opt2, "Use opt.getOrElse(...) instead of if(opt.isDefined) opt.get else ...")
            }
          case If(Select(Select(opt1, isDefined), nme.UNARY_!), elseCase, getCase @ Select(opt2, get)) //duplication
            if (isDefined is "isDefined") && (get is "get") && (opt1 equalsStructure opt2) && !(getCase.tpe.widen <:< NothingClass.tpe) =>
            
            if(elseCase equalsStructure Literal(Constant(null))) {
              warn(opt2, "Use opt.orNull or opt.getOrElse(null) instead of if(!opt.isDefined) null else opt.get")
            } else if(getCase.tpe.widen <:< elseCase.tpe.widen) {
              warn(opt2, "Use opt.getOrElse(...) instead of if(!opt.isDefined) ... else opt.get")
            }
          case If(Apply(Select(opt1, nme.NE), List(scala_None)), getCase @ Select(opt2, get), elseCase) //duplication
            if (scala_None is "scala.None") && (get is "get") && (opt1 equalsStructure opt2) && !(elseCase.tpe.widen <:< NothingClass.tpe) =>
            
            if(elseCase equalsStructure Literal(Constant(null))) {
              warn(opt2, "Use opt.orNull or opt.getOrElse(null) instead of if(opt != None) opt.get else null")
            } else if(getCase.tpe.widen <:< elseCase.tpe.widen) {
              warn(opt2, "Use opt.getOrElse(...) instead of if(opt != None) opt.get else ...")
            }
          
          /// find(...).isDefined is better written as exists(...)
          case Select(Apply(pos @ Select(collection, find), func), isDefined) 
            if (find is "find") && (isDefined is "isDefined") && (collection startsWithAny ("scala.", "immutable.")) =>
            
            warn(pos, "Use exists(...) instead of find(...).isDefined")

          /// flatMap(if(...) x else Nil/None) is better written as filter(...)
          case Apply(TypeApply(Select(collection, flatMap), _), List(Function(List(ValDef(flags, param, _, _)), If(cond, e1, e2))))
            if flatMap is "flatMap" =>

            // swap branches, to simplify the matching
            val (expr1, expr2) = if(e1 endsWithAny (".Nil", ".None")) (e1, e2) else (e2, e1)

            (expr1, expr2) match {
              case (nil,Apply(TypeApply(Select(collection, apply), _), List(Ident(id))))
                if (collection startsWithAny ("scala.collection.immutable.", "immutable."))
                && (nil endsWith ".Nil") 
                && (id.toString == param.toString) =>
                
                warn(tree, "Use filter(x => condition) instead of flatMap(x => if(condition) ... else ...)")

              case (Apply(option2Iterable1, List(none)),Apply(option2Iterable2, List(Apply(TypeApply(Select(some, apply), _), List(Ident(id))))))
                if (none is "scala.None")
                && (some is "scala.Some")
                && (id.toString == param.toString) =>
                
                warn(tree, "Use filter(x => condition) instead of flatMap(x => if(condition) ... else ...)")
                
              case _ => 
                //println((showRaw(expr1), showRaw(expr2)))
            }
            
            /// Checks for Option.size, which is probably a bug (use .isDefined instead)
            case t @ Select(Apply(option2Iterable, List(opt)), size) if (option2Iterable.toString contains "Option.option2Iterable") && (size is "size") =>
              
              if(opt.tpe.widen.typeArgs.exists(tp => tp.widen <:< StringClass.tpe))
                warn(t, "Did you mean to take the size of the string inside the Option?")
              else if(opt.tpe.widen.typeArgs.exists(tp => tp.widen.baseClasses.exists(_.tpe =:= TraversableClass.tpe)))
                warn(t, "Did you mean to take the size of the collection inside the Option?")
              else
                warn(t, "Using Option.size is not recommended, use Option.isDefined instead")
              
          case _ =>
        }
        
        val MapFactoryClass = definitions.getClass(newTermName("scala.collection.generic.MapFactory"))
        tree match {
          /// Checks for duplicate mappings in a Map
          case Apply(TypeApply(Select(map, apply), _), args)
            if (apply is "apply")
            && (map.tpe.baseClasses.exists(_.tpe <:< MapFactoryClass.tpe)) =>
            
            val keys = args flatMap {
              case Apply(TypeApply(Select(Apply(any2ArrowAssoc, List(lhs)), arrow), _), rhs)
                if (arrow is "$minus$greater")
                && (any2ArrowAssoc startsWith "scala.this.Predef.any2ArrowAssoc") 
                && (lhs.isInstanceOf[Literal] || lhs.isInstanceOf[Ident]) => //TODO: support pure functions someday
                List(lhs)
                
              case Apply(tuple2apply, List(lhs, rhs)) 
                if (tuple2apply startsWith "scala.Tuple2.apply") 
                && (lhs.isInstanceOf[Literal] || lhs.isInstanceOf[Ident]) =>
                List(lhs)

              case _ => //unknown mapping format, TODO
                Nil
            }
            
            keys.foldLeft(List[Tree]())((acc, newItem) => 
              if(acc.exists(item => item equalsStructure newItem)) {
                warn(newItem, "This key has already been defined, and will override the previous mapping.")
                acc
              } else {
                acc :+ newItem
              }
            )
            
          /// Checks for inefficient use of .size
          case Apply(Select(pos @ Select(obj, size), op), List(Literal(Constant(x))))
            if (size isAny ("size", "length"))
            && (obj.tpe.widen.baseClasses.exists(_.tpe =:= ListClass.tpe) || obj.tpe.widen <:< StringClass.tpe)
            && ((x == 0 && (op == nme.EQ || op == nme.GT || op == nme.LE || op == nme.NE))
            || (x == 1 && (op == nme.LT || op == nme.GE))) =>
          
            if(op == nme.EQ || op == nme.LE || op == nme.LT)
              warn(pos, "Use isEmpty instead of comparing to size.")
            else
              warn(pos, "Use nonEmpty instead of comparing to size.")

          /*case Apply(Select(obj, op), List(Literal(Constant(""))))
            if (obj.tpe.widen <:< StringClass.tpe)
            && (op == nme.EQ || op == nme.NE) =>
            
            if(op == nme.EQ)
              warn(tree, "Use isEmpty instead of comparing to empty string.")
            else
              warn(tree, "Use nonEmpty instead of comparing to empty string.")*/
              
          /// Passing a block that returns a function to map - http://scalapuzzlers.com/#pzzlr-001
          case Apply(TypeApply(Select(collection, op), _), List(block @ Block(list, ret @ Function(List(ValDef(paramMods, _, _, _)), funcbody))))
            if paramMods.isSynthetic
            && op.toString.matches("map|foreach|filter(Not)?") //TODO: add more
            && list.nonEmpty // col.map(func), where func is an already defined function
            && (list match { // eta expansion, similar to above
              case List(ValDef(_, eta, _, _)) if eta.startsWith("eta$") => false 
              case _ => true
            })
            && collection.tpe.baseClasses.exists(_.tpe =:= TraversableClass.tpe) =>
            
            warn(block, "You're passing a block that returns a function - the statements in this block, except the last one, will only be executed once.")

          /// Integer division assigned to a floating point variable
          case Assign(varName, body) if isFloatingType(varName) && hasIntegerDivision(body) =>
            warn(body, "Integer division detected in an expression assigned to a floating point variable.")
          case Apply(Select(varName, varSetter), List(body)) if (varSetter endsWith "_$eq") && isFloatingType(body) && hasIntegerDivision(body) =>
            warn(body, "Integer division detected in an expression assigned to a floating point variable.")
          case ValDef(_, _, tpe, body) if isFloatingType(tpe) && hasIntegerDivision(body) =>
            warn(body, "Integer division detected in an expression assigned to a floating point variable.")

          case _ => 
        }

        super.traverse(tree)
      }
    }
  }
  private object PostRefChecksComponent extends PluginComponent {
    val global = LinterPlugin.this.global
    import global._
    
    override val runsAfter = List("refchecks")
    
    val phaseName = "linter-refchecked"
    
    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: global.CompilationUnit) {
        if(!unit.isJava) new PostRefChecksTraverser(unit).traverse(unit.body)
      }
    }
    
    class PostRefChecksTraverser(unit: CompilationUnit) extends Traverser {
      implicit val unitt = unit

      override def traverse(tree: Tree) {
        tree match {
          case DefDef(mods: Modifiers, name, _, valDefs, typeTree, body) =>
            //workaround for scala 2.9 - copied from compiler code
            def isOverridingSymbol(s: Symbol): Boolean = {
              def canMatchInheritedSymbols: Boolean = (
                   (s ne NoSymbol)
                && s.owner.isClass
                && !s.isClass
                && !s.isConstructor
              )

              (canMatchInheritedSymbols && s.owner.ancestors.exists(base => s.overriddenSymbol(base) != NoSymbol))
            }
          
            /// Unused method parameters
            if(name.toString != "<init>" && !body.isEmpty && !(mods.isOverride || isOverridingSymbol(tree.symbol))) {
              //Get the parameters, except the implicit ones
              val params = valDefs.flatMap(_.filterNot(_.mods.isImplicit)).map(_.name.toString).toBuffer

              // Is the body simple enough to ignore?
              def isBodySimple(body: Tree): Boolean = !(body exists { case Block(_, _) => true; case _ => false })

              if(!(name.toString == "main" && params.size == 1 && params.head == "args") && !isBodySimple(body)) { // filter main method
                val used = for(Ident(name) <- tree if params contains name.toString) yield name.toString
                val unused = params -- used
                
                //TODO: scalaz is a good codebase for finding interesting false positives
                //TODO: macro impl is special case?
                unused.size match {
                  case 0 => //
                  case 1 => warn(tree, "Parameter %s is not used in method %s." format (unused.mkString(", "), name))
                  case _ => warn(tree, "Parameters (%s) are not used in method %s." format (unused.mkString(", "), name))
                }
              }
            }
            
          case _ => 
        }
        super.traverse(tree)
      }
    }
  }

}
