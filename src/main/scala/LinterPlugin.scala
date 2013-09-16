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
  val components = List[PluginComponent](PreTyperComponent, PostTyperComponent, PostTyperInterpreterComponent, PostRefChecksComponent)
  
  override val optionsHelp: Option[String] = Some(Seq(
    "%s:comma,separated,warning,names".format(name, LinterOptions.EnableOnlyArgument),
    "%s:comma,separated,warning,names".format(name, LinterOptions.DisableOnlyArgument)
  ).map("  -P:" + name + ":" + _).mkString("\n"))

	override def processOptions(options: List[String], error: String => Unit) {
    LinterOptions.parse(options) match {
     case Left(errorMessage) => error(errorMessage)
     case Right(LinterOptions(disabledWarnings)) => 
       Utils.disabledWarningNames = disabledWarnings
    }
  }

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
            warn(unusedTrait._2, UnextendedSealedTrait)(unit)
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
      
      import java.math.MathContext
      val java_math_MathContext = definitions.getClass(newTermName("java.math.MathContext"))
      def getMathContext(t: Tree): Option[MathContext] = t match {
        case Apply(Select(New(mc), nme.CONSTRUCTOR), (Literal(Constant(precision: Int)) :: roundingMode)) =>
          Some(new MathContext(precision)) // I think roundingMode is irrelevant, because the check will warn if there is any rounding
        case _ =>
          None
      }

      override def traverse(tree: Tree) { 
        //TODO: the matchers are broken up for one reason only - Scala 2.9 pattern matcher :)
        // Workarounds for some cases
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
          //// Some numeric checks
          /*case Apply(Select(lhs, nme.EQ), List(rhs))
            if isSubtype(lhs, DoubleClass.tpe) || isSubtype(lhs, FloatClass.tpe) || isSubtype(rhs, DoubleClass.tpe) || isSubtype(rhs, FloatClass.tpe) =>
            
            warn(tree, "Exact comparison of floating point values is potentially unsafe.")*/
            
          /// Tip to use log1p(x) and expm1(x), instead of log(1+x) and exp(x)-1 -- see http://www.johndcook.com/blog/2010/06/07/math-library-functions-that-seem-unnecessary/
          //TODO: maybe make checks to protect against potentially wrong fixes, e.g. log1p(a + 1) or log1p(a - 1)
          // also, check 1-exp(x) and other negated versions
          case Apply(log, List(Apply(Select(Literal(Constant(1)), nme.ADD), _))) if log is "scala.math.`package`.log" => 
            warn(tree, UseLog1p)
          case Apply(log, List(Apply(Select(_, nme.ADD), List(Literal(Constant(1)))))) if log is "scala.math.`package`.log" => 
            warn(tree, UseLog1p)
            
          case Apply(Select(Apply(exp, _), nme.SUB), List(Literal(Constant(1)))) if exp is "scala.math.`package`.exp" => 
            warn(tree, UseExpm1)
          case Apply(Select(Literal(Constant(-1)), nme.ADD), List(Apply(exp, _))) if exp is "scala.math.`package`.exp" =>
            warn(tree, UseExpm1)

          /// Use x.abs instead of doing it manually
          case Apply(sqrt, List(Apply(pow, List(expr, Literal(Constant(2))))))
            if (sqrt is "scala.math.`package`.sqrt") && (pow is "scala.math.`package`.pow") =>
            
            warn(tree, UseAbsNotSqrtPow)

          case Apply(math_sqrt, List(Apply(Select(expr1, nme.MUL), List(expr2))))
            if (expr1 equalsStructure expr2) && (math_sqrt is "scala.math.`package`.sqrt") =>
            
            warn(tree, UseAbsNotSqrtSquare)

          /// Use x.isNaN instead of (x != x)
          case Apply(Select(left, func), List(right))
            if isFloatingType(left)
            && (func == nme.EQ || func == nme.NE)
            && ((left equalsStructure right) || (right equalsStructure Literal(Constant(Double.NaN))) || (right equalsStructure Literal(Constant(Float.NaN)))) =>
            
            if(left equalsStructure right) 
              warn(tree, UseIsNanNotSelfComparison)
            else
              warn(tree, UseIsNanNotNanComparison)

          /// Use x.signum instead of doing it manually
          case pos @ Apply(Select(expr1, nme.DIV), List(expr2)) if ((expr1, expr2) match {
              case (expr1, Apply(abs, List(expr2))) if (abs is "scala.math.`package`.abs") && (expr1 equalsStructure expr2) => true
              case (Apply(abs, List(expr1)), expr2) if (abs is "scala.math.`package`.abs") && (expr1 equalsStructure expr2) => true
              case (expr1, Select(Apply(wrapper, List(expr2)), abs)) if (wrapper endsWith "Wrapper") && (abs is "abs") && (expr1 equalsStructure expr2) => true
              case (Select(Apply(wrapper, List(expr1)), abs), expr2) if (wrapper endsWith "Wrapper") && (abs is "abs") && (expr1 equalsStructure expr2) => true
              case _ => false
            }) =>
            
            warn(pos, UseSigNum)
            
          case _ => 
        }
        
        tree match {
          /// BigDecimal checks
          // BigDecimal(0.1) //TODO: someday move to interpreter - detect this for known values
          case Apply(Select(bigDecimal, apply_valueOf), (treeLiteral @ Literal(Constant(literal))) :: context)
            if ((bigDecimal is "scala.`package`.BigDecimal") || (bigDecimal endsWith "math.BigDecimal"))
            && (apply_valueOf isAny ("apply", "valueOf")) 
            && (isFloatingType(treeLiteral) || treeLiteral.tpe.widen <:< StringClass.tpe) =>
            
            val isFloat = isFloatingType(treeLiteral)
            
            // actualLiteral relevant only for double - compiler rounds it automatically
            val (strLiteral, actualLiteral) = 
              if(isFloat) {
                // Get the actual token from code
                var token = ""
                try {
                  val pos = treeLiteral.pos
                  token = pos.lineContent.substring(pos.column - 1).takeWhile(_.toString matches "[-+0-9.edfEDF]").toLowerCase
                  if(!token.isEmpty && (token.last == 'f' || token.last == 'd')) token = token.dropRight(1)
                } catch {
                  case e: java.lang.UnsupportedOperationException => // Happens if tree doesn't have a position
                  case e: java.lang.NumberFormatException => // Could warn here, but I don't trust the above code enough
                }
                (literal.toString, token)
              } else {
                (literal.asInstanceOf[String], literal.asInstanceOf[String])
              }
            
            val mathContext = 
              if(context.size == 1 && context.head.tpe <:< java_math_MathContext.tpe) {
                getMathContext(context.head)
              } else {
                None
              }

            val warnMsg =
              if(isFloat) {
                if(strLiteral == actualLiteral && mathContext.isDefined)
                  "Possible loss of precision - use a larger MathContext."
                else
                  "Possible loss of precision - " + (if(apply_valueOf is "apply") "use a string constant." else "use a constructor with a string constant.")
              } else {
                if(mathContext.isDefined) {
                  "Possible loss of precision - use a larger MathContext."
                } else {
                  "Possible loss of precision - add a custom MathContext."
                }
              }

            try {
              // Ugly hack to get around a few different representations: 0.005, 5E-3, ...
              def cleanString(s: String): String = s.replaceAll("^-|[.]|[Ee].*$|(0[.])?0*", "")
              
              val bd = if(mathContext.isDefined) BigDecimal(strLiteral, mathContext.get) else BigDecimal(strLiteral)
              if(cleanString(bd.toString) != cleanString(actualLiteral)) warn(tree, warnMsg)
            } catch {
              case e: java.lang.NumberFormatException =>
                warn(tree, BigDecimalNumberFormat)
            }

          // new java.math.BigDecimal(0.1)
          case Apply(Select(New(java_math_BigDecimal), nme.CONSTRUCTOR), List(Literal(Constant(d: Double)))) 
            if java_math_BigDecimal is "java.math.BigDecimal" =>
            
            warn(tree, BigDecimalPrecisionLoss)
          
          case _ =>
        }
        
        tree match {
          /// Checks for self-assignments: a = a
          case Assign(left, right) if left equalsStructure right =>
            warn(left, ReflexiveAssignment)
          case Apply(Select(type1, varSetter), List(Select(type2, varName))) 
            if (type1 equalsStructure type2) && (varSetter.toString == varName.toString+"_$eq") =>
            warn(type1, ReflexiveAssignment)

          /// Checks if you read from a file without closing it: scala.io.Source.fromFile(file).mkString
          //TODO: Only checks one-liners where you chain it - doesn't actually check if you close it
          //TODO: Possibly skips checking code in higher order functions later on
          case Select(fromFile, _) if fromFile startsWith "scala.io.Source.fromFile" =>
            warn(fromFile, CloseSourceFile)
            return
            
          /// Comparing with == on instances of different types: 5 == "5"
          //TODO: Scala 2.10 has a similar check "comparing values of types Int and String using `==' will always yield false"
          case Apply(eqeq @ Select(lhs, nme.EQ), List(rhs))
            if methodImplements(eqeq.symbol, Object_==)
            && !isSubtype(lhs, rhs) && !isSubtype(rhs, lhs)
            && lhs.tpe.widen.toString != "Null" && rhs.tpe.widen.toString != "Null" =>
            
            warn(eqeq, new UnlikelyEquality(lhs.tpe.widen.toString, rhs.tpe.widen.toString))

          /// Warn agains importing from collection.JavaConversions
          case Import(pkg, selectors) if (pkg.symbol == JavaConversionsModule) && (selectors exists isGlobalImport) =>
            warn(pkg, JavaConverters)
          
          /// Warn about wildcard imports (disabled)
          /*case Import(pkg, selectors) if selectors exists isGlobalImport =>
            //TODO: Too much noise - maybe it would be useful to non-IDE users if it printed 
            // a nice selector import replacement, e.g. import mutable._ => import mutable.{HashSet,ListBuffer}
            
            warn(pkg, "Wildcard imports should be avoided. Favor import selector clauses.")*/

          /// Collection.contains on different types: List(1,2,3).contains("2")
          case Apply(contains @ Select(seq, _), List(target)) 
            if methodImplements(contains.symbol, SeqLikeContains) 
            && !(target.tpe <:< SeqMemberType(seq.tpe)) =>
            
            warn(contains, new ContainsTypeMismatch(seq.tpe.widen.toString, target.tpe.widen.toString))

          /// Warn about using .asInstanceOf[T] (disabled)
          //TODO: false positives in case class A(), and in the interpreter init
          /*case aa @ Apply(a, List(b @ Apply(s @ Select(instanceOf,dd),ee))) if methodImplements(instanceOf.symbol, AsInstanceOf) =>
            //println((aa,instanceOf))
          case instanceOf @ Select(a, func) if methodImplements(instanceOf.symbol, AsInstanceOf) =>   
            //TODO: too much noise, maybe detect when it's completely unnecessary
            warn(tree, "Avoid using asInstanceOf[T] (use pattern matching, type ascription, etc).")*/

          /// Calling Option.get is potentially unsafe (disabled)
          //TODO: if(x.isDefined) func(x.get) / if(x.isEmpty) ... else func(x.get), etc. are false positives -- those could be detected in abs-interpreter
          /*case get @ Select(_, nme.get) if methodImplements(get.symbol, OptionGet) => 
            warn(tree, "Calling .get on Option will throw an exception if the Option is None.")*/

          /// Nag about using null (disabled)
          //TODO: Too much noise - limit in some way
          /*case Literal(Constant(null)) =>
            warn(tree, "Using null is considered dangerous, use Option.")*/

          //// String checks 
          /// Repeated string literals (disabled)
          /*case Literal(Constant(str: String)) =>
            //TODO: String interpolation gets broken down into parts and causes false positives
            //TODO: a quick benchmark showed string literals are actually more optimized than almost anything else, even final vals
            val threshold = 4

            stringLiteralCount(str) += 1
            if(stringLiteralCount(str) == threshold && !(stringLiteralFileExceptions.contains(unit.source.toString)) && !(str.matches(stringLiteralExceptions))) {
              //TODO: Too much noise :)
              warn(tree, """String literal """"+str+"""" appears multiple times.""")
            }*/
            
          /// str.substring(len) -> str.drop(len) (disabled)
          /*case Apply(Select(str, substring), List(strLen))
            if str.tpe <:< definitions.StringClass.tpe            
            && substring.toString == "substring"
            && getStringFromLength(strLen).isDefined =>
            
            warn(tree, "Use x.take(len), instead of x.substring(len)")*/
            
          /// str.substring(0, str.length - len) -> str.dropRight(len) (disabled)
          /*case Apply(Select(str1, substring), List(Literal(Constant(0)), Apply(Select(str1Len, nme.SUB), List(str2Len))))
            if str1.tpe <:< definitions.StringClass.tpe
            && substring.toString == "substring"
            && getStringFromLength(str1Len).isDefined && getStringFromLength(str2Len).isDefined
            && (str1 equalsStructure getStringFromLength(str1Len).get) =>
          
            warn(tree, "Use x.dropRight(len), instead of x.substring(x.length-len)")*/
            
          /// Processing a constant string: "hello".size (disabled)
          /*case Apply(Select(pos @ Literal(Constant(s: String)), func), params) =>
            func.toString match {
              case "$plus"|"equals"|"$eq$eq"|"toCharArray"|"matches"|"getBytes" => //ignore
              case "length" => warn(pos, "Taking the length of a constant string")
              case _        => warn(pos, "Processing a constant string")
            }
          case Select(Apply(Select(predef, augmentString), List(pos @ Literal(Constant(s: String)))), size)
            if predef is "scala.this.Predef" && augmentString.toString == "augmentString" && size.toString == "size" => 
            warn(pos, "Taking the size of a constant string")*/
            
          //// Pattern Matching checks
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
              
              warn(tree, PatternMatchConstant)
            }
            
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
                  /// Checks if pattern matching on Option -- see: http://blog.tmorris.net/posts/scalaoption-cheat-sheet/ (disabled)
                  //TODO: too much noise, and some cases are perfectly fine - try detecting all the exact cases from link
                  //warn(tree, "There are probably better ways of handling an Option.")
                } else if(booleanCase) {
                  /// Checks if pattern matching on Boolean
                  //TODO: case something => ... case _ => ... is also an if in a lot of cases
                  warn(tree, PreferIfToBooleanMatch)
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
                warn(streak.tree.body, new IdenticalCaseBodies(streak.streak.toString))
              }
            }
            
            /// Checking for unused variables in pattern matching (disabled)
            def checkUsage(c: CaseDef) {
              //TODO: use for self-testing from time to time - ~100 warnings currently :/
              /*val binds = for(b @ Bind(name, _) <- c.pat; if !name.toString.startsWith("_")) yield (b, name.toString)
              for(unused <- binds.filter { case (b, name) => !abstractInterpretation.isUsed(c, name)}) {
                println(showRaw(pat))
                warn(unused._1, "Unused value in pattern matching, use _ instead. (or prefix with _ to get rid of me)")
              }*/
            }

            /// Detect some unreachable cases 
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
                warn(c, IdenticalCaseConditions)
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

          //// If checks
          /// Same expression on both sides of comparison.
          case Apply(Select(left, func), List(right)) 
            if (func.toString matches "[$](greater|less|eq|bang)([$]eq)?") && (left equalsStructure right) =>
            
            warn(tree, ReflexiveComparison)

          /// Same expression on both sides of a boolean operation.
          case Apply(Select(left, func), List(right)) 
            if (func == nme.ZOR || func == nme.OR || func == nme.XOR || func == nme.ZAND || func == nme.AND/* || func == nme.EQ || func == nme.NE*/)
            && (left.symbol == null || !left.symbol.isMethod)
            && (left equalsStructure right) 
            && tree.tpe.widen <:< BooleanClass.tpe =>
            
            warn(tree, ReflexiveComparison)
           
          /// Yoda conditions -- http://www.codinghorror.com/blog/2012/07/new-programming-jargon.html
          // workaround for ranges, where it's acceptable - if(6 < a && a < 10) ...
          case Apply(Select(
            yoda @ Apply(Select(Literal(Constant(const1)), func1), List(notLiteral1)), opLogic), 
            List(Apply(Select(notLiteral2, func2), List(arg2))))
            if (func1.toString matches "[$](greater|less)([$]eq)?") && !isLiteral(notLiteral1)
            && (func2.toString matches "[$](greater|less)([$]eq)?")  && (notLiteral1 equalsStructure notLiteral2)
            && (func1.toString.take(5) == func2.toString.take(5)) => // cheap way of saying < and <= can appear in range together
            
            nowarnPositions += yoda.pos
            
          case Apply(Select(Literal(Constant(const)), func), List(notLiteral)) if (func.toString matches "[$](greater|less|eq)([$]eq)?") && !isLiteral(notLiteral) =>
            warn(tree, YodaConditions)

          /// Unnecessary Ifs
          case If(cond, Literal(Constant(true)), Literal(Constant(false))) =>
            warn(cond, new UseConditionDirectly())
          case If(cond, Literal(Constant(false)), Literal(Constant(true))) =>
            warn(cond, new UseConditionDirectly(negated = true))
          case If(cond, a, b) if (a equalsStructure b) && (a.children.nonEmpty) => 
            //TODO: empty if statement (if(...) { }) triggers this - issue warning for that case?
            //TODO: test if a single statement counts as children.nonEmpty
            warn(a, DuplicateIfBranches)
          case If(cond, a, If(cond2, b, c)) 
            if (a.children.nonEmpty && ((a equalsStructure b) || (a equalsStructure c))) 
            || (b.children.nonEmpty && (b equalsStructure c)) =>
            //TODO: could be made recursive, but probably no need
            
            warn(a, DuplicateIfBranches)

          /// Find repeated (sub)conditions in if-else chains, that will never hold
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
                    warn(newCond, IdenticalIfCondition)

                  conds ++= subCondsOr

                  elseIf(e)
                case _ =>
              }
            }
            elseIf(e)
            false
          } => //Fallthrough

          /// if(cond1) { if(cond2) { ... } } is the same as if(cond1 && cond2) { ... }
          case If(_, If(_, body, Literal(Constant(()))), Literal(Constant(()))) =>
            warn(tree, MergeNestedIfs)
          
          //// Multiple-statement checks
          case blockElem @ Block(init, last) =>
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
                        warn(tree, new VariableAssignedUnusedValue(id.toString))
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
            /// Warning on exiting a block with unused values (broken/disabled)
            /*val unused = (assigns filter { _._2 == Unused() } map { _._1.toString.trim } mkString ", ")
            if(!unused.isEmpty) warn(block.last, "Variable(s) "+unused+" have an unused value before here.")*/

            //// Checks on two subsequent statements
            val blockPairs = (block zip block.tail)
            blockPairs foreach {
              /// Probably mistake in swaping two variables: a = b, b = b
              case (Assign(id1, id2), Assign(id2_, id1_)) if(id1 equalsStructure id1_) && (id2 equalsStructure id2_) =>
                warn(id1_, "Did you mean to swap these two variables?")

              /// "...; val x = value; x }" at the end of a method (disabled)
              // TODO: disabled - I usually have commented debug outputs in between these
              // TODO: this could be generalized in the new unused value code above 
              /*case (v @ ValDef(_, id1, _, _), l @ Ident(id2)) if id1.toString == id2.toString && (l eq last) =>
                warn(v, "You don't need that temp variable.")*/

              /// Doing the same thing twice
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
                nowarnPositions += s2.pos
                warn(s1, "You're doing the exact same thing twice or more.")

              case _ =>
            }

          /// Literal negative index for a collection (obsolete?)
          case pos @ Apply(Select(seq, apply), List(Literal(Constant(index: Int)))) 
            if methodImplements(pos.symbol, SeqLikeApply) && index < 0 =>
            warn(pos, "Using a negative index for a collection.")

          /// Literal division by zero (obsolete?)
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

          /// Inferred type Nothing, Any, M[Nothing], or M[Any] (idea by OlegYch)
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
          
          /// Comparing Option to None instead of using isDefined (disabled)
          /*case Apply(Select(opt, op), List(scala_None)) if (op == nme.EQ || op == nme.NE) && (scala_None is "scala.None") =>
            warn(tree, "Use .isDefined instead of comparing to None")*/

          /// orElse(Some(...)).get is better written as getOrElse(...)
          case Select(Apply(TypeApply(Select(opt, orElse), _), List(Apply(scala_Some_apply, List(value)))), get)
            if (orElse is "orElse") && (get is "get") && (scala_Some_apply startsWith "scala.Some.apply") =>
            
            warn(scala_Some_apply, "Use getOrElse(...) instead of orElse(Some(...)).get")

          /// if(opt.isDefined) opt.get else something is better written as getOrElse(something) and similar warnings
          //TODO: improve the warning text, and curb the code duplication
          case If(Select(opt1, isDefined), getCase @ Select(opt2, get), elseCase) //duplication
            if (isDefined is "isDefined") && (get is "get") && (opt1 equalsStructure opt2) && !(elseCase.tpe.widen <:< NothingClass.tpe) =>
            
            if(elseCase equalsStructure Literal(Constant(null))) warn(opt2, "Use opt.orNull or opt.getOrElse(null) instead of if(opt.isDefined) opt.get else null")
            else if(getCase.tpe.widen <:< elseCase.tpe.widen)    warn(opt2, "Use opt.getOrElse(...) instead of if(opt.isDefined) opt.get else ...")

          case If(Select(Select(opt1, isDefined), nme.UNARY_!), elseCase, getCase @ Select(opt2, get)) //duplication
            if (isDefined is "isDefined") && (get is "get") && (opt1 equalsStructure opt2) && !(elseCase.tpe.widen <:< NothingClass.tpe) =>
            
            if(elseCase equalsStructure Literal(Constant(null))) warn(opt2, "Use opt.orNull or opt.getOrElse(null) instead of if(!opt.isDefined) null else opt.get")
            else if(getCase.tpe.widen <:< elseCase.tpe.widen)    warn(opt2, "Use opt.getOrElse(...) instead of if(!opt.isDefined) ... else opt.get")

          case If(Select(opt1, isEmpty), elseCase, getCase @ Select(opt2, get)) //duplication
            if (isEmpty is "isEmpty") && (get is "get") && (opt1 equalsStructure opt2) && !(elseCase.tpe.widen <:< NothingClass.tpe) =>
            
            if(elseCase equalsStructure Literal(Constant(null))) warn(opt2, "Use opt.orNull or opt.getOrElse(null) instead of if(opt.isEmpty) null else opt.get")
            else if(getCase.tpe.widen <:< elseCase.tpe.widen)    warn(opt2, "Use opt.getOrElse(...) instead of if(opt.isEmpty) ... else opt.get")

          case If(Select(Select(opt1, isEmpty), nme.UNARY_!), getCase @ Select(opt2, get), elseCase) //duplication
            if (isEmpty is "isEmpty") && (get is "get") && (opt1 equalsStructure opt2) && !(elseCase.tpe.widen <:< NothingClass.tpe) =>
            
            if(elseCase equalsStructure Literal(Constant(null))) warn(opt2, "Use opt.orNull or opt.getOrElse(null) instead of if(!opt.isEmpty) opt.get else null")
            else if(getCase.tpe.widen <:< elseCase.tpe.widen)    warn(opt2, "Use opt.getOrElse(...) instead of if(!opt.isEmpty) opt.get else ...")

          case If(Apply(Select(opt1, nme.NE), List(scala_None)), getCase @ Select(opt2, get), elseCase) //duplication
            if (scala_None is "scala.None") && (get is "get") && (opt1 equalsStructure opt2) && !(elseCase.tpe.widen <:< NothingClass.tpe) =>
            
            if(elseCase equalsStructure Literal(Constant(null))) warn(opt2, "Use opt.orNull or opt.getOrElse(null) instead of if(opt != None) opt.get else null")
            else if(getCase.tpe.widen <:< elseCase.tpe.widen)    warn(opt2, "Use opt.getOrElse(...) instead of if(opt != None) opt.get else ...")

          case If(Apply(Select(opt1, nme.EQ), List(scala_None)), elseCase, getCase @ Select(opt2, get)) //duplication
            if (scala_None is "scala.None") && (get is "get") && (opt1 equalsStructure opt2) && !(elseCase.tpe.widen <:< NothingClass.tpe) =>
            
            if(elseCase equalsStructure Literal(Constant(null))) warn(opt2, "Use opt.orNull or opt.getOrElse(null) instead of if(opt == None) opt.get else null")
            else if(getCase.tpe.widen <:< elseCase.tpe.widen)    warn(opt2, "Use opt.getOrElse(...) instead of if(opt == None) opt.get else ...")
          
          /// find(...).isDefined is better written as exists(...)
          /// filter(...).isEmpty is better written as exists(...)
          case Select(Apply(pos @ Select(collection, find_filter), func), isEmpty_isDefined) 
            if ((find_filter isAny ("find", "filter")) && (isEmpty_isDefined isAny ("isEmpty", "isDefined")))
            && (collection startsWithAny ("scala.", "immutable.")) =>
            
            warn(pos, "Use exists(...) instead of "+find_filter+"(...)."+isEmpty_isDefined)

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
            
            /// Warnings about using Option.size, which is probably a bug (use .isDefined instead)
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
            
          //// Inefficient use of .size, instead of is/nonEmpty (Idea by non)
          /// Inefficient use of List.size, instead of is/nonEmpty
          case Apply(Select(pos @ Select(obj, size), op), List(Literal(Constant(x))))
            if (size isAny ("size", "length"))
            && (obj.tpe.widen.baseClasses.exists(_.tpe =:= ListClass.tpe) || obj.tpe.widen <:< StringClass.tpe)
            && ((x == 0 && (op == nme.EQ || op == nme.GT || op == nme.LE || op == nme.NE))
            || (x == 1 && (op == nme.LT || op == nme.GE))) =>
          
            if(op == nme.EQ || op == nme.LE || op == nme.LT)
              warn(pos, "Use isEmpty instead of comparing to size.")
            else
              warn(pos, "Use nonEmpty instead of comparing to size.")

          /// Inefficient use of String.size, instead of is/nonEmpty (disabled)
          /*case Apply(Select(obj, op), List(Literal(Constant(""))))
            if (obj.tpe.widen <:< StringClass.tpe)
            && (op == nme.EQ || op == nme.NE) =>
            
            if(op == nme.EQ)
              warn(tree, "Use isEmpty instead of comparing to empty string.")
            else
              warn(tree, "Use nonEmpty instead of comparing to empty string.")*/
              
          /// Passing a block that returns a function to map, instead of a function -- http://scalapuzzlers.com/#pzzlr-001
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

          /// Integer division in an expression assigned to a floating point variable
          case Assign(varName, body) if isFloatingType(varName) && hasIntegerDivision(body) =>
            warn(body, "Integer division detected in an expression assigned to a floating point variable.")
          case Apply(Select(varName, varSetter), List(body)) if (varSetter endsWith "_$eq") && isFloatingType(body) && hasIntegerDivision(body) =>
            warn(body, "Integer division detected in an expression assigned to a floating point variable.")
          case ValDef(_, _, tpe, body) if isFloatingType(tpe) && hasIntegerDivision(body) =>
            warn(body, "Integer division detected in an expression assigned to a floating point variable.")

          /// col.flatten instead of col.filter(_.isDefined).map(_.get)
          case Apply(TypeApply(Select(Apply(Select(col, filter), List(Function(List(ValDef(_, p1, _, _)), Select(p1_, isDefined)))), map), _), List(Function(List(ValDef(_, p2, _, _)), Select(p2_, get))))
            if(col.tpe.widen.baseClasses.exists(_.tpe =:= TraversableClass.tpe) && 
              (p1.toString == p1_.toString) && (p1_.tpe.widen.baseClasses.exists(_.tpe =:= OptionClass.tpe)) &&
              (p2.toString == p2_.toString) && (p2_.tpe.widen.baseClasses.exists(_.tpe =:= OptionClass.tpe)) &&
              (filter is "filter") && (isDefined is "isDefined") && (map is "map") && (get is "get")) =>

            warn(tree, "Use col.flatten instead of col.filter(_.isDefined).map(_.get)")

          /// use partial function directly - temporary variable is unnecessary (idea by yzgw)
          case Apply(_, List(Function(List(ValDef(mods, x_1, TypeTree(), EmptyTree)), Match(x_1_, _))))
            if ((x_1 is "x$1") && (x_1_ is "x$1") && (mods.isSynthetic) && (mods.isParameter))   // _ match { ... }
            || ((x_1.toString == x_1_.toString) && !(mods.isSynthetic) && (mods.isParameter)) => // x match { ... }
            
            val param = if(x_1 is "x$1") "_" else x_1.toString + " => " + x_1.toString
            
            //TODO: also detects for(x <- col) x match { ... } ... current workaround with filter has false negatives
            warn(tree, "You can pass the partial function in directly. (Remove \""+param+" match {\")", filters = List("for", "<-"))

          case _ => 
            //if(tree.toString contains "...") println(showRaw(tree))
        }

        super.traverse(tree)
      }
    }
  }

  //// Abstract Interpreter
  private object PostTyperInterpreterComponent extends PluginComponent {
    val global = LinterPlugin.this.global
    import global._

    override val runsAfter = List("typer")

    val phaseName = "linter-typed-interpreter"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: global.CompilationUnit) {
        if(!unit.isJava) {
          new PostTyperInterpreterTraverser(unit).traverse(unit.body)
        }
      }
    }

    class PostTyperInterpreterTraverser(unit: CompilationUnit) extends Traverser {
      implicit val unitt = unit
      var treePosHolder: Tree = null
      import Utils._
      val utils = new Utils[global.type](global)
      import utils._

      import definitions.{AnyClass, AnyValClass, NothingClass, PredefModule, ObjectClass, Object_==}
      import definitions.{OptionClass, SeqClass, TraversableClass, ListClass, StringClass}
      import definitions.{DoubleClass, FloatClass, CharClass, ByteClass, ShortClass, IntClass, LongClass, BooleanClass}

      def checkRegex(reg: String) {
        try {
          val regex = reg.r
        } catch {
          case e: java.util.regex.PatternSyntaxException =>
            warn(treePosHolder, "Regex pattern syntax error: "+e.getDescription)
          case e: Exception =>
        }
      }

      //Data structure ideas:
      //1. common trait for Value, Collection, StringAttrs
      //2. for Values: empty -> any, and add "nonValues" or "conditions" to cover
      //   if(a == 0) ..., if(a%2 == 0) ... even for huge collections

      //// Integer/List value data structure
      object Values {
        lazy val empty = new Values()
        def apply(i: Int, name: String = ""): Values = new Values(name = name, values = Set(i))
        def apply(low: Int, high: Int): Values = new Values(ranges = Set((low, high)))
      }
      class Values(
          val ranges: Set[(Int, Int)] = Set(),
          val values: Set[Int] = Set(),
          val conditions: Set[Int => Boolean] = Set(),//Currently obeyed only ifEmpty otherwise
          val name: String = "",
          val isSeq: Boolean = false,
          val actualSize: Int = -1
        ) {

        //require(isEmpty || isValue || isSeq || (ranges.size == 1 && values.size == 0) || (ranges.size == 0 && values.size > 0))
        //println(this)
        
        def conditionsContain(i: Int): Boolean = conditions.forall(c => c(i))
        
        // experimental alternative to empty
        def makeUnknown: Values = new Values(conditions = conditions)
       
        def rangesContain(i: Int): Boolean = (ranges exists { case (low, high) => i >= low && i <= high })
        
        def notSeq: Values = new Values(ranges, values, conditions, name, false)
        
        def contains(i: Int): Boolean = (values contains i) || rangesContain(i)
        def containsAny(i: Int*): Boolean = i exists { i => this.contains(i) }
        def apply(i: Int): Boolean = contains(i)
        //TODO: this crashes if (high-low) > Int.MaxValue - code manually, or break large ranges into several parts
        //def exists(func: Int => Boolean) = (values exists func) || (ranges exists { case (low, high) => (low to high) exists func })
        //def forall(func: Int => Boolean) = (values forall func) && (ranges forall { case (low, high) => (low to high) forall func })
        def existsLower(i: Int): Boolean = (values exists { _ < i }) || (ranges exists { case (low, high) => low < i })
        def existsGreater(i: Int): Boolean = (values exists { _ > i }) || (ranges exists { case (low, high) => high > i })
        def forallLower(i: Int): Boolean = (values forall { _ < i }) && (ranges forall { case (low, high) => (low <= high) && (high < i) })
        def forallEquals(i: Int): Boolean = (values forall { _ == i }) && (ranges forall { case (low, high) => (low == high) && (i == low) })
        
        def addRange(low: Int, high: Int): Values = new Values(ranges + (if(low > high) (high, low) else (low, high)), values, conditions, name, false, -1)
        def addValue(i: Int): Values = new Values(ranges, values + i, conditions, name, false, -1)
        def addSet(s: Set[Int]): Values = new Values(ranges, values ++ s, conditions, name, false, -1) //TODO: are these false, -1 ok?
        def addCondition(c: Int => Boolean): Values = new Values(ranges, values, conditions + c, name, isSeq, actualSize)
        def addConditions(c: (Int => Boolean)*): Values = new Values(ranges, values, conditions ++ c, name, isSeq, actualSize)
        def addConditions(c: Set[Int => Boolean]): Values = new Values(ranges, values, conditions ++ c, name, isSeq, actualSize)
        def addName(s: String): Values = new Values(ranges, values, conditions, s, isSeq, actualSize)
        def addActualSize(s: Int): Values = new Values(ranges, values, conditions, name, isSeq, s)
        //TODO: this can go wrong in many ways - (low to high) limit, freeze on huge collection, etc
        
        def distinct: Values = {
          val t = this.optimizeValues
          //ADD: optimizeranges that makes them non-overlapping
          if(t.values.size == 0 && t.ranges.size == 1) {
            new Values(ranges = t.ranges, conditions = conditions, /*name = name,*/ isSeq = isSeq, actualSize = actualSize)
          } else {
            new Values(values = t.values ++ t.ranges.flatMap { case (low, high) => (low to high) }, conditions = conditions, /*name = name,*/ isSeq = isSeq, actualSize = actualSize)
          }
        }
        //TODO: Is this correct for weird code?
        def sum: Int = {
          val t = this.distinct
          t.values.sum + ranges.foldLeft(0)((acc, n) => acc + (n._1 to n._2).sum)
        }
        
        // discard values, that are inside ranges
        def optimizeValues: Values = new Values(ranges, values.filter(v => !rangesContain(v)), conditions, name, isSeq, actualSize)
        //def optimizeRanges = new Values(values = values, ranges = ranges.)
        
        def isEmpty: Boolean = this.size == 0
        def nonEmpty: Boolean = this.size > 0
        def isValue: Boolean = this.values.size == 1 && this.ranges.isEmpty // TODO: this is stupid and buggy, and woudn't exist if I didn't mix all types into one class
        def getValue: Int = if(isValue) values.head else throw new Exception()
        def isValueForce: Boolean = (this.values.size == 1 && this.ranges.isEmpty) || (ranges.size == 1 && this.size == 1)
        def getValueForce: Int = if(isValue) values.head else if(ranges.size == 1 && this.size == 1) ranges.head._1 else throw new Exception()

        def max: Int = math.max(if(values.nonEmpty) values.max else Int.MinValue, if(ranges.nonEmpty) ranges.maxBy(_._2)._2 else Int.MinValue)
        def min: Int = math.min(if(values.nonEmpty) values.min else Int.MaxValue, if(ranges.nonEmpty) ranges.minBy(_._1)._1 else Int.MaxValue)

        def dropValue(i: Int): Values = new Values(
          ranges.flatMap { case (low, high) =>
            if(i > low && i < high) List((low,i-1), (i+1,high)) else
            if(i == low && i < high) List((low+1,high)) else
            if(i > low && i == high) List((low,high-1)) else
            if(i == low && i == high) Nil else
            List((low, high))
          }, 
          values - i,
          Set(),//ADD: conditions?
          name)

        //beware of some operations over ranges.
        def map(func: Int => Int, rangeSafe: Boolean = true): Values = //ADD: conditions?
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
        
        //// Apply and check a condition, return a pair of possible values of this Integer/List if condition is true and if false
        def applyCond(condExpr: Tree): (Values, Values) = {
          //TODO: alwaysTrue and false cound be possible returns for error msgs
          //TODO: check out if you're using 'this' for things that aren't... this method shouldn't be here anyways
          //println("expr: "+showRaw(condExpr))
          var alwaysHold, neverHold = false
          val out = condExpr match {
            case Apply(Select(expr1, nme.ZAND), List(expr2)) => //&&
              this.applyCond(expr1)._1.applyCond(expr2)._1
              
            case Apply(Select(expr1, nme.ZOR), List(expr2)) => //||
              val (left,right) = (this.applyCond(expr1)._1, this.applyCond(expr2)._1)
              new Values(
                left.ranges ++ right.ranges,
                left.values ++ right.values,
                left.conditions ++ right.conditions,
                this.name)

            // Pass on String functions
            case strFun @ Apply(Select(string, func), params) if string.tpe != null && string.tpe.widen <:< StringClass.tpe =>
              computeExpr(strFun)
              
            case strFun @ Select(Apply(scala_augmentString, List(string)), func)
              if (scala_augmentString.toString endsWith "augmentString") =>

              computeExpr(strFun)

            //ADD: expr op expr that handles idents right
            case Apply(Select(Ident(v), op), List(expr)) if v.toString == this.name && computeExpr(expr).isValue => 
            
              val value = computeExpr(expr).getValue
              val out: Values = op match {
                case nme.EQ => 
                  (if(this.nonEmpty) { 
                    if(this.contains(value)) {
                      if(this.forallEquals(value)) alwaysHold = true
                      Values(value).addName(name)
                    } else {
                      neverHold = true
                      this.makeUnknown
                    }
                  } else if(!this.conditionsContain(value)) {
                    neverHold = true
                    this.makeUnknown
                  } else {
                    this.makeUnknown
                  }).addCondition(_ == value)
                case nme.NE => 
                  (if(this.contains(value)) {
                    val out = this.dropValue(value) 
                    if(out.isEmpty) neverHold = true
                    out
                  } else if(this.nonEmpty) {
                    alwaysHold = true
                    this
                  } else {
                    this.makeUnknown
                  }).addCondition(_ != value)
                case nme.GT => new Values(
                    ranges.flatMap { case (low, high) => if(low > value) Some((low, high)) else if(high > value) Some((value+1, high)) else None },
                    values.filter { _ > value },
                    Set(_ > value),
                    this.name)
                case nme.GE => new Values(
                    ranges.flatMap { case (low, high) => if(low >= value) Some((low, high)) else if(high >= value) Some((value, high)) else None },
                    values.filter { _ >= value },
                    Set(_ >= value),
                    this.name)
                case nme.LT => new Values(
                    ranges.flatMap { case (low, high) => if(high < value) Some((low, high)) else if(low < value) Some((low, value-1)) else None },
                    values.filter { _ < value },
                    Set(_ < value),
                    this.name)
                case nme.LE => new Values(
                    ranges.flatMap { case (low, high) => if(high <= value) Some((low, high)) else if(low <= value) Some((low, value)) else None },
                    values.filter { _ <= value },
                    Set(_ <= value),
                    this.name)
                case a => 
                  //println("applyCond: "+showRaw( a ));
                  this.makeUnknown
              }
              
              if((Set[Name](nme.GT, nme.GE, nme.LT, nme.LE) contains op) && this.nonEmpty) {
                if(out.isEmpty) neverHold = true
                if(out.size == this.size) alwaysHold = true
              }
              
              out          
            case Apply(Select(expr1, op), List(expr2)) =>
              val (left, right) = (computeExpr(expr1), computeExpr(expr2))
              var out = Values.empty.addActualSize(this.actualSize)
              val startOut = out
              op match {
                case nme.EQ | nme.NE =>
                  if(left.isValue && right.isValue && left.getValue == right.getValue) {
                    if(op == nme.EQ) alwaysHold = true else neverHold = true
                    if(op == nme.EQ && (left.name == this.name || right.name == this.name)) out = this
                  } else if(left.nonEmpty && right.nonEmpty && (left.min > right.max || right.min > left.max)) {
                    if(op != nme.EQ) alwaysHold = true else neverHold = true
                    if(op != nme.EQ && (left.name == this.name || right.name == this.name)) out = this
                  } else if(left.name == this.name && right.isValueForce && op == nme.EQ) {
                    out = Values(right.getValueForce).addName(this.name)
                  } else if(right.name == this.name && left.isValueForce && op == nme.EQ) { //Yoda conditions 
                    out = Values(left.getValueForce).addName(this.name)
                  }
                  if(left.isEmpty && left.conditions.nonEmpty && right.isValue && !left.conditionsContain(right.getValue)) {
                    neverHold = true
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

              if(this.name == out.name) out.addConditions(this.conditions) else out
            case Select(expr, op) =>
              computeExpr(expr).applyUnary(op)
              Values.empty.addActualSize(this.actualSize)
              
            case expr =>
              //println("expr: "+showRaw(expr))
              computeExpr(expr)
              Values.empty.addActualSize(this.actualSize)
          }
          
          
          // Check if condition will always or never hold
          if(neverHold) warn(condExpr, "This condition will never hold.")
          if(alwaysHold) warn(condExpr, "This condition will always hold.")
          
          if(!isUsed(condExpr, this.name)) (this, this) else (out, if(neverHold) this else if(alwaysHold) Values.empty else this - out)
        }
        
        //TODO: does this even work? the v map is suspect and ugly
        def -(value: Values): Values = {
          if(this.isEmpty || value.isEmpty) {
            Values.empty
          } else {
            var out = this
            value map({ v => out = out.dropValue(v); v }, rangeSafe = false)
            out
          }
        }
        
        //// Apply a unary operation on a Integer/List variable
        def applyUnary(op: Name): Values = op match { 
          case nme.UNARY_+ => this
          case nme.UNARY_- => this.map(a => -a)
          case nme.UNARY_~ => this.map(a => ~a, rangeSafe = false)
          case signum if signum.toString == "signum" => this.map(a => math.signum(a)).addCondition({ a => Set(-1,0,1) contains a })
          case abs if abs.toString == "abs" =>
            new Values(//ADD: conditions
              ranges
                .map { case (low, high) => (if(low > 0) low else 0, math.max(math.abs(low), math.abs(high))) }
                .map { case (low, high) if low > high => (high, low); case (low, high) => (low, high) },
              values.map(a => math.abs(a))).addCondition(_ >= 0)

          case size if (size.toString matches "size|length") => if(this.actualSize != -1) Values(this.actualSize) else Values.empty.addCondition(_ >= 0)
          case head_last if (head_last.toString matches "head|last") => 
            //Only works for one element :)
            if(this.actualSize == 0) warn(treePosHolder, "Taking the "+head_last.toString+" of an empty collection.")
            if(this.actualSize == 1 && this.size == 1) Values(this.getValueForce) else Values.empty
          case tail_init if (tail_init.toString matches "tail|init") && this.actualSize != -1 => 
            if(this.actualSize == 0) {
              warn(treePosHolder, "Taking the "+tail_init.toString+" of an empty collection.")
              Values.empty
            } else {
              Values.empty.addActualSize(this.actualSize - 1)
            }
          
          case to if (to.toString matches "toIndexedSeq|toList|toSeq|toVector") => this //only immutable
          case distinct if (distinct.toString == "distinct") && this.actualSize != -1 => 
            val out = this.distinct
            out.addActualSize(out.size)
          
          case id if (id.toString matches "reverse") => this //Will hold, while Set is used for values
          case max if (max.toString == "max") && this.nonEmpty => Values(this.max)
          case min if (min.toString == "min") && this.nonEmpty => Values(this.min)
          case sum if (sum.toString == "sum") && this.nonEmpty && this.distinct.size == this.actualSize => Values(this.sum)

          case empty if (empty.toString == "isEmpty") && (this.actualSize != -1) => 
            warn(treePosHolder, "This condition will " + (if(this.actualSize == 0) "always" else "never") + " hold.")
            Values.empty
          case empty if (empty.toString == "nonEmpty") && (this.actualSize != -1) => 
            warn(treePosHolder, "This condition will " + (if(this.actualSize > 0) "always" else "never") + " hold.")
            Values.empty

          case a => 
            //val raw = showRaw( treePosHolder );
            //println("applyUnary: op:"+op+" thisval:"+this+" tree:"+treePosHolder.toString+"\n"+raw);
            Values.empty
        }
        //// Apply a binary operation on a Integer/List variable
        def applyBinary(op: Name, right: Values): Values = {
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
              case a if a.toString matches "apply|take|drop|max|min|contains|map|count" => ((a: Int, b: Int) => throw new Exception(), false): F //Foo, check below
              case _ => return Values.empty
          }
          
          val out: Values = (
            if(op.toString == "count") {
              (if(left.actualSize == 0) Values(0) else if(left.actualSize > 0) Values.empty.addCondition(_ < left.actualSize) else Values.empty).addCondition(_ >= 0)
            } else if(left.isEmpty || right.isEmpty) {
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
                  warn(treePosHolder, "This contains will always return true")
                } else {
                  warn(treePosHolder, "This contains will never return true")
                }
              }
              Values.empty
            } else if(op.toString == "max") {
              if(left.isValue && right.isValue) { 
                if(left.getValue >= right.getValue) {
                  warn(treePosHolder, "This max will always return the first value")
                } else {
                  warn(treePosHolder, "This max will always return the second value")
                }
                Values(math.max(left.getValue, right.getValue))
              } else if(left.isValue && !right.isValue) {
                if(left.getValue >= right.max) {
                  warn(treePosHolder, "This max will always return the first value")
                  Values(left.getValue)
                } else if(left.getValue <= right.min) {
                  warn(treePosHolder, "This max will always return the second value")
                  right
                } else {
                  Values.empty
                }
              } else if(!left.isValue && right.isValue) {
                if(right.getValue >= left.max) {
                  warn(treePosHolder, "This max will always return the second value")
                  Values(right.getValue)
                } else if(right.getValue <= left.min) {
                  warn(treePosHolder, "This max will always return the first value")
                  left
                } else {
                  Values.empty
                }
              } else {
                Values.empty
              }          
            } else if(op.toString == "min") {
              if(left.isValue && right.isValue) {
                if(left.getValue <= right.getValue) {
                  warn(treePosHolder, "This min will always return the first value")
                } else {
                  warn(treePosHolder, "This min will always return the second value")
                }
                Values(math.min(left.getValue, right.getValue))
              } else if(left.isValue && !right.isValue) {
                if(left.getValue <= right.min) {
                  warn(treePosHolder, "This min will always return the first value")
                  Values(left.getValue)
                } else if(left.getValue > right.max) {
                  warn(treePosHolder, "This min will always return the second value")
                  right
                } else {
                  Values.empty
                }
              } else if(!left.isValue && right.isValue) {
                if(right.getValue <= left.min) {
                  warn(treePosHolder, "This min will always return the second value")
                  Values(right.getValue)
                } else if(right.getValue > left.max) {
                  warn(treePosHolder, "This min will always return the first value")
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
                  warn(treePosHolder, "This take is always unnecessary.")
                  this 
                } else {
                  if(right.getValueForce <= 0) warn(treePosHolder, "This collection will always be empty.")
                  Values.empty.addName(name).addActualSize(math.max(0, right.getValueForce))
                }
              } else {
                Values.empty
              }
            } else if(op.toString == "drop") {
              if(left.isSeq && left.actualSize != -1 && right.isValue) {
                if(right.getValueForce <= 0) {
                  warn(treePosHolder, "This drop is always unnecessary.")
                  this
                } else {
                  if(left.actualSize-right.getValueForce <= 0) warn(treePosHolder, "This collection will always be empty.")
                  Values.empty.addName(name).addActualSize(math.max(0, left.actualSize-right.getValueForce))
                }
              } else { 
                Values.empty
              }
            } else if(left.isValue && right.isValue) {
              if(left.getValue == right.getValue && op == nme.SUB && !left.name.isEmpty) warn(treePosHolder, "Same values on both sides of subtraction.")
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
                    if(op == nme.SUB) warn(treePosHolder, "Same values on both sides of subtraction will return 0.")
                    if(op == nme.XOR) warn(treePosHolder, "Same values on both sides of ^ will return 0.")
                    Values(0)
                  case nme.AND | nme.OR  => left
                  case _ => Values.empty
                }
              } else {
                Values.empty
              }
            }
          )
          
          op match {
            case nme.MOD if right.isValue => out.addConditions(_ > -math.abs(right.getValue), _ < math.abs(right.getValue))
            case nme.DIV if left.isValue => out.addConditions(_ >= -math.abs(left.getValue), _ <= math.abs(left.getValue))
            /*case (nme.AND | nme.OR) if left.isValue || right.isValue => 
              //TODO: you need to learn two's complement, brah
              val (min, max) = (
                if(left.isValue && right.isValue) 
                  (math.min(left.getValue, right.getValue), math.max(left.getValue, right.getValue))
                else if(left.isValue) 
                  (left.getValue, left.getValue)
                else //if(right.isValue) 
                  (right.getValue, right.getValue)
              )
                  
              if(op == nme.AND) out.addConditions(_ <)*/
            case _ => out
          }
        }
        
        //approximate
        def size: Int = values.size + ranges.foldLeft(0)((acc, range) => acc + (range._2 - range._1) + 1)
        
        override def toString: String = 
          "Values("+(if(name.size > 0) name+")(" else "")+(values.map(_.toString) ++ ranges.map(a => a._1+"-"+a._2)).mkString(",")+", "+isSeq+", "+actualSize+")"
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
      //// Attempt to compute part of the AST, and return Integer/List value
      def computeExpr(tree: Tree): Values = {
        treePosHolder = tree
        val out: Values = tree match {
          case Literal(Constant(value: Int)) => Values(value)
          //TODO: I don't think .this. ones work at all...
          case Select(This(typeT), termName) =>
            val name = termName.toString
            val n = (if(name.contains(".this.")) name.substring(name.lastIndexOf(".")+1) else name).trim
            vals(n)
          case Ident(termName) => 
            val name = termName.toString
            val n = (if(name.contains(".this.")) name.substring(name.lastIndexOf(".")+1) else name).trim
            //println(n+": "+vals(n))
            if(vals contains n) vals(n) else if((defModels contains n) && defModels(n).isLeft) defModels(n).left.get else Values.empty
          case Apply(Ident(termName), params) if defModels contains termName.toString =>
            val n = termName.toString
            if(vals contains n) vals(n) else if((defModels contains n) && defModels(n).isLeft) defModels(n).left.get else Values.empty
          
          // String size
          /*case Apply(Select(Ident(id), length), List()) if stringVals.exists(_.name == Some(id.toString)) && length.toString == "length" =>
            val exactValue = stringVals.find(_.name == Some(id.toString)).map(_.exactValue)
            exactValue.map(v => if(v.isDefined) Values(v.get.size) else Values.empty).getOrElse(Values.empty)
            
          case Select(Apply(Select(predef, augmentString), List(Ident(id))), size)
            if stringVals.exists(_.name == Some(id.toString)) && predef.toString == "scala.this.Predef" && augmentString.toString == "augmentString" && size.toString == "size" => 
            val exactValue = stringVals.find(_.name == Some(id.toString)).map(_.exactValue)
            exactValue.map(v => Values(v.size)).getOrElse(Values.empty)*/
            
          // String stuff (TODO: there's a copy up at applyCond)
          case strFun @ Apply(Select(string, func), params) if string.tpe.widen <:< StringClass.tpe =>
            StringAttrs.stringFunc(string, func, params).right.getOrElse(Values.empty)
            
          case strFun @ Select(Apply(scala_augmentString, List(string)), func)
            if (scala_augmentString.toString endsWith "augmentString") =>
            
            StringAttrs.stringFunc(string, func).right.getOrElse(Values.empty)

          case strFun @ Apply(Select(Apply(scala_augmentString, List(string)), func), params)
            if (scala_augmentString.toString endsWith "augmentString") =>
            
            StringAttrs.stringFunc(string, func, params).right.getOrElse(Values.empty)

          /// Division by zero
          case pos @ Apply(Select(_, op), List(expr)) if (op == nme.DIV || op == nme.MOD) && {
            val value = computeExpr(expr)
            if(value.isValue && value.getValue == 1) {
              if(op == nme.MOD)
                warn(pos, "Taking the modulo by one will always return zero.")
              else
                warn(pos, "Dividing by one will always return the original number.")

              true
            } else if(value.contains(0)) {
              warn(pos, "You will likely divide by zero here.")

              true
            } else {
              false //Fallthrough
            }
          } =>
            Values.empty

          // Range
          case Apply(Select(Apply(scala_Predef_intWrapper, List(Literal(Constant(low: Int)))), to_until), List(Literal(Constant(high: Int)))) 
            if (scala_Predef_intWrapper.toString endsWith "Wrapper") && (to_until.toString matches "to|until") =>
            
            val high2 = if(to_until.toString == "to") high else high-1
            new Values(Set((low, high2)), Set(), Set(), "", isSeq = true, high2-low)

          /// Use (low until high) instead of (low to high-1)
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
                  }) warn(treePosHolder, "Use (low until high) instead of (low to high-1)")
                case _ =>
              }
            }
            
            if((to_until.toString matches "to|until") && computeExpr(expr1).isValue && computeExpr(expr2).isValue) {
              val (low, high) = (computeExpr(expr1).getValue, computeExpr(expr2).getValue + (if(to_until.toString == "to") 0 else -1))
              
              new Values(Set((low, high)), Set(), Set(), name = "", isSeq = true, actualSize = high-low)
            } else {
              Values.empty
            }

          case t @ Apply(TypeApply(genApply @ Select(_,_), _), genVals)
            if methodImplements(genApply.symbol, SeqLikeGenApply) && (!t.tpe.widen.toString.contains("collection.mutable.")) =>
            
            val values = genVals.map(v => computeExpr(v))
            if(values.forall(_.isValue)) new Values(values = values.map(_.getValue).toSet, isSeq = true, actualSize = values.size) else Values.empty.addActualSize(genVals.size)

          //Array isn't immutable, maybe later
          //case Apply(Select(Select(scala, scala_Array), apply), genVals) if(scala_Array.toString == "Array") =>
          
          //TODO: is this for array or what?
          case Select(Apply(arrayOps @ Select(_, intArrayOps), List(expr)), op) if(arrayOps.toString == "scala.this.Predef.intArrayOps") => 
            computeExpr(expr).applyUnary(op)
          
          case Apply(TypeApply(t @ Select(expr, op), _), List(scala_math_Ordering_Int)) if scala_math_Ordering_Int.toString.endsWith("Int") => //.max .min
            computeExpr(t)

          case Apply(Select(scala_math_package, op), params) if scala_math_package.toString == "scala.math.`package`" =>
            op.toString match {
              case "abs"|"signum" if params.size == 1 => computeExpr(params.head).applyUnary(op)
              case "max"|"min"    if params.size == 2 => computeExpr(params(0)).applyBinary(op, computeExpr(params(1)))
              case _ => Values.empty
            }

          /// Parameter of Random.nextInt might be lower than 1 (runtime exception)
          case Apply(Select(scala_util_Random, nextInt), params) if nextInt.toString == "nextInt" && scala_util_Random.tpe <:< definitions.getClass(newTermName("scala.util.Random")).tpe =>
            if(params.size == 1) {
              val param = computeExpr(params.head)
              if(param.nonEmpty) {
                if(param.min <= 0) {
                  warn(treePosHolder, "The parameter of this nextInt might be lower than 1 here.")
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

          case Apply(TypeApply(Select(valName, op), _), List(scala_math_Numeric_IntIsIntegral)) 
            if scala_math_Numeric_IntIsIntegral.toString == "math.this.Numeric.IntIsIntegral" && op.toString == "sum" =>
            
            computeExpr(valName).applyUnary(op)

          //List(Literal(Constant(1)), Literal(Constant(2)), Literal(Constant(3)), Literal(Constant(4)), Literal(Constant(0)), Literal(Constant(0))))
          case Select(expr, op) =>
            //println((expr, op, computeExpr(expr).applyUnary(op)))
            computeExpr(expr).applyUnary(op)

          case Apply(Select(expr1, op), List(expr2)) =>
            //println("BinaryOp: "+(op, expr1, expr2, (computeExpr(expr1))(op)(computeExpr(expr2))))
            (computeExpr(expr1)).applyBinary(op, computeExpr(expr2))
          
          case Apply(Apply(TypeApply(Select(valName, map), List(_, _)), List(Function(List(ValDef(mods, paramName, _, EmptyTree)), expr))), _) if(map.toString == "map") => 
            //List(TypeApply(Select(Select(This(newTypeName("immutable")), scala.collection.immutable.List), newTermName("canBuildFrom")), List(TypeTree()))))
            
            pushDefinitions()

            val res = computeExpr(valName)
            vals += paramName.toString -> res
            //println(">        "+vals)
            val out = computeExpr(expr)

            popDefinitions()
            //println(">        "+out)
            out
          
          case If(condExpr, expr1, expr2) =>
            //TODO: if condExpr always/never holds, return that branch
            pushDefinitions()
            
            val e1 = computeExpr(expr1)

            popDefinitions()
            pushDefinitions()

            val e2 = computeExpr(expr2)
            
            popDefinitions()
            
            /*println(vals)
            vals = backupVals.map(a => (a._1, a._2.applyCond(condExpr)._1)).withDefaultValue(Values.empty)
            println("e1"+vals)
            val e1 = computeExpr(expr1)
            println("e1"+e1)
            stringVals = backupStrs

            vals = backupVals.map(a => (a._1, a._2.applyCond(condExpr)._2)).withDefaultValue(Values.empty)
            println("e2"+vals)
            val e2 = computeExpr(expr2)
            println("e2"+e1)*/
            
            //if(e1.isValue && e2.isValue) new Values(values = e1.values ++ e2.values) else Values.empty
            if(expr1.tpe <:< NothingClass.tpe) {
              e2
            } else if(expr2.tpe <:< NothingClass.tpe) {
              e1
            } else if(!e1.isSeq && !e2.isSeq && e1.nonEmpty && e2.nonEmpty) {
              new Values(values = e1.values ++ e2.values, ranges = e1.ranges ++ e2.ranges)
            } else {
              Values.empty
            }

          case a => 
            //val raw = showRaw( a ); if(!exprs.contains(raw) && raw.size < 700 && raw.size > "EmptyTree".size)println("computeExpr: "+treePosHolder.toString+"\n"+raw); exprs += raw
            //for(Ident(id) <- a) if(stringVals.exists(_.name == Some(id.toString))) {println("id: "+id+"  "+showRaw( a )); }
            //println("computeExpr: "+showRaw( a ))
            Values.empty
        }
        //println(out+"  "+showRaw(tree))
        out
      }
      //val exprs = mutable.HashSet[String]()
          
      var vals = Map[String, Values]().withDefaultValue(Values.empty)
      var vars = Set[String]()
      var stringVals = Set[StringAttrs]()
      var defModels = Map[String, Either[Values, StringAttrs]]().withDefaultValue(Left(Values.empty))
      var labels = Map[String, Tree]()
      def discardVars() {
        for(v <- vars) vals += v -> Values.empty
      }
      def discardVars(tree: Tree) {
        for(v <- vars; if isAssigned(tree, v)) {
          vals += v -> Values.empty
          stringVals = stringVals.filter(v => v.name.isDefined && !(vars contains v.name.get))
        }
      }
      def discardVars(tree: Tree, force: Set[String]) {
        for(v <- vars; if isAssigned(tree, v) || (force contains v)) {
          vals += v -> Values.empty
          stringVals = stringVals.filter(v => v.name.isDefined && !(vars contains v.name.get))
        }
      }
      def discardVars(force: Set[String]) {
        for(v <- vars; if (force contains v)) {
          vals += v -> Values.empty
          stringVals = stringVals.filter(v => v.name.isDefined && !(vars contains v.name.get))
        }
      }
      
      //vals,vars,stringVals,defModels
      val backupStack = mutable.Stack[(Map[String, Values], Set[String], Set[StringAttrs], Map[String, Either[Values, StringAttrs]])]()
      def pushDefinitions() {
        backupStack.push((vals, vars, stringVals, defModels))
      }
      def popDefinitions() {
        //discards new and discarded vars also
        val varsCurr = vars
        val (valsBack, varsBack, stringValsBack, defModelsBack) = backupStack.pop
        vals = valsBack
        vars = varsBack
        discardVars((varsCurr | varsBack) -- (varsCurr & varsBack))
        stringVals = stringValsBack
        defModels = defModelsBack
      }
      
      def forLoop(tree: Tree) {
        treePosHolder = tree
        //TODO: actually anything that takes (A <: (a Number) => _), this is awful
        val funcs = "foreach|map|filter(Not)?|exists|find|flatMap|forall|groupBy|count|((drop|take)While)|(min|max)By|partition|span"

        val (param, values, body, func, collection) = tree match {
          case Apply(TypeApply(Select(collection, func), _), List(Function(List(ValDef(_, param, _, _)), body))) if (func.toString matches funcs) =>
            //println(showRaw(collection))
            val values = computeExpr(collection).addName(param.toString)
            //println(values)
            //if(values.isEmpty) {
                //println("not a collection I know("+tree.pos+"): "+showRaw(collection))
                //return
            //}
            
            (param.toString, values, body, func.toString, collection)
          case _ => 
            //println("not a loop("+tree.pos+"): "+showRaw(tree))
            return
        }
        
        if(values.nonEmpty) {
          vals += param -> values.notSeq
          
          val exceptions =
            (func == "foreach" ||
            collection.tpe.toString.startsWith("scala.collection.immutable.Range"))
          
          if(!isUsed(body, param) && !exceptions) warn(tree, "Iterator value is not used in the body.")

          traverseBlock(body)
        }
      }
      
      val doNotTraverse = mutable.HashSet[Tree]()
      
      //implicit def String2StringAttrs(s: String) = new StringAttrs(exactValue = Some(s))
      //// String abstract interpreter
      object StringAttrs {
        //TODO: when merging, save known chunks - .contains then partially works
        def empty: StringAttrs = new StringAttrs()
        
        // scalastyle:off magic.number
        def toStringAttrs(param: Tree): StringAttrs = {
          val intParam = computeExpr(param)
          if(intParam.isValue) new StringAttrs(exactValue = Some(intParam.getValue.toString))
          if(intParam.size > 1) {
            val maxLen = math.max(intParam.max.toString.length, intParam.min.toString.length)
            new StringAttrs(minLength = 1, trimmedMinLength = 1, maxLength = maxLen, trimmedMaxLength = maxLen)
          } else if(param match { case Literal(Constant(a)) => true case _  => false }) param match { //groan.
            case Literal(Constant(null)) => new StringAttrs(exactValue = Some("null"))
            case Literal(Constant(a))    => new StringAttrs(Some(a.toString))
          } else if(param.tpe.widen <:< CharClass.tpe)  new StringAttrs(minLength = 1, trimmedMinLength = 0, maxLength = 1, trimmedMaxLength = 0)
          else if(param.tpe.widen <:< ByteClass.tpe)    new StringAttrs(minLength = 1, trimmedMinLength = 1, maxLength = 4, trimmedMaxLength = 4)
          else if(param.tpe.widen <:< ShortClass.tpe)   new StringAttrs(minLength = 1, trimmedMinLength = 1, maxLength = 6, trimmedMaxLength = 6)
          else if(param.tpe.widen <:< IntClass.tpe)     new StringAttrs(minLength = 1, trimmedMinLength = 1, maxLength = 11, trimmedMaxLength = 11)
          else if(param.tpe.widen <:< LongClass.tpe)    new StringAttrs(minLength = 1, trimmedMinLength = 1, maxLength = 20, trimmedMaxLength = 20)
          //http://stackoverflow.com/questions/1701055/what-is-the-maximum-length-in-chars-needed-to-represent-any-double-value :)
          else if(param.tpe.widen <:< DoubleClass.tpe)  new StringAttrs(minLength = 1, trimmedMinLength = 1, maxLength = 1079, trimmedMaxLength = 1079)
          else if(param.tpe.widen <:< FloatClass.tpe)   new StringAttrs(minLength = 1, trimmedMinLength = 1, maxLength = 154, trimmedMaxLength = 154)
          else if(param.tpe.widen <:< BooleanClass.tpe) new StringAttrs(minLength = 4, trimmedMinLength = 4, maxLength = 5, trimmedMaxLength = 5)
          else {
            //TODO: prefix "Type(" and suffix ")" for collections
            //TODO: not sure if this is the right way, but <:< TraversableClass.tpe does not work directly
            // also, it's possible to have a Traversable, which has a shorter toString - check if you're in scala. or Predef
            if((param.tpe.baseClasses.exists(_.tpe =:= TraversableClass.tpe)) && !(param.tpe.widen <:< StringClass.tpe)) {
              // collections: minimal is Nil or Type()
              //TODO: Surely I can do moar... intParam.isSeq, etc
              val minLen = 3
              //println(Left(str + new StringAttrs(minLength = minLen, trimmedMinLength = minLen)))
              new StringAttrs(minLength = minLen, trimmedMinLength = minLen)
            } else {
              //TODO:discover moar
              //if(!(param.tpe.widen <:< StringClass.tpe) && !(param.tpe.widen <:< AnyClass.tpe))println(((str, param), (param.tpe, param.tpe.widen)))
            
              StringAttrs(param)
            }
          }
        }
        // scalastyle:on magic.number
        
        //// Tries to execute string functions and returns either a String or Int representation
        def stringFunc(string: Tree, func: Name, params: List[Tree] = List[Tree]()): Either[StringAttrs, Values] = {
          val str = StringAttrs(string)
          lazy val intParam = if(params.size == 1 && params.head.tpe.widen <:< IntClass.tpe) computeExpr(params.head) else Values.empty
          lazy val intParams = if(params.forall(_.tpe.widen <:< IntClass.tpe)) params.map(computeExpr).toList else List() //option?
          lazy val stringParam = if(params.size == 1 && params.head.tpe.widen <:< StringClass.tpe) StringAttrs(params.head) else empty
          lazy val stringParams = if(params.forall(_.tpe.widen <:< StringClass.tpe)) params.map(StringAttrs.apply) else List()

          //println((string, func, params, str, intParam))
          //println(str.exactValue)
          //if(!(string.tpe.widen <:< StringClass.tpe))
          //if((string.tpe.widen <:< StringClass.tpe))println((string, func, params, str, intParam))
                
          // We can get some information, even if the string is unknown
          //if(str == StringAttrs.empty) {
          //  Left(empty)
          //} else 
          func.toString match {
            case "size"|"length" if params.size == 0 => 
              Right(
                str.exactValue
                  .map(v => Values(v.size))
                  .getOrElse(
                    if(str.getMinLength == str.getMaxLength) 
                      Values(str.getMinLength)
                    else if(str.getMinLength == 0 && str.getMaxLength == Int.MaxValue)
                      Values.empty
                    else 
                      Values(str.getMinLength, str.getMaxLength)
                  ).addCondition(_ >= 0).addCondition(_ < str.getMaxLength)
              )
            case "toString" if params.size == 0 =>
              Left(toStringAttrs(string))
            case ("$plus"|"concat") if params.size == 1 =>
              //println(str.toString +" + "+toStringAttrs(params.head).toString + " == "+ (str + toStringAttrs(params.head)).toString)
              Left(str + toStringAttrs(params.head))
            case "$times" =>
              Left(str * intParam)

            case f @ ("init"|"tail") => 
              if(str.exactValue.isDefined) {
                if(str.exactValue.get.isEmpty) {
                  warn(treePosHolder, "Taking the "+f+" of an empty string.")
                  Left(empty)
                } else {
                  Left(new StringAttrs(str.exactValue.map(a => if(f=="init") a.init else a.tail)))
                }
              } else
                Left(new StringAttrs(minLength = math.max(str.minLength-1, 0), maxLength = if(str.maxLength != Int.MaxValue) math.max(str.maxLength-1, 0) else Int.MaxValue))

            case "capitalize" if params.size == 0 => Left(str.capitalize)
            case "distinct"   if params.size == 0 => Left(str.distinct)
            case "reverse"    if params.size == 0 => Left(str.reverse)
            case "count"      if params.size == 1 => 
              val out = Values.empty.addCondition(_ >= 0)
              Right(if(str.getMaxLength != Int.MaxValue) out.addCondition(_ < str.getMaxLength) else out)
            case "filter"     if params.size == 1 => 
              Left(str.removeExactValue.zeroMinLengths)
            
            case f @ ("indexOf"|"lastIndexOf") if params.size == 1 =>
              if(str.exactValue.isDefined && stringParam.exactValue.isDefined) {
                if(f == "indexOf")
                  Right(Values(str.exactValue.get.indexOf(stringParam.exactValue.get)))
                else
                  Right(Values(str.exactValue.get.lastIndexOf(stringParam.exactValue.get)))
              } else if(str.getMaxLength < Int.MaxValue) {
                Right(Values.empty.addConditions(_ >= -1, _ < str.getMaxLength))
              } else {
                Right(Values.empty.addConditions(_ >= -1))
              }

            //These come in (Char/String) versions
            case "stringPrefix" if params.size == 0 && str.exactValue.isDefined => Left(new StringAttrs(str.exactValue.map(_.stringPrefix)))
            case "stripLineEnd" if params.size == 0 && str.exactValue.isDefined => Left(new StringAttrs(str.exactValue.map(_.stripLineEnd)))
            case "stripMargin"  if params.size == 0 && str.exactValue.isDefined => Left(new StringAttrs(str.exactValue.map(_.stripMargin)))
            
            case "toUpperCase" => Left(str.toUpperCase)
            case "toLowerCase" => Left(str.toLowerCase)
            case "trim" =>  Left(str.trim)
            case "nonEmpty"|"isEmpty" => 
              if(str.alwaysNonEmpty) warn(treePosHolder, "This string will never be empty.")
              if(str.alwaysIsEmpty) warn(treePosHolder, "This string will always be empty.")
              Left(empty)
            case "hashCode" if str.exactValue.isDefined => 
              Right(Values(str.exactValue.get.hashCode))
            /// String to(Int,Long,Float,Double) conversion will likely fail (runtime exception)
            case "toInt" if str.exactValue.isDefined =>
              try {
                Right(Values(str.exactValue.get.toInt))
              } catch {
                case e: Exception =>
                  warn(treePosHolder, "This String toInt conversion will likely fail.")
                  Left(empty)
              }
            case f @ ("toLong") if str.exactValue.isDefined =>
              try {
                str.exactValue.get.toLong
              } catch {
                case e: Exception =>
                  warn(treePosHolder, "This String "+f+" conversion will likely fail.")
              }
              Left(empty)
            case f @ ("toDouble"|"toFloat") if str.exactValue.isDefined =>
              try {
                str.exactValue.get.toDouble
              } catch {
                case e: Exception =>
                  /// String to floating point conversion will likely fail (runtime exception)
                  warn(treePosHolder, "This String "+f+" conversion will likely fail.")
              }
              Left(empty)

            // str.func(String)
            case f @ ("charAt"|"codePointAt"|"codePointBefore"|"substring"
                     |"apply"|"drop"|"take"|"dropRight"|"takeRight") if intParam.isValue =>
              val param = intParam.getValue
              lazy val string = str.exactValue.get //lazy to avoid None.get... didn't use monadic, because I was lazy

              //println((string, param))
              
              //TODO use reflection, dummy :)
              //TODO: could do some prefix/suffix enhancements
              try f match {
                case "charAt"|"apply" => 
                  if(str.exactValue.isDefined) { string.charAt(param); Left(empty) } else if(param < 0) throw new IndexOutOfBoundsException else Left(empty)
                case "codePointAt" => 
                  if(str.exactValue.isDefined) Right(Values(string.codePointAt(param))) else if(param < 0) throw new IndexOutOfBoundsException else Left(empty)
                case "codePointBefore" => 
                  if(str.exactValue.isDefined) Right(Values(string.codePointBefore(param))) else if(param < 1) throw new IndexOutOfBoundsException else Left(empty)
                case "substring" => 
                  if(str.exactValue.isDefined) Left(new StringAttrs(Some(string.substring(param)))) else if(param < 0) throw new IndexOutOfBoundsException else Left(empty)
                case "drop" => 
                  if(str.exactValue.isDefined) 
                    Left(new StringAttrs(Some(string.drop(param)))) 
                  else 
                    Left(new StringAttrs(minLength = math.max(str.minLength-param, 0), maxLength = if(str.maxLength != Int.MaxValue) math.max(str.maxLength-param, 0) else Int.MaxValue))
                case "take" => 
                  if(str.exactValue.isDefined) 
                    Left(new StringAttrs(Some(string.take(param))))
                  else
                    Left(new StringAttrs(minLength = math.min(param, str.minLength), maxLength = math.max(param, 0)))
                case "dropRight" => 
                  if(str.exactValue.isDefined)
                    Left(new StringAttrs(Some(string.dropRight(param))))
                  else 
                    Left(new StringAttrs(minLength = math.max(str.minLength-param, 0), maxLength = if(str.maxLength != Int.MaxValue) math.max(str.maxLength-param, 0) else Int.MaxValue))
                case "takeRight" => 
                  if(str.exactValue.isDefined) 
                    Left(new StringAttrs(Some(string.takeRight(param))))
                  else 
                    Left(new StringAttrs(minLength = math.min(param, str.minLength), maxLength = math.max(param, 0)))
                case a => 
                  Left(empty)
              } catch {
                case e: IndexOutOfBoundsException =>
                  /// String index will likely cause an IndexOutOfBoundsException (runtime exception)
                  warn(params.head, "This index will likely cause an IndexOutOfBoundsException.")
                  Left(empty)
                case e: Exception =>
                  Left(empty)
              }
              
            //str.func(Int, Int)
            case f @ ("substring"|"codePointCount") if intParams.size == 2 && intParams.forall(_.isValue) =>
              lazy val string = str.exactValue.get //lazy to avoid None.get... didn't use monadic, because I was lazy
              val param = intParams.map(_.getValue)

              //println((string, param))
              
              try f match {
                case "substring" =>
                  if(str.exactValue.isDefined) 
                    Left(new StringAttrs(Some(string.substring(param(0), param(1)))))
                  else if(param(0) < 0 || param(1) < param(0) || param(0) >= str.getMaxLength || param(1) >= str.getMaxLength)
                    throw new IndexOutOfBoundsException
                  else if(param(0) == param(1))
                    Left(new StringAttrs(Some("")))
                  else
                    Left(empty)
                case "codePointCount" =>
                  if(str.exactValue.isDefined) 
                    Right(Values(string.codePointCount(param(0), param(1))))
                  else if(param(0) < 0 || param(1) < param(0) || param(0) >= str.getMaxLength || param(1) >= str.getMaxLength)
                    throw new IndexOutOfBoundsException
                  else
                    Left(empty)
                case a => Left(empty)
              } catch {
                case e: IndexOutOfBoundsException =>
                  warn(params.head, "This index will likely cause an IndexOutOfBoundsException.")
                  Left(empty)
                case e: Exception =>
                  Left(empty)
              }

            // str.func(String)
            /// Try to verify String contains, startsWith, endsWith
            case func @ ("contains"|"startsWith"|"endsWith"|"equals"|"$eq$eq"|"$bang$eq") =>
              val result = func match {
                case "contains"   => (str contains stringParam)
                case "startsWith" => (str startsWith stringParam)
                case "endsWith"   => (str endsWith stringParam)
                case "equals"|"$eq$eq" => (str equals stringParam)
                case "$bang$eq" => (str nequals stringParam)
                case _ => None
              }
              val function = if(func == "$eq$eq") "equals" else if(func == "$bang$eq") "not equals" else func
              if(result.isDefined) warn(params.head, "This "+function+" will always return "+result.get+".")
              
              Left(empty)
              
            //str.func(String, String)
            case f @ ("replaceAll") if stringParams.size == 2 && stringParams.forall(_.exactValue.isDefined) =>
              val (p0,p1) = (stringParams(0).exactValue.get, stringParams(1).exactValue.get)
              
              if(str.exactValue.isDefined) {
                try { 
                  Left(new StringAttrs(Some(str.exactValue.get.replaceAll(p0, p1))))
                } catch {
                  case e: java.util.regex.PatternSyntaxException =>
                    Left(empty)
                }
              } else if((p0 matches """\[[^\]]+\]""") && p1.size == 1) { //keeps the same length
                Left(str)
              } else if(p1 == "") { //length is 0..size
                Left(str.zeroMinLengths)
              } else {
                Left(empty)
              }
            /// String format checks (runtime exception)
            case f @ "format"
              if (params.nonEmpty) && !(params.head.tpe.widen <:< StringClass.tpe) && !(params.head.tpe.widen <:< definitions.getClass(newTermName("java.util.Locale")).tpe) => 
              //Ignore the default Java impl, just work with scala's format(Any*)
              //TODO: scrap the whole thing, and tell people to use string interpolators ;)
              
              val parActual = params map {
                case param if(param.tpe.widen <:< IntClass.tpe) => computeExpr(param)
                case param if(param.tpe.widen <:< StringClass.tpe) => StringAttrs(param)
                case Literal(Constant(x)) => x
                case _ => Values.empty
              }
              
              val areValues = parActual forall { 
                case v: Values => v.isValue
                case s: StringAttrs => s.exactValue.isDefined
                case _ => true
              }
              
              if(str.exactValue.isDefined && areValues) {
                try {
                  Left(new StringAttrs(exactValue = Some(str.exactValue.get.format(parActual map {
                    case v: Values => v.getValue
                    case s: StringAttrs => s.exactValue.get
                    case x => x
                  }:_*))))
                } catch {
                  case e: java.util.UnknownFormatConversionException =>
                    warn(string, "This string format will fail with: " + e.getMessage)
                    Left(empty)
                  case e: java.util.IllegalFormatConversionException if !e.getMessage.contains("!= java.lang.String") =>
                    warn(string, "This string format will fail with: " + e.getMessage)
                    Left(empty)
                  case e: java.util.MissingFormatArgumentException =>
                    warn(string, "This string format will fail with: " + e.getMessage)
                    Left(empty)
                  case e: Exception =>
                    Left(empty)
                }
              } else if(str.exactValue.isDefined && !areValues) {
                try {
                  str.exactValue.get.format()
                } catch {
                  case e: java.util.UnknownFormatConversionException =>
                    warn(string, "This string format will fail with: " + e.getMessage)
                  case e: Exception => 
                }
                Left(empty)
              } else {
                Left(empty)
              }
            case _ =>
              //if(str.exactValue.isDefined)println((str, func, params))
              Left(empty)
          }
        }

        
        //// Try to convert an AST into a String value
        def apply(tree: Tree): StringAttrs = {
          def traverseString(tree: Tree): StringAttrs = tree match {
            case Literal(Constant(null)) => new StringAttrs(exactValue = Some("null"))
            case Literal(Constant(c)) => 
              if(stringVals.filter(s => s.name.isDefined && !(vars contains s.name.get)).exists(_.exactValue == Some(c.toString))) {
                //warn(tree, "You have defined that string as a val already, maybe use that?")
              }

              new StringAttrs(exactValue = Some(c.toString))
              
            case Ident(name) =>
              stringVals
                .find(_.name.exists(_ == name.toString))
                .getOrElse(empty)
                
            case Apply(Ident(name), params) if defModels contains name.toString =>
              defModels
                .find(m => m._1 == name.toString && m._2.isRight)
                .map(_._2.right.get)
                .getOrElse(empty)
            
            case If(cond, expr1, expr2) =>
              val (e1, e2) = (traverseString(expr1), traverseString(expr2))
              
              if(expr1.tpe <:< NothingClass.tpe) {
                e2
              } else if(expr2.tpe <:< NothingClass.tpe) {
                e1
              } else {
                new StringAttrs(
                  minLength = math.min(e1.getMinLength, e2.getMinLength), 
                  trimmedMinLength = math.min(e1.getTrimmedMinLength, e2.getTrimmedMinLength),
                  maxLength = math.max(e1.getMaxLength, e2.getMaxLength), 
                  trimmedMaxLength = math.max(e1.getTrimmedMaxLength, e2.getTrimmedMaxLength))
              }

            case Apply(augmentString, List(expr)) if(augmentString.toString == "scala.this.Predef.augmentString") =>
              StringAttrs(expr)
              
            // Implicit toString
            case Apply(Select(expr1, nme.ADD), List(expr2)) if (expr1.tpe.widen <:< StringClass.tpe ^ expr2.tpe.widen <:< StringClass.tpe) =>
              toStringAttrs(expr1) + toStringAttrs(expr2)

            // Pass on functions on strings
            //TODO: maybe check if some return string and can be computed
            case Apply(Select(str, func), params) => 
              stringFunc(str, func, params).left.getOrElse(empty)

            case Select(Apply(scala_augmentString, List(string)), func) if (scala_augmentString.toString endsWith "augmentString") =>
              stringFunc(string, func).left.getOrElse(empty)

            case a => 
              //println(showRaw(a))
              empty
          }
          
          val a = traverseString(tree)
          //println("tree: "+ a)
          a
        }
      }
      class StringAttrs(
          val exactValue: Option[String] = None,
          val name: Option[String] = None, //Move to outer map?
          private val minLength: Int = 0,
          private val trimmedMinLength: Int = 0,
          private val maxLength: Int = Int.MaxValue,
          private val trimmedMaxLength: Int = Int.MaxValue,
          private var prefix: String = "",
          private var suffix: String = "",
          private var knownPieces: Set[String] = Set[String]()) {
        
        //keep this in mind, make getters
        if(exactValue.isDefined) {
          prefix = exactValue.get
          suffix = exactValue.get
          knownPieces = Set[String]()
        }
        //println(this)

        def equals(s: StringAttrs): Option[Boolean] = 
          if(s.getMinLength > this.getMaxLength || this.getMinLength > s.getMaxLength) Some(false)
          else if(s.exactValue.isDefined) this.equals(s.exactValue.get)
          else if(this.exactValue.isDefined) s.equals(this.exactValue.get)
          else None
        def equals(s: String): Option[Boolean] = {
          if(exactValue.isDefined) {
            Some(exactValue.get == s)
          } else {
            if((s.length < getMinLength)
            || (s.length > getMaxLength)
            || !(s startsWith prefix)
            || !(s endsWith suffix)
            || !(knownPieces forall { s contains _ }))
              Some(false)
            else
              None
          }
        }

        def nequals(s: StringAttrs): Option[Boolean] = equals(s).map(equal => !equal)
        def nequals(s: String): Option[Boolean] = equals(s).map(equal => !equal)

        def contains(s: StringAttrs): Option[Boolean] =
          if(s.getMinLength > getMaxLength) Some(false)
          else if(s.exactValue.isDefined) this.contains(s.exactValue.get)
          else None
        def contains(s: String): Option[Boolean] =
          if(exactValue.isDefined) Some(exactValue.get contains s)
          else if((prefix contains s) || (suffix contains s) || (knownPieces exists { _ contains s })) Some(true)
          else if(s.length > getMaxLength) Some(false)
          else None

        def startsWith(s: StringAttrs): Option[Boolean] = 
          if(s.getMinLength > getMaxLength) Some(false)
          else if(s.exactValue.isDefined) this.startsWith(s.exactValue.get)
          else None
        def startsWith(s: String): Option[Boolean] = 
          if(s.length > getMaxLength) Some(false)
          else if(prefix.isEmpty || s.length > prefix.length) None 
          else Some(prefix startsWith s)

        def endsWith(s: StringAttrs): Option[Boolean] = 
          if(s.getMinLength > getMaxLength) Some(false)
          else if(s.exactValue.isDefined) this.endsWith(s.exactValue.get)
          else None
        def endsWith(s: String): Option[Boolean] = 
          if(s.length > getMaxLength) Some(false)
          else if(suffix.isEmpty || s.length > suffix.length) None
          else Some(suffix endsWith s)

        def capitalize: StringAttrs = 
          new StringAttrs(
            exactValue = exactValue.map { _.capitalize },
            minLength = getMinLength,
            trimmedMinLength = getTrimmedMinLength,
            maxLength = getMaxLength,
            trimmedMaxLength = getTrimmedMaxLength,
            prefix = prefix.capitalize,
            suffix = suffix,
            knownPieces = knownPieces filterNot { _ contains suffix })

        def distinct: StringAttrs = 
          new StringAttrs(
            exactValue = exactValue.map { _.distinct },
            minLength = math.min(getMinLength, 1),
            maxLength = getMaxLength,
            trimmedMaxLength = getTrimmedMaxLength)

        def reverse: StringAttrs = 
          new StringAttrs(
            exactValue = exactValue.map { _.reverse },
            minLength = getMinLength,
            trimmedMinLength = getTrimmedMinLength,
            maxLength = getMaxLength,
            trimmedMaxLength = getTrimmedMaxLength,
            prefix = suffix.reverse,
            suffix = prefix.reverse,
            knownPieces = knownPieces map { _.reverse })

        def trim: StringAttrs = {
          val newMinLength = exactValue.map(_.trim.size).getOrElse(getTrimmedMinLength)
          val newMaxLength = exactValue.map(_.trim.size).getOrElse(getTrimmedMaxLength)
          new StringAttrs(
            exactValue = exactValue.map { _.trim },
            minLength = newMinLength, 
            trimmedMinLength = newMinLength,
            maxLength = newMaxLength, 
            trimmedMaxLength = newMaxLength)//TODO: prefix/suffix trimleft/right
        }

        def toUpperCase: StringAttrs = 
          new StringAttrs(
            exactValue = exactValue.map { _.toUpperCase },
            minLength = getMinLength,
            trimmedMinLength = getTrimmedMinLength,
            maxLength = getMaxLength,
            trimmedMaxLength = getTrimmedMaxLength,
            prefix = prefix.toUpperCase,
            suffix = suffix.toUpperCase,
            knownPieces = knownPieces map { _.toUpperCase })
        def toLowerCase: StringAttrs = 
          new StringAttrs(
            exactValue = exactValue.map { _.toLowerCase },
            minLength = getMinLength,
            trimmedMinLength = getTrimmedMinLength,
            maxLength = getMaxLength,
            trimmedMaxLength = getTrimmedMaxLength,
            prefix = prefix.toLowerCase,
            suffix = suffix.toLowerCase,
            knownPieces = knownPieces map { _.toLowerCase })

        def addName(name: String): StringAttrs = new StringAttrs(exactValue, Some(name), getMinLength, trimmedMinLength, getMaxLength, trimmedMaxLength, prefix, suffix, knownPieces)
        def removeExactValue: StringAttrs = new StringAttrs(None, name, getMinLength, trimmedMinLength, getMaxLength, trimmedMaxLength)
        def zeroMinLengths: StringAttrs = new StringAttrs(exactValue, name, 0, 0, getMaxLength, trimmedMaxLength, prefix, suffix, knownPieces)
        
        def alwaysIsEmpty: Boolean = getMaxLength == 0
        def alwaysNonEmpty: Boolean = getMinLength > 0
        
        def getMinLength: Int = exactValue.map(_.size).getOrElse(minLength)
        def getMaxLength: Int = exactValue.map(_.size).getOrElse(maxLength)
        def getTrimmedMinLength: Int = exactValue.map(_.trim.size).getOrElse(trimmedMinLength)
        def getTrimmedMaxLength: Int = exactValue.map(_.trim.size).getOrElse(trimmedMaxLength)
        
        def +(s: String): StringAttrs = 
          new StringAttrs(
            exactValue = if(this.exactValue.isDefined) Some(this.exactValue.get + s) else None,
            minLength = this.getMinLength + s.length,
            trimmedMinLength = this.getTrimmedMinLength + s.trim.length, //TODO: can be made more exact
            maxLength = if(this.maxLength == Int.MaxValue) Int.MaxValue else this.getMaxLength + s.length,
            trimmedMaxLength = 
              if(this.getTrimmedMaxLength == Int.MaxValue) Int.MaxValue 
              //else if(this.exactValue.isDefined) (this.exactValue.get + s).trim.size // This case is covered in getTrimmedMaxLength if exactValue is known
              else this.getMaxLength + s.length,
            prefix = if(this.exactValue.isDefined) this.exactValue.get + s else this.prefix,
            suffix = this.suffix + s,
            knownPieces = this.knownPieces)

        def +(s: StringAttrs): StringAttrs = 
          new StringAttrs(
            exactValue = if(this.exactValue.isDefined && s.exactValue.isDefined) Some(this.exactValue.get + s.exactValue.get) else None,
            minLength = this.getMinLength + s.getMinLength,
            trimmedMinLength = this.getTrimmedMinLength + s.getTrimmedMinLength, //TODO: can be made more exact
            maxLength = if(this.getMaxLength == Int.MaxValue || s.getMaxLength == Int.MaxValue) Int.MaxValue else this.getMaxLength + s.getMaxLength,
            trimmedMaxLength = 
              if(this.getTrimmedMaxLength == Int.MaxValue || s.getTrimmedMaxLength == Int.MaxValue) Int.MaxValue 
              //else if(this.isDefined && s.isDefined) (this.exactValue.get + s.exactValue.get).trim.size // This case is covered in getTrimmedMaxLength if exactValue is known
              else this.getMaxLength + s.getMaxLength,
            prefix = if(this.exactValue.isDefined) this.exactValue.get + s.prefix else this.prefix,
            suffix = if(s.exactValue.isDefined) this.suffix + s.suffix else s.suffix,
            knownPieces = this.knownPieces ++ s.knownPieces + (this.suffix + s.prefix))

        /// String multiplication with value <= 0 warning
        def *(n: Values): StringAttrs = {
          if(n.isValue) {
            this * n.getValue
          } else if(n.nonEmpty) {
            if(n forallLower 2) {
              if(n.max == 1) {
                new StringAttrs(
                  minLength = 0,
                  trimmedMinLength = 0,
                  maxLength = this.getMaxLength,
                  trimmedMaxLength = this.getTrimmedMaxLength)
              } else {
                warn(treePosHolder, "Multiplying a string with a value <= 0 will result in an empty string.")
                new StringAttrs(exactValue = Some(""))
              }
            } else {
              new StringAttrs(
                minLength = if(n.min > 0) this.getMinLength*n.min else 0,
                trimmedMinLength = if(n.min > 0) this.getTrimmedMinLength*n.min else 0)
            }
          } else {
            StringAttrs.empty
          }
        }
        def *(n: Int): StringAttrs = 
          if(n <= 0) {
            warn(treePosHolder, "Multiplying a string with a value <= 0 will result in an empty string.")
            new StringAttrs(Some(""))
          } else {
            new StringAttrs(
              exactValue = if(this.exactValue.isDefined) Some(this.exactValue.get*n) else None,
              minLength = this.getMinLength*n,
              trimmedMinLength = this.getTrimmedMinLength*n, //ADD: can be made more exact
              maxLength = if(this.getMaxLength == Int.MaxValue) Int.MaxValue else this.getMaxLength*n,
              trimmedMaxLength = if(this.getTrimmedMaxLength == Int.MaxValue) Int.MaxValue else this.getTrimmedMaxLength*n,
              prefix = this.prefix,
              suffix = this.suffix,
              knownPieces = this.knownPieces ++ (if(n >= 2) Set(this.suffix+this.prefix) else Nil))
          }
        
        override def hashCode: Int = exactValue.hashCode + name.hashCode + minLength + trimmedMinLength + maxLength + trimmedMaxLength
        override def equals(that: Any): Boolean = that match {
          case s: StringAttrs => (this.exactValue.isDefined && s.exactValue.isDefined && this.exactValue.get == s.exactValue.get)
          case s: String => this.exactValue.exists(_ == s)
          case _ => false
        }
        
        override def toString: String = 
         "StringAttrs" + (if(exactValue.isDefined) "("+exactValue.get+")" else (name, getMinLength, getTrimmedMinLength, getMaxLength, getTrimmedMaxLength, prefix, suffix, knownPieces))
      }
     
      def traverseBlock(tree: Tree) {
        pushDefinitions()
        traverse(tree)
        popDefinitions()
      }

      override def traverse(tree: Tree) {
        if(doNotTraverse contains tree) return
        treePosHolder = tree
        tree match {
          /// Very hacky support for some var interpretion
          /*case ValDef(m: Modifiers, varName, _, value) if(m.hasFlag(MUTABLE)) =>
            vars += varName.toString
            vals(varName.toString) = computeExpr(value)
            //println("assign: "+(vals))*/
          case Assign(varName, value) if vars contains varName.toString =>
            vals += varName.toString -> computeExpr(value)
            //println("reassign: "+(vals))
          
          case LabelDef(label, List(), If(cond, body, unit)) if {
            //discardVars(cond, (vars & getUsed(cond)).toSeq:_*)
            //discardVars(body)
            discardVars(tree, (vars & getUsed(tree)))
            labels += label.toString -> tree
            false
          } => //Fallthrough
          case Apply(label, List()) if (labels contains label.toString) && {
            // TODO: check if there is an infinite...
            // vals.filter(a => vars contains a._1).map(a =>println(a._1, a._2.applyCond(labels(label.toString))._2))
            
            discardVars(labels(label.toString))//, (vars & getUsed(labels(label.toString))).toSeq:_*)

            labels -= label.toString
            false
          } => //Fallthrough
          case e if { //throw away assigns inside other blocks
            discardVars(e)
            false
          } => //Fallthrough

          case forloop @ Apply(TypeApply(Select(collection, _), _), List(Function(List(ValDef(_, _, _, _)), _))) =>
            forLoop(forloop)
          
          /// Assertions checks (assert, assume, require)
          case Apply(Select(scala_Predef, assertion), List(condExpr)) 
            if (scala_Predef.tpe.widen <:< PredefModule.tpe) && (assertion.toString matches "assert|assume|require") => 
            
            // we can apply these conditions to vals - if they don't hold, it'll throw an exception anyway
            // and they'll reset at the end of the current block
            vals = vals.map(a => (a._1, a._2.applyCond(condExpr)._1)).withDefaultValue(Values.empty)
          
          /// String checks
          //case s @ Literal(Constant(str: String)) if stringVals.filter(s => s.name.isDefined && !(vars contains s.name.get)).find(_.exactValue == Some(str)).isDefined =>
            //warn(s, "You have defined that string as a val already, maybe use that?")

          case ValDef(m: Modifiers, valName, _, s @ Literal(Constant(str: String))) if(!m.isMutable && !m.isFinal && !m.hasDefault) =>
            //if(stringVals.filter(s => s.name.isDefined && !(vars contains s.name.get)).exists(_.exactValue == Some(str)))
              //warn(s, "You have defined that string as a val already, maybe use that?")
            //stringVals += str
            
            val str2 = StringAttrs(s).addName(valName.toString)
            //println("str2: "+str2)
            if(str2.exactValue.isDefined || str2.getMinLength > 0) {
              stringVals += str2
            }
            //println("stringVals2: "+stringVals)

          case ValDef(m: Modifiers, valName, _, Literal(Constant(a: Int))) if(!m.isMutable && !m.hasDefault) =>
            val valNameStr = valName.toString.trim
            vals += valNameStr -> Values(a, valNameStr)
            //println(vals(valName.toString))

          case v@ValDef(m: Modifiers, valName, _, expr) if(!m.hasDefault) => 
            //if !m.hasFlag(MUTABLE) /*&& !m.hasFlag(LAZY)) && !computeExpr(expr).isEmpty*/ => //&& computeExpr(expr).isValue =>
            //ADD: aliasing... val a = i, where i is an iterator, then 1/i-a is divbyzero
            //ADD: isSeq and actualSize

            if(expr.tpe.widen <:< StringClass.tpe) {
              val str = StringAttrs(expr).addName(valName.toString)//StringAttrs.toStringAttrs(expr)
              //println("str1: "+str)
              if(str.exactValue.isDefined || str.getMinLength > 0) {
                //println(str)
                stringVals += str
              }
              //println("stringVals1: "+stringVals)
            }

            if(!(m.isPrivateLocal && m.isMutable)) { // private[this] var k = 4 messes up a few things
              val valNameStr = valName.toString
              val res = computeExpr(expr).addName(valNameStr)
              vals += valNameStr -> res
              if(m.isMutable) vars += valNameStr
            }
            
           
            //println("newVal: "+computeExpr(expr).addName(valNameStr))
            //println("newVal: "+vals(valName.toString))
            //println(showRaw(expr))
            
            expr match {
              case e => //Block(_, _) | If(_,_,_) =>
                //println(expr)
                pushDefinitions()
                
                traverse(expr)
                
                popDefinitions()
              //case _ =>
            }
          
          case Match(pat, cases) if pat.tpe.toString != "Any @unchecked" && cases.size >= 2 =>
            for(c <- cases) {
              pushDefinitions()

              //TODO: c.pat can override some variables
              traverse(c.body)
      
              popDefinitions()          
            }
          
          case If(condExpr, t, f) => //TODO: moved to computeExpr?
            val backupVals = vals
            val backupStrs = stringVals
            pushDefinitions()
            
            //println(vals)
            vals = backupVals.map(a => (a._1, a._2.applyCond(condExpr)._1)).withDefaultValue(Values.empty)
            //println(vals)
            //ADD: if always true, or always false pass the return value, e.g. val a = 1; val b = if(a == 1) 5 else 4
            super.traverse(t)

            popDefinitions()
            pushDefinitions()

            vals = backupVals.map(a => (a._1, a._2.applyCond(condExpr)._2)).withDefaultValue(Values.empty)
            super.traverse(f)
            
            popDefinitions()
            //println(vals)

          /// Attempt to verify collection indices are correct
          //case pos @ Apply(Select(Ident(seq), apply), List(indexExpr)) 
          case pos @ Apply(Select(seq, apply), List(indexExpr)) if methodImplements(pos.symbol, SeqLikeApply) =>
            //println(seq.toString)
            //println("indexExpr: "+computeExpr(indexExpr))
            //println(showRaw(indexExpr))
            if(vals.contains(seq.toString) && vals(seq.toString).actualSize != -1 && (computeExpr(indexExpr).existsGreater(vals(seq.toString).actualSize-1))) {
              warn(pos, "You will likely use a too large index for a collection here.")
            }
            if(computeExpr(indexExpr).existsLower(0)) {
              warn(pos, "You will likely use a negative index for a collection here.")
            }
          
          case DefDef(_, name, _, params, _, block @ Block(b, last)) => 
            //if you want to model one expr funcs - here's a start
            /*val (block, last) = body match {
              case block @ Block(b, last) => (block, last)
              case expr => (EmptyTree, expr)
            }*/
            
            pushDefinitions()

            //TODO: handle params
            val paramNames = params.flatten.map(_.name.toString)        
            vals = vals.filterNot(paramNames contains _._1)
            discardVars()
            doNotTraverse ++= params.flatten
            super.traverse(block)
            
            val returnVal = last match {
              case Return(ret) => 
                def otherReturns: Boolean = { 
                  for(Return(ret) <- b) return true
                  false
                }
                /// Unnecessary use of return keyword
                if(otherReturns) warn(last, "Scala has implicit return, you don't need a return statement at the end of a method")
                ret
              case a => 
                a
            }
            
            //defModels = backupDefModels

            /// Method always returns the same value
            if(returnCount(block) == 0 && throwsCount(block) == 0) {//ADD: can be made better - if sentences are easy to model
              val retVal = computeExpr(returnVal)
              if(retVal.isValue || (retVal.isSeq && retVal.size > 0)) {
                warn(last, "This method always returns the same value: "+retVal.getValue)
              }
              popDefinitions()
              if(retVal.nonEmpty || retVal.conditions.nonEmpty || (retVal.isSeq && retVal.actualSize != -1)) {
                //println("ModeledV: "+name.toString+" "+retVal)
                defModels += name.toString -> Left(retVal)
              } else {
                val retVal = StringAttrs(returnVal)
                if((retVal.getMinLength > 0 || retVal.getMaxLength < Int.MaxValue) && !(name.toString matches "[<$]init[$>]")) {
                  //println("ModeledS: "+name.toString+" "+retVal)
                  defModels += name.toString -> Right(retVal)
                }
              }
            } else {
              popDefinitions()
            }

          /// Invalid regex (runtime exception)
          case Apply(java_util_regex_Pattern_compile, List(regExpr)) if java_util_regex_Pattern_compile.toString == "java.util.regex.Pattern.compile" =>
            treePosHolder = regExpr
            StringAttrs(regExpr).exactValue.foreach(checkRegex)
          
          case Apply(Select(str, func), List(regExpr)) if (str.tpe.widen <:< StringClass.tpe) && (func.toString matches "matches|split") =>
            treePosHolder = regExpr
            StringAttrs(regExpr).exactValue.foreach(checkRegex)
            
          case Apply(Select(str, func), List(regExpr, str2)) if (str.tpe.widen <:< StringClass.tpe) && (func.toString matches "replace(All|First)") =>
            treePosHolder = regExpr
            StringAttrs(regExpr).exactValue.foreach(checkRegex)

          case Select(Apply(scala_Predef_augmentString, List(regExpr)), r)
            if(scala_Predef_augmentString.toString.endsWith(".augmentString") && r.toString == "r") =>
            treePosHolder = regExpr
            StringAttrs(regExpr).exactValue.foreach(checkRegex)
            
          /// Checks conditions that use Option.size (there is a separate check for all uses of Option.size)
          //ADD: Generalize... move to applyCond completely, make it less hacky
          case t @ Apply(Select(Select(Apply(option2Iterable, List(opt)), size), op), List(expr))
            if (option2Iterable.toString contains "Option.option2Iterable") && size.toString == "size" && t.tpe.widen <:< BooleanClass.tpe =>

            pushDefinitions()

            val valName = "__foobar__" //shoot me :P
            
            vals += valName -> (new Values(values = Set(0,1)))
            
            val cond = Apply(Select(Ident(newTermName(valName)), op), List(expr))
            cond.pos = t.pos
            
            vals(valName).applyCond(cond)
            
            popDefinitions()

          case b @ Block(stmts, ret) => 
            //println("block: "+b)
            pushDefinitions()

            super.traverse(tree)
            
            /// Try to use vars - TODO: is probably buggy
            /*val block = stmts :+ ret
            val vars = mutable.HashSet[String]()
            block foreach {
              case ValDef(m: Modifiers, varName, _, value) if(m.hasFlag(MUTABLE)) =>
                vars += varName.toString
                vals(varName.toString) = computeExpr(value)
                println("assign: "+(vals))
              case Assign(varName, value) if vars contains varName.toString =>
                vals(varName.toString) = computeExpr(value)
                println("reassign: "+(vals))
              case e => //throw away assigns inside other blocks
                for(v <- vars; if isAssigned(e, v)) {
                  vals(v) = Values.empty
                  println("discard: "+(vals))
                }
            }
            */
            
            popDefinitions()

          /// Pass on expressions
          case a =>
            //TODO: Range and Lists work too, I think
            if(a.tpe != null && ((a.tpe <:< StringClass.tpe) || (a.tpe <:< AnyValClass.tpe))) computeExpr(a)
            
            //if(vals.nonEmpty)println("in: "+showRaw(tree))
            //if(vals.nonEmpty)println(">   "+vals);
            //if(showRaw(tree).startsWith("Literal") || showRaw(tree).startsWith("Constant"))println("in: "+showRaw(tree))
            //tree.children.foreach(traverse)
            super.traverse(tree)
        }
        //super.traverse(tree)
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
