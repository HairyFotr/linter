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

package org.psywerx.hairyfotr

import scala.collection.mutable
import scala.tools.nsc.plugins.{ Plugin, PluginComponent }
import scala.tools.nsc.{ Global, Phase, Properties }

final class LinterPlugin(val global: Global) extends Plugin {
  import org.psywerx.hairyfotr.Utils._
  import global._

  val name = "linter"
  val description = "a static analysis compiler plugin to help protect against bugs and style problems"
  val components: List[PluginComponent] =
    if (settings.isInstanceOf[scala.tools.nsc.doc.Settings]) {
      inform("This is a scaladoc compilation, Linter is disabled.")
      List()
    } else {
      List(PreTyperComponent, PostTyperComponent, PostTyperInterpreterComponent, PostRefChecksComponent)
    }

  override val optionsHelp: Option[String] = Some(Seq(
    LinterOptions.DisableArgument+":plus+separated+warning+names",
    LinterOptions.EnableOnlyArgument+":plus+separated+warning+names",
    LinterOptions.PrintWarningNames+":true|false"
  ).map(" -P:" + name + ":" +  _).mkString("\n"))

  override def processOptions(options: List[String], error: String => Unit): Unit = {
    LinterOptions.parse(options) match {
     case Left(errorMessage) => error(errorMessage)
     case Right(linterOptions) =>
       Utils.linterOptions = linterOptions
    }
  }

  val inferred = mutable.HashSet[Position]() // Used for a scala 2.9 hack (can't find out which types are inferred)
  val intLiteralDiv = mutable.HashSet[Position]()

  private[this] object PreTyperComponent extends PluginComponent {
    val global = LinterPlugin.this.global
    import global._

    override val runsAfter = List("parser")

    val phaseName = "linter-parsed"

    private[this] val sealedTraits = mutable.Map.empty[Name, Tree]
    private[this] val usedTraits = mutable.Set.empty[Name]
    private[this] var inTrait = false
    private[this] def resetTraits(): Unit = {
      sealedTraits.clear()
      usedTraits.clear()
      inTrait = false
    }
    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: global.CompilationUnit): Unit = {
        if (!unit.isJava) {
          resetTraits()
          nowarnPositions.clear
          new PreTyperTraverser(unit).traverse(unit.body)
          //println((sealedTraits, usedTraits))
          for (unusedTrait <- sealedTraits.filterNot(st => usedTraits.exists(_.toString == st._1.toString))) {
            //TODO: It might still be used in some type-signature somewhere... see scalaz
            warn(unusedTrait._2, UnextendedSealedTrait)(unit)
          }
        }
      }
    }

    class PreTyperTraverser(unit: CompilationUnit) extends Traverser {
      implicit val unitt: CompilationUnit = unit

      var superTraverse = true
      def catcher(): PartialFunction[Throwable, Unit] = {
        case _: NullPointerException => //Ignore
        case _: NoSuchMethodError => //Ignore
        case _: StackOverflowError => superTraverse = false
        case e: Exception => //e.printStackTrace//TODO: Print details and ask user to report it
      }
      def finalizer(tree: Tree): Unit = {
        if (superTraverse) try { super.traverse(tree) } catch catcher
      }
      override def traverse(tree: Tree): Unit = try {
        superTraverse = true
        //if (showRaw(tree).contains("Hello"))println(showRaw(tree))
        tree match {
          /// quick hack for nowarnMergeNestedIfsPositions, see issue #18
          case If(_, apply @ Apply(_, _), Literal(Constant(())))
            if !(linterOptions.disabledWarningNames contains MergeNestedIfs.name)
            && { nowarnMergeNestedIfsPositions += apply.pos; false } => //Fallthrough

          /// Unused sealed traits (Idea by edofic)
          case ClassDef(mods, _name, _, Template(extendsList, _, body)) if !mods.isSealed && mods.isTrait =>
            for (Ident(traitName) <- extendsList) usedTraits += traitName
            inTrait = true
            for (stmt <- body) traverse(stmt)
            inTrait = false
            superTraverse = false; return

          // Typeparams (3rd param) are sometimes used for type-checking hacks
          case ClassDef(mods, name, Nil, Template(extendsList, _, body)) if mods.isSealed && mods.isTrait && !inTrait =>
            sealedTraits += name -> tree
            for (Ident(traitName) <- extendsList if traitName.toString != name.toString) usedTraits += traitName
            for (stmt <- body) traverse(stmt)
            superTraverse = false; return

          case ClassDef(_mods, _, _, Template(extendsList, _, body)) => //if mods.hasFlag(CASE) =>
            for (Ident(traitName) <- extendsList) usedTraits += traitName
            for (stmt <- body) traverse(stmt)
            superTraverse = false; return

          case ModuleDef(_mods, _name, Template(extendsList, _, body)) =>
            for (Ident(traitName) <- extendsList) usedTraits += traitName
            for (stmt <- body) traverse(stmt)
            superTraverse = false; return


          /// Warn on Nothing/Any or M[Nothing/Any] (idea by OlegYch)
          case ValDef(mods, _, tpe, _)
            if !mods.isParameter
            && tpe.toString == "<type ?>" =>

            inferred += tpe.pos

          //case DefDef(mods: Modifiers, name, _, valDefs, typeTree, body) =>
            //if (name.toString != "<init>" && !body.isEmpty && !mods.isAnyOverride) {
              /// Recursive call with exactly the same params
              //TODO: Currently doesn't cover shadowing or mutable changes of params, or the method shadowing/overriding
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
            /*if (mods.isImplicit && typeTree.isEmpty && !(name.toString matches "((i?)to).+|.*(To|2)[A-Z].*")) {
              warn(tree, "Implicit method %s needs explicit return type" format name)
            }*/

          /// Detect literal integer division
          case Apply(Select(Literal(Constant(_num: Int)), op), List(Literal(Constant(_denom: Int))))
            if (op == nme.DIV || op == nme.MOD) =>

            if (op == nme.DIV && math.abs(_denom) > math.abs(_num)) {
              //TODO: should also be implemented in absinterpreter, but keep here because of expression simplifier
              //TODO: should all simplifiable expressions warn?
              warn(tree, OperationAlwaysProducesZero("integer division"))
            }

            intLiteralDiv += tree.pos

          /// Loss of precision on literals
          // Moved here because ConstantFolder in typechecker messes it up
          //TODO: "regular expression below can be used to screen the input string" http://docs.oracle.com/javase/6/docs/api/java/lang/Double.html#valueOf(java.lang.String)
          case treeLiteral @ Literal(Constant(literal)) if literal != null && literal != (()) && (literal match { case _: Double | _: Float => true; case _ => false; }) => //&& isFloatingPointType(tree) =>

            val tpe =
              (literal match {
                case _: Double => "Double"
                case _: Float => "Float"
                case _ => "floating point type" })

            val (strLiteral, actualLiteral) = {
                // Get the actual token from code
                val floatRegex = "[-+]?[0-9.]+([eE][-+]?[0-9]+)?[dfDF]?".r
                var token = ""
                try {
                  val pos = treeLiteral.pos
                  token = floatRegex.findPrefixMatchOf(pos.lineContent.substring(pos.column - 1)).get.toString
                  token.toDouble.toString // trigger NumberFormatException
                } catch {
                  case _: UnsupportedOperationException => // Happens if tree doesn't have a position
                  case _: NumberFormatException => // Could warn here, but I don't trust the above code enough
                  case _: MatchError => // Float regex failed
                  case _: Exception =>
                    //println(treeLiteral.pos.lineContent)
                    //e.printStackTrace
                }
                //TODO: this is more correct, but needs some work
                //("%.1080f".format(literal), token)
                (literal.toString, token)
              }

            try {
              // Ugly hack to get around a few different representations: 0.005, 5E-3, ...
              def cleanString(s: String): String = s.replaceAll("^[-+]|[.]|[Ee].*$|[fdFD-]*$|(0[.])?0*", "")

              if (!strLiteral.isEmpty && !actualLiteral.isEmpty
              && (strLiteral matches ".*[0-9].*") && (actualLiteral matches ".*[0-9].*")
              && cleanString(strLiteral) != cleanString(actualLiteral)
              && ((if (strLiteral.toDouble < 0) "-" else "") + actualLiteral) != strLiteral)
                warn(treeLiteral, PossibleLossOfPrecision("Literal cannot be represented exactly by "+tpe+". ("+(if (strLiteral.toDouble < 0) "-" else "") + actualLiteral+" != "+strLiteral.replaceAll("0+$", "0")+")"))

            } catch {
              case _: NumberFormatException => //Ignore
            }

          case _ => //Ignore
        }
      } catch catcher finally finalizer(tree)
    }
  }

  private[this] object PostTyperComponent extends PluginComponent {
    val global = LinterPlugin.this.global
    import global._

    override val runsAfter = List("typer")

    val phaseName = "linter-typed"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: global.CompilationUnit): Unit = {
        if (!unit.isJava) {
          nowarnPositions.clear
          new PostTyperTraverser(unit).traverse(unit.body)
        }
      }
    }

    class PostTyperTraverser(unit: CompilationUnit) extends Traverser {
      implicit val unitt: CompilationUnit = unit
      val utils = new Utils[global.type](global)
      import utils._

      import definitions.{ AnyClass, NothingClass, ObjectClass, Object_== }
      import definitions.{ OptionClass, SeqClass, TraversableClass, ListClass, StringClass }
      import definitions.{ DoubleClass, FloatClass, CharClass, ByteClass, ShortClass, IntClass, LongClass, BooleanClass }

      val MutableSeqLike: Symbol = rootMirror.getClassByName(newTermName("scala.collection.mutable.SeqLike"))

      val ExceptionClass = rootMirror.getClassByName(newTermName("java.lang.Exception"))

      val JavaConversionsModule: Symbol = rootMirror.getModuleByName(newTermName("scala.collection.JavaConversions"))
      val SeqLikeClass: Symbol = rootMirror.getClassByName(newTermName("scala.collection.SeqLike"))
      val SeqLikeContains: Symbol = SeqLikeClass.info.member(newTermName("contains"))
      val SeqLikeApply: Symbol = SeqLikeClass.info.member(newTermName("apply"))
      val MapFactoryClass = rootMirror.getClassByName(newTermName("scala.collection.generic.MapFactory"))
      val TraversableFactoryClass: Symbol = rootMirror.getClassByName(newTermName("scala.collection.generic.TraversableFactory"))
      val TraversableOnceClass: Symbol = rootMirror.getClassByName(newTermName("scala.collection.TraversableOnce"))
      val BigIntClass: Symbol = rootMirror.getClassByName(newTermName("scala.math.BigInt"))
      val JavaIntegerClass: Symbol = rootMirror.getClassByName(newTermName("java.lang.Integer"))

      val integerTypes = Set(JavaIntegerClass.tpe, BigIntClass.tpe, IntClass.tpe)

      val OptionGet: Symbol = OptionClass.info.member(nme.get)

      val IsInstanceOf = AnyClass.info.member(nme.isInstanceOf_)
      val AsInstanceOf = AnyClass.info.member(nme.asInstanceOf_)
      val ToString: Symbol = AnyClass.info.member(nme.toString_)

      def seqMemberType(seenFrom: Type): Type = {
        if (seenFrom.baseClasses.exists(_.tpe =:= SeqLikeClass.tpe))
          SeqLikeClass.tpe.typeArgs.head.asSeenFrom(seenFrom, SeqLikeClass)
        else
          OptionClass.tpe.typeArgs.head.asSeenFrom(seenFrom, OptionClass)
      }

      def isSubtype(x: Tree, y: Tree): Boolean = { x.tpe.widen <:< y.tpe.widen }
      def isSubtype(x: Tree, y: Type): Boolean = { x.tpe.widen <:< y.widen }

      def isIntegerType(tpe: Type): Boolean =
          (tpe <:< ByteClass.tpe
        || tpe <:< ShortClass.tpe
        || tpe <:< IntClass.tpe
        || tpe <:< LongClass.tpe)
      def isIntegerType(x: Tree): Boolean = isIntegerType(x.tpe.widen)

      def isFloatingPointType(tpe: Type): Boolean =
          (tpe <:< FloatClass.tpe
        || tpe <:< DoubleClass.tpe)
      def isFloatingPointType(x: Tree): Boolean = isFloatingPointType(x.tpe.widen)

      def methodImplements(method: Symbol, target: Symbol): Boolean =
        try { method == target || method.allOverriddenSymbols.contains(target) } catch { case _: NullPointerException => false }

      def isGlobalImport(selector: ImportSelector): Boolean = {
        selector.name == nme.WILDCARD && selector.renamePos == -1
      }

      def containsAnyType(tpe: Type): Boolean = (tpe =:= AnyClass.tpe || tpe.typeArgs.exists(_ =:= AnyClass.tpe))
      def containsNothingType(tpe: Type): Boolean = (tpe =:= NothingClass.tpe || tpe.typeArgs.exists(_ =:= NothingClass.tpe))

      def isOptionOption(t: Tree): Boolean =
        (t.tpe.widen.baseClasses.exists(_.tpe =:= OptionClass.tpe)
        && t.tpe.widen.typeArgs.exists(_.widen.baseClasses.exists(_.tpe =:= OptionClass.tpe)))

      def isIdent(t: Tree, recursive: Boolean = false): Boolean = t match {
        case Ident(_) => true
        case Select(t2, _TermName) if recursive => isIdent(t2, recursive)
        case _ => false
      }

      def getUsed(tree: Tree): Set[String] = (for (Ident(id) <- tree) yield id.toString).toSet
      def getAssigned(tree: Tree): Set[String] = {
        (for (Assign(Ident(id), _) <- tree) yield id.toString).toSet
        //TODO: non-local stuff (for (Apply(Select(id, setter), List(_)) <- tree; if setter.toString endsWith "_$eq") yield setter.dropRight(4)).toSet
      }

      //TODO: Still possible to have false positives...
      // val i = 4; val f: Double = 2.3 + List(1/i).sum
      def hasIntegerDivision(tree: Tree): Boolean = {
        def hasIntDiv: Boolean = tree exists { // <- this causes FP's
          case Apply(Select(param1, nme.DIV), List(param2)) if isIntegerType(param1) && isIntegerType(param2) => true
          case _ => false
        }
        def isFloatConversion: Boolean = tree match { // for Int expressions... .toDouble is implicitly added
          case Select(num, Name("toFloat"|"toDouble")) if isIntegerType(num) => true
          case _ => false
        }
        def hasIntOpFloat: Boolean = tree exists { //TODO: detect implicit widening, e.g. float + int
          case Apply(Select(param1, _op), List(param2))
            if (isIntegerType(param1) && isFloatingPointType(param2)) || (isFloatingPointType(param1) && isIntegerType(param2)) => true
          case _ => false
        }

        hasIntDiv && (isFloatConversion || hasIntOpFloat)
      }

      def identOrDefault(tree: Tree, default: String): String = {
        tree match {
          case Ident(_) if !tree.contains(".") =>

            tree.toString

          case Apply(scala_Predef_augmentString, List(str @ Ident(name)))
            if str.tpe <:< StringClass.tpe
            && (scala_Predef_augmentString endsWith "Predef.augmentString") =>

            name.toString

          case _ => default
        }
      }
      def identOrCol(tree: Tree): String = identOrDefault(tree, "col")
      def identOrOpt(tree: Tree): String = identOrDefault(tree, "opt")

      // Returns the string subtree of a string.length/size subtree
      def getStringFromLength(t: Tree): Option[Tree] = t match {
        case Apply(Select(str, Name("length")), Nil)
          if str.tpe <:< StringClass.tpe =>

          Some(str)

        case Select(Apply(scala_Predef_augmentString, List(str)), Name("size"))
          if str.tpe <:< StringClass.tpe
          && (scala_Predef_augmentString endsWith "Predef.augmentString") =>

          Some(str)

        case _ =>
          None
      }

      import java.math.MathContext
      val java_math_MathContext = rootMirror.getClassByName(newTermName("java.math.MathContext"))
      def getMathContext(t: Tree): Option[MathContext] = t match {
        case Apply(Select(New(_mathcontext), nme.CONSTRUCTOR), (Literal(Constant(precision: Int)) :: _roundingMode)) =>
          Some(new MathContext(precision)) // I think roundingMode is irrelevant, because the check will warn if there is any rounding
        case _ =>
          None
      }

      def isMath(t: Tree, name: String): Boolean = t match {
        case field
          if (field is "scala.math.`package`."+name)
          || (field is staticSelect("java", "lang.Math."+name))
          || (field is staticSelect("java", "lang.StrictMath."+name)) => true
        case Apply(func, _params)
          if (func is "scala.math.`package`."+name)
          || (func is staticSelect("java", "lang.Math."+name))
          || (func is staticSelect("java", "lang.StrictMath."+name)) => true
        case Select(Apply(scala_Predef_xWrapper, List(_arg)), Name(`name`))
          if (scala_Predef_xWrapper.toString endsWith "Wrapper")
          && (t.tpe.widen weak_<:< DoubleClass.tpe) => true
        case _ => false
      }

      def isMathPow2(pow: Tree, detectSquareNumbers: Boolean = false): Boolean = pow match {
        case Apply(pow, List(_, Literal(Constant(2.0)))) if isMath(pow, "pow") => true   // math.pow(x, 2)
        case Apply(Select(id1, nme.MUL), List(id2)) if (id1 equalsStructure id2) => true // x*x
        case Literal(Constant(x: Int)) if (detectSquareNumbers) && (x > 1) && (math.sqrt(x.toDouble) == math.sqrt(x.toDouble).toInt) => true  // Square numbers
        case _ => /*println(showRaw(pow));*/ false
      }

      def isMathE(e: Tree): Boolean = e match {
        case Literal(Constant(e: Double))
          if (e >= 2.718281)
          && (e <= 2.718282) => true
        case _ if isMath(e, "E") => true
        case _ => false
      }

      def getAbs(t: Tree): Option[Tree] = t match {
        case Apply(abs, List(arg)) if isMath(abs, "abs") => Some(arg)
        case Select(Apply(scala_Predef_xWrapper, List(arg)), Name("abs"))
          if scala_Predef_xWrapper.toString endsWith "Wrapper" => Some(arg)
        case _ => None
      }

      def unwrapNumber(t: Tree): Tree = t match {
        case Apply(xWrapper, List(num)) if xWrapper.toString endsWith "Wrapper" => num
        case _ => t
      }

      def isEmptyCompare(x: Any, op: Name): Boolean = {
        ((x == 0 && (op == nme.EQ || op == nme.GT || op == nme.LE || op == nme.NE))
          || (x == 1 && (op == nme.LT || op == nme.GE)))
      }


      def isReturnStatement(t: Tree): Boolean = t match {
        case Return(_) => true
        case _ => false
      }

      var superTraverse = true
      def catcher(): PartialFunction[Throwable, Unit] = {
        case _: NullPointerException => //Ignore
        case _: NoSuchMethodError => //Ignore
        case _: StackOverflowError => superTraverse = false
        case e: Exception => //e.printStackTrace//TODO: Print details and ask user to report it
      }
      def finalizer(tree: Tree): Unit = {
        if (superTraverse) try { super.traverse(tree) } catch catcher
      }

      override def traverse(tree: Tree): Unit = try {
        superTraverse = true
        //Workarounds for some cases
        tree match {
          ///Workaround: case class generated code triggers a lot of the checks...
          case ClassDef(mods, _, _, body) if mods.isCase => traverse(body); superTraverse = false; return
          ///Workaround: suppresse a null warning and a "remove the if" check for """case class A()""" - see case class unapply's AST)
          case If(Apply(Select(_, nme.EQ), List(Literal(Constant(null)))), Literal(Constant(false)), Literal(Constant(true))) => superTraverse = false; return
          ///Workaround: ignores "Assignment right after declaration..." in case class hashcode
          case DefDef(_, Name("hashCode"), _, _, _, Block(block, last)) if {
            (block :+ last) match {
              case ValDef(_, _id1, _, _) :: Assign(_id2, _) :: _ => true
              case _ => false
            }} => superTraverse = false; return

          //// Some numeric checks
          /// Detects some invariant conditions
          //TODO: Move to abstract interpreter, handle edge cases > / >=

          case Apply(Select(Apply(Select(term1, compareOp1), List(n1 @ Literal(Constant(num1)))), logicOp), List(Apply(Select(term2, compareOp2), List(n2 @ Literal(Constant(num2))))))
            if (tree.tpe <:< BooleanClass.tpe)
            && (term1 equalsStructure term2)
            && isIdent(term1, recursive = true)
            && (n1.tpe weak_<:< DoubleClass.tpe)
            && (n2.tpe weak_<:< DoubleClass.tpe)
            && {
              // It still thinks num is Any...
              def toDouble[T](x: T): Double = x match {
                case a: Double => a
                case a: Float  => a.toDouble
                case a: Long   => a.toDouble
                case a: Int    => a.toDouble
                case a: Short  => a.toDouble
                case a: Byte   => a.toDouble
                case _ => Double.NaN
              }

              def minmax(num: Double, compareOp: Name): (Double, Double) = compareOp match {
                case nme.GE | nme.GT => (num, Double.PositiveInfinity)
                case nme.LE | nme.LT => (Double.NegativeInfinity, num)
                case _               => (Double.NaN, Double.NaN)
              }

              val (min1, max1) = minmax(toDouble(num1), compareOp1)
              val (min2, max2) = minmax(toDouble(num2), compareOp2)

              if (!min1.isNaN && !max1.isNaN && !min2.isNaN && !max2.isNaN) logicOp match {
                case nme.ZAND
                  if (min1 > max2 || min2 > max1) => // Intervals don't cross
                  warn(tree, InvariantCondition(always = true, "return false"))
                case nme.ZOR
                  if (min1 < max2 && min2 < max1) // Intervals cross, and span whole space
                  && (math.min(min1, min2) == Double.NegativeInfinity)
                  && (math.max(max1, max2) == Double.PositiveInfinity) =>
                  warn(tree, InvariantCondition(always = true, "return true"))
                case _ =>
              }

              false
            } => //Fallthrough

          /*case Apply(Select(lhs, nme.EQ), List(rhs))
            if isSubtype(lhs, DoubleClass.tpe) || isSubtype(lhs, FloatClass.tpe) || isSubtype(rhs, DoubleClass.tpe) || isSubtype(rhs, FloatClass.tpe) =>

            warn(tree, "Exact comparison of floating point values is potentially unsafe.")*/

          /// Tip to use log1p(x) and expm1(x), instead of log(1+x) and exp(x)-1 -- see http://www.johndcook.com/blog/2010/06/07/math-library-functions-that-seem-unnecessary/
          //TODO: maybe make checks to protect against potentially wrong fixes, e.g. log1p(a + 1) or log1p(a - 1)
          // also, check 1-exp(x) and other negated versions
          case Apply(log, List(Apply(Select(Literal(Constant(1)), nme.ADD), _))) if isMath(log, "log") =>
            warn(tree, UseLog1p)
          case Apply(log, List(Apply(Select(_, nme.ADD), List(Literal(Constant(1)))))) if isMath(log, "log") =>
            warn(tree, UseLog1p)

          case Apply(Select(Apply(exp, _), nme.SUB), List(Literal(Constant(1)))) if isMath(exp, "exp") =>
            warn(tree, UseExpm1)
          case Apply(Select(Literal(Constant(-1)), nme.ADD), List(Apply(exp, _))) if isMath(exp, "exp") =>
            warn(tree, UseExpm1)

          /// Suggest using hypot instead of sqrt(a*a, b*b)
          case Apply(sqrt, List(Apply(Select(pow1, nme.ADD), List(pow2))))
            if isMath(sqrt, "sqrt")
            && isMathPow2(pow1, detectSquareNumbers = true)
            && isMathPow2(pow2, detectSquareNumbers = true) =>

            warn(tree, UseHypot)
          case Apply(sqrt, List(Select(Apply(Select(pow1, nme.ADD), List(pow2)), Name("toDouble")))) // Handle toDouble (implicit) converion
            if isMath(sqrt, "sqrt")
            && isMathPow2(pow1, detectSquareNumbers = true)
            && isMathPow2(pow2, detectSquareNumbers = true)  =>

            warn(tree, UseHypot)

          /// Suggest using cbrt instead of pow(a, 1/3)
          /// Suggest using sqrt instead of pow(a, 0.5)
          /// Unnecessary power (zero or one)
          case Apply(math_pow, List(_num, Literal(Constant(pow: Double)))) if isMath(math_pow, "pow") && {
            if (pow >= 0.3333332 && pow <= 0.3333334) warn(tree, UseCbrt)
            else if (pow == 0.5) warn(tree, UseSqrt)
            else if (pow == 1 || pow == 0 || pow.isNaN || pow.isInfinite) warn(tree, SuspiciousPow(pow))

            false
          } => //Fallthrough

          /// Suggest using exp instead of pow(E, a)
          case Apply(pow, List(e, _num))
            if isMath(pow, "pow")
            && isMathE(e) =>

            warn(tree, UseExp)

          /// Suggest using log10(x) instead of log(x)/log(10)
          case Apply(Select(Apply(log1, List(_num)), nme.DIV), List(Apply(log2, List(Literal(Constant(10.0))))))
            if isMath(log1, "log")
            && isMath(log2, "log") =>

            warn(tree, UseLog10)

          /// Suggest using x.abs instead of doing it manually with sqrt(x^2)
          case Apply(sqrt, List(pow2))
            if isMath(sqrt, "sqrt")
            && isMathPow2(pow2) =>

            warn(tree, UseAbsNotSqrtSquare)

          /// Use x.isNaN instead of (x != x)
          case Apply(Select(left, func), List(right))
            if isFloatingPointType(left)
            && (func == nme.EQ || func == nme.NE)
            && ((left equalsStructure right) || (right equalsStructure Literal(Constant(Double.NaN))) || (right equalsStructure Literal(Constant(Float.NaN)))) =>

            warn(tree, if (left equalsStructure right) UseIsNanNotSelfComparison else UseIsNanNotNanComparison)

          /// Use x.signum instead of doing it manually
          case pos @ Apply(Select(expr1, nme.DIV), List(expr2)) if ((expr1, expr2) match {
              case (expr1, Apply(abs, List(expr2))) if isMath(abs, "abs") && (expr1 equalsStructure expr2) => true
              case (Apply(abs, List(expr1)), expr2) if isMath(abs, "abs") && (expr1 equalsStructure expr2) => true
              case (expr1, Select(Apply(wrapper, List(expr2)), Name("abs"))) if (wrapper endsWith "Wrapper") && (expr1 equalsStructure expr2) => true
              case (Select(Apply(wrapper, List(expr1)), Name("abs")), expr2) if (wrapper endsWith "Wrapper") && (expr1 equalsStructure expr2) => true
              case _ => false
            }) =>

            warn(pos, UseSignum)

          case _ if {
            /// Possibly unsafe use of abs -- math.abs(Int.MinValue) == -2147483648
            getAbs(tree).foreach {
              case pos @ Apply(Select(rand, Name(nextX @ ("nextInt"|"nextLong"))), Nil)
                if (rand.tpe.widen.toString startsWith "scala.util.Random") =>

                val typeX = nextX.stripPrefix("next")
                warn(pos, UnsafeAbs(s"Use ${nextX}(${typeX}.MaxValue) instead, as abs($typeX.MinValue) equals $typeX.MinValue"))

              case _ => //Ignore
            }

            false
          } => //Fallthrough

          /// BigDecimal checks
          // BigDecimal(0.1) //TODO: someday move to interpreter - detect this for known values
          case Apply(Select(bigDecimal, Name("apply"|"valueOf")), (treeLiteral @ Literal(Constant(literal))) :: paramTail)
            if ((bigDecimal is "scala.`package`.BigDecimal") || (bigDecimal endsWith "math.BigDecimal"))
            && (isFloatingPointType(treeLiteral) || treeLiteral.tpe.widen <:< StringClass.tpe) =>

            val isFloat = isFloatingPointType(treeLiteral)

            // actualLiteral is relevant only for double - compiler rounds it automatically
            val (strLiteral, actualLiteral) =
              if (isFloat) {
                // Get the actual token from code
                var token = ""
                try {
                  val pos = treeLiteral.pos
                  token = pos.lineContent.substring(pos.column - 1).takeWhile(_.toString matches "[-+0-9.edfEDF]").toLowerCase
                  if (!token.isEmpty && (token.last == 'f' || token.last == 'd')) token = token.dropRight(1)
                } catch {
                  case _: UnsupportedOperationException => // Happens if tree doesn't have a position
                  case _: NumberFormatException => // Could warn here, but I don't trust the above code enough
                }
                (literal.toString, token)
              } else {
                (literal.asInstanceOf[String], literal.asInstanceOf[String])
              }

            val mathContext =
              if (paramTail.size == 1 && paramTail.head.tpe <:< java_math_MathContext.tpe) {
                getMathContext(paramTail.head)
              } else {
                None
              }

            val improvement: String =
              if (isFloat) {
                if (strLiteral == actualLiteral && mathContext.isDefined) "Use a larger MathContext." else ""
                //else if (apply_valueOf is "apply") "Use a string constant."
                //else "Use a constructor with a string constant."
              } else {
                if (mathContext.isDefined) "Use a larger MathContext."
                else "Add a custom MathContext."
              }

            if (improvement != "") try {
              // Ugly hack to get around a few different representations: 0.005, 5E-3, ...
              def cleanString(s: String): String = s.replaceAll("^-|[.]|[Ee].*$|(0[.])?0*", "")

              val bd = if (mathContext.isDefined) BigDecimal(strLiteral, mathContext.get) else BigDecimal(strLiteral)
              if (cleanString(bd.toString) != cleanString(actualLiteral)) warn(treeLiteral, PossibleLossOfPrecision(improvement))
            } catch {
              case _: NumberFormatException =>
                warn(treeLiteral, BigDecimalNumberFormat)
            }

          // new java.math.BigDecimal(0.1)
          case Apply(Select(New(java_math_BigDecimal), nme.CONSTRUCTOR), List(Literal(Constant(_num: Double))))
            if java_math_BigDecimal is "java.math.BigDecimal" =>

            warn(tree, BigDecimalPrecisionLoss)

          // Warn about using NumericRange with floating point numbers because its broken:
          //   NumericRange is broken for floating point numbers (float, double), that means, when using it
          //   (collection methods: map, foreach, zip), it depends on which method is used for constructing
          //   the new collections. You basically can go two paths on NumericRange:
          //   1) The "apply" path:   @see scala.collection.immutable.NumericRange.locationAfterN
          //   2) The "foreach" path: @see scala.collection.immutable.NumericRange.foreach
          //
          //   The difference between these paths is that "foreach" accumulates values to obtain the current value
          //   (current += step), but "apply" will directly calculate the current value (start + (step * fromInt(n))).
          //   The apply path is more accurate. This means that the values on these path will diverge due to other
          //   rounding behavior.
          //
          //   For example:
          //
          //   val range = 0D to 1D by (1 / 7D)
          //
          //   1) Apply path
          //   range.iterator.toVector // uses apply in range.iterator in the end
          //   Vector(
          //     0.0,
          //     0.14285714285714285,
          //     0.2857142857142857,
          //     0.42857142857142855,
          //     0.5714285714285714,
          //     0.7142857142857142,
          //     0.8571428571428571,
          //     1.0
          //   )
          //
          //   2) Foreach path
          //   range.toVector // uses foreach at CanBuildFrom respectively ++= in the end
          //   Vector(
          //     0.0,
          //     0.14285714285714285,
          //     0.2857142857142857,
          //     0.42857142857142855,
          //     0.5714285714285714,
          //     0.7142857142857142,
          //     0.857142857142857,
          //     0.9999999999999998
          //   )
          //
          //   As one can see the last two values differ and iterator/apply is more accurate.
          case Apply(Select(partialRangeTree, Name("by")), List(FloatingPointNumber(_num)))
              if {
                val partialRangeClass = rootMirror.getClassByName(newTermName("scala.collection.immutable.Range.Partial"))

                (partialRangeTree.tpe.typeConstructor =:= partialRangeClass.tpe.typeConstructor &&
                 partialRangeTree.tpe.widen.typeArgs.headOption.exists(isFloatingPointType))
              }
            => warn(tree, FloatingPointNumericRange)

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
            superTraverse = false; return

          /// Comparing with == on instances of different types: 5 == "5"
          //TODO: Scala 2.10 has a similar check "comparing values of types Int and String using `==' will always yield false"
          case Apply(eqeq @ Select(lhs, op @ (nme.EQ | nme.NE)), List(rhs))
            if methodImplements(eqeq.symbol, Object_==)
            && !isSubtype(lhs, rhs) && !isSubtype(rhs, lhs)
            && !(integerTypes.contains(lhs.tpe.widen) && integerTypes.contains(rhs.tpe.widen))
            && lhs.tpe.widen.toString != "Null" && rhs.tpe.widen.toString != "Null"
            && ((lhs.tpe.widen.toString.takeWhile(_ != '[') != rhs.tpe.widen.toString.takeWhile(_ != '[')) || {
              val higherReg = """.+?\[(.+)\]""".r
              val higherReg(lhsHigher) = lhs.tpe.widen.toString
              val higherReg(rhsHigher) = rhs.tpe.widen.toString

              def generic(s: String): Boolean =
                s.split(',').exists { t => (t.size == 1 || t.contains("_") || t.contains("?")) }

              ((lhsHigher != rhsHigher) && !generic(lhsHigher) && !generic(rhsHigher))
            }) =>

            warn(eqeq, UnlikelyEquality(lhs.tpe.widen.toString, rhs.tpe.widen.toString, if (op == nme.EQ) "==" else "!="))

          /// Warn against importing from collection.JavaConversions
          case Import(pkg, selectors) if (pkg.symbol == JavaConversionsModule) && (selectors exists isGlobalImport) =>
            warn(pkg, JavaConverters)

          /// Warn about wildcard imports (disabled)
          /*case Import(pkg, selectors) if selectors exists isGlobalImport =>
            //TODO: Too much noise - maybe it would be useful to non-IDE users if it printed
            // a nice selector import replacement, e.g. import mutable._ => import mutable.{ HashSet, ListBuffer }

            warn(pkg, "Wildcard imports should be avoided. Favor import selector clauses.")*/

          /// Collection.contains on different types: List(1, 2, 3).contains("2")
          case Apply(Select(col, Name("contains")), List(target))
            if !(target.tpe.widen weak_<:< seqMemberType(col.tpe.widen))
            && !(target.tpe =:= AnyClass.tpe)
            && (col.tpe.baseClasses.exists(c => c.tpe =:= SeqClass.tpe || c.tpe =:= OptionClass.tpe)) =>

            warn(tree, ContainsTypeMismatch(col.tpe.widen.toString, target.tpe.widen.toString))

          case Apply(TypeApply(Select(col, Name("contains")), _), List(target))
            if !(target.tpe.widen weak_<:< seqMemberType(col.tpe.widen))
            && !(target.tpe =:= AnyClass.tpe)
            && (col.tpe.baseClasses.exists(c => c.tpe =:= SeqClass.tpe || c.tpe =:= OptionClass.tpe)) =>

            warn(tree, ContainsTypeMismatch(col.tpe.widen.toString, target.tpe.widen.toString))

          /// Using toString on an Array
          case Apply(Select(array, Name("toString")), Nil)
            if (array.tpe.widen.toString startsWith "Array[") =>

            warn(tree, UnlikelyToString("Array"))

          case Apply(Select(java_lang_String, Name("valueOf")), List(array))
            if (array.tpe.widen.toString startsWith "Array[")
            && (java_lang_String is staticSelect("java", "lang.String")) =>

            warn(tree, UnlikelyToString("Array"))

          case Apply(Select(java_lang_String, Name("valueOf")), List(str))
            if (java_lang_String is staticSelect("java", "lang.String"))
            && (str.tpe <:< StringClass.tpe) =>

            warn(tree, TypeToType("String"))

          /// Warn about using .asInstanceOf[T] (disabled)
          //TODO: false positives in case class A(), and in the interpreter init
          /*case aa @ Apply(a, List(b @ Apply(s @ Select(instanceOf, dd), ee))) if methodImplements(instanceOf.symbol, AsInstanceOf) =>
            //println((aa, instanceOf))
          case instanceOf @ Select(a, func) if methodImplements(instanceOf.symbol, AsInstanceOf) =>
            //TODO: too much noise, maybe detect when it's completely unnecessary
            warn(tree, "Avoid using asInstanceOf[T] (use pattern matching, type ascription, etc).")*/
          /// Warn about using .asInstanceOf[Number]
          case TypeApply(asInstanceOf @ Select(num, _asInstanceOf), _tpe)
            if methodImplements(asInstanceOf.symbol, AsInstanceOf)
            && !(num.tpe.widen <:< tree.tpe.widen || tree.tpe.widen <:< num.tpe.widen)
            && ((num.tpe.widen weak_<:< tree.tpe.widen) || (tree.tpe.widen weak_<:< num.tpe.widen)) =>
              warn(tree, NumberInstanceOf(tree.tpe.widen.toString))

          /*case TypeApply(instanceOf @ Select(a, funcName), t)
            if methodImplements(instanceOf.symbol, AsInstanceOf)
            && !(a.tpe.widen <:< tree.tpe.widen || tree.tpe.widen <:< a.tpe.widen)
            && !(a.tpe.typeSymbol.isAbstractType || tree.tpe.typeSymbol.isAbstractType)
            && !(a.tpe.widen.toString.matches(""".*\[((_\$?|\?)[0-9]+|[A-Z])\].*""") || tree.tpe.widen.toString.matches(""".*\[((_\$?|\?)[0-9]+|[A-Z])\].*"""))
            && !(a.tpe.widen.toString == "Object" || tree.tpe.widen.toString == "Object")
            && !(a.tpe.widen.toString == "Null") =>

            warn(tree.pos, pref+"The cast "+a.tpe.widen+".asInstanceOf["+tree.tpe.widen+"] is likely invalid.")*/

          /// Calling Option.get is potentially unsafe (disabled)
          //TODO: if (x.isDefined) func(x.get) / if (x.isEmpty) ... else func(x.get), etc. are false positives -- those could be detected in abs-interpreter
          /*case get @ Select(_, nme.get) if methodImplements(get.symbol, OptionGet) =>
            warn(tree, "Calling .get on Option will throw an exception if the Option is None.")*/

          /// Nag about using null (disabled)
          //TODO: Too much noise - limit in some way
          /*case Literal(Constant(null)) =>
            warn(tree, "Using null is considered dangerous, use Option.")*/

          /// TypeToType ... "hello".toString, 5.toInt, List(1, 2, 3).toList ...
          case Select(fTpe, tTpe)
            if !fTpe.isInstanceOf[This] // calling toX inside X
            && tTpe.toString.startsWith("to") && {
              val fromTpe = fTpe.tpe.widen.toString
              val toTpe = tTpe.toString.drop(2)
              val targetTpe = tree.tpe.widen.toString

              //TODO: deal with false positives from this, see commented test
              // oh my...
              /// Array.toArray: scala.collection.mutable.ArrayOps[Int] => [U >: Int](implicit evidence$1: scala.reflect.ClassTag[U])Array[U]
              /// Map.toMap: scala.collection.mutable.Map[Int,Int] => [T, U](implicit ev: <:<[(Int, Int),(T, U)])scala.collection.immutable.Map[T,U])
              def cleanTypeStr(tpeStr: String) =
                (tpeStr
                  .replaceFirst("^\\[.*?\\](=>)? ?", "")
                  .replaceFirst("^\\(.*?\\)", "")
                  .takeWhile(_ != '[')
                  .replaceAll(".*[.]", "")
                  .stripPrefix("Rich")
                  .stripSuffix("Ops"))

              if (toTpe == cleanTypeStr(targetTpe) && toTpe == cleanTypeStr(fromTpe)) {
                if (toTpe == "Set" || toTpe == "Map") {
                  // on mutable Map and Set conversion returns immutable
                  !fromTpe.startsWith("scala.collection.mutable.")
                } else {
                  true
                }
              } else {
                false
              }

            } =>

            warn(tree, TypeToType(tTpe.toString.stripPrefix("to")))

          //// String checks
          /// Repeated string literals (disabled)
          /*case Literal(Constant(str: String)) =>
            //TODO: String interpolation gets broken down into parts and causes false positives
            //TODO: a quick benchmark showed string literals are actually more optimized than almost anything else, even final vals
            val threshold = 4

            stringLiteralCount(str) += 1
            if (stringLiteralCount(str) == threshold && !(stringLiteralFileExceptions.contains(unit.source.toString)) && !(str.matches(stringLiteralExceptions))) {
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
              case "$plus"|"equals"|"$eq$eq"|"toCharArray"|"matches"|"getBytes" => //Ignore
              case "length" => warn(pos, "Taking the length of a constant string")
              case _        => warn(pos, "Processing a constant string")
            }
          case Select(Apply(Select(predef, augmentString), List(pos @ Literal(Constant(s: String)))), size)
            if predef is "scala.this.Predef" && augmentString.toString == "augmentString" && size.toString == "size" =>
            warn(pos, "Taking the size of a constant string")*/

          //// Pattern Matching checks
          case Match(pat, cases)
            if (pat match {
              case Typed(_, t) if !t.contains("switch") => false; // for scala.annotation.switch
              case _ => true })
            && pat.tpe.toString != "Any @unchecked"
            && cases.size >= 2 =>
            //Workaround: "Any @unchecked" seems to happen on the matching structures of actors - and all cases return true
            //Workaround: Typed (or Annotated) seems to happen in for loop pattern matching, which doesn't work right for at least for checkUsage

            /// Pattern Matching on a constant value
            //TODO: move to abs-interpreter
            if (isLiteral(pat)) {
              /*val returnValue =
                cases
                  .map { ca => (ca.pat, ca.body) }
                  .find { case (Literal(Constant(c)), _) => c == a; case _ => false}
                  .map { _._2 }
                  .orElse { if (cases.last.pat.toString == "_") Some(cases.last.body) else None }
                  .map { s => " will always return " + s }
                  .getOrElse("")*/

              warn(tree, PatternMatchConstant)
            }

            var optionCase, booleanCase = false
            //TODO: Hacky hack hack -_-, sorry. use tpe <:< definitions.xxx.tpe
            val (optionCaseReg, booleanCaseReg) = ("(Some[\\[].*[\\]]|None[.]type)", "Boolean[(](true|false)[)]")
            def checkCase(caseTree: CaseDef): Unit = {
              val caseStr = caseTree.pat.toString
              val caseTypeStr = caseTree.pat.tpe.toString
              //println((caseStr, caseTypeStr))

              optionCase |= (caseTypeStr matches optionCaseReg)
              booleanCase |= (caseTypeStr matches booleanCaseReg)
            }
            def printCaseWarning(): Unit = {
              if (cases.size == 2) {
                if (optionCase) {
                  /// Checks if pattern matching on Option -- see: http://blog.tmorris.net/posts/scalaoption-cheat-sheet/ (disabled)
                  //TODO: too much noise, and some cases are perfectly fine - try detecting all the exact cases from link
                  //warn(tree, "There are probably better ways of handling an Option.")
                } else if (booleanCase) {
                  /// Checks if pattern matching on Boolean
                  //TODO: case something => ... case _ => ... is also an if in a lot of cases
                  warn(tree, PreferIfToBooleanMatch)
                }
              }
            }

            /// Checking for duplicate case bodies and suggests merging
            // Check only if isLiteral(c.pat), because other types usually cannot be easily merged
            //TODO: could also work for literal local vals
            case class Streak(streak: Int, tree: CaseDef)
            var streak = Streak(0, cases.head)
            def checkStreak(c: CaseDef): Unit = {
              if ((c.body != EmptyTree) && (c.body equalsStructure streak.tree.body) && isLiteral(c.pat) && (c.guard == EmptyTree && streak.tree.guard == EmptyTree)) {
                streak = Streak(streak.streak + 1, c)
              } else {
                printStreakWarning()
                streak = Streak(1, c)
              }
            }
            def printStreakWarning(): Unit = {
              if (streak.streak == cases.size) {
                // This one always turns out to be a false positive :)
                //warn(tree, "All "+cases.size+" cases will return "+cases.head.body+", regardless of pattern value")
              } else if (streak.streak > 1) {
                warn(streak.tree.body, IdenticalCaseBodies(streak.streak.toString))
              }
            }

            /// Checking for unused variables in pattern matching (disabled)
            //def checkUsage(c: CaseDef): Unit = {
              //TODO: use for self-testing from time to time - ~100 warnings currently :/
              /*val binds = for (b @ Bind(name, _) <- c.pat; if !name.toString.startsWith("_")) yield (b, name.toString)
              for (unused <- binds.filter { case (b, name) => !abstractInterpretation.isUsed(c, name)}) {
                println(showRaw(pat))
                warn(unused._1, "Unused value in pattern matching, use _ instead. (or prefix with _ to get rid of me)")
              }*/
            //}

            /// Detect some unreachable cases
            //TODO: move to abs. interpreter to detect impossible guards
            //TODO: if there is a case (x, y) without a guard, it will make a latter case (x, y) with a guard unreachable
            val pastCases = mutable.ListBuffer[CaseDef]()
            def checkUnreachable(c: CaseDef): Unit = {
              // Adapted from scala/reflect/internal/Trees.scala to cover wildcards in CaseDef
              def correspondsWildcardStructure(thiz: CaseDef, that: CaseDef): Boolean = {
                val wildcards = mutable.HashSet[(Name, Name)]() // Enumerate wildcard aliases

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
                        if (b1.tpe =:= b2.tpe) {
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

              if (pastCases exists { p => correspondsWildcardStructure(p, c) })
                warn(c, IdenticalCaseConditions)
              else {
                val nonUnitResult = (pastCases += c)
              }
            }

            for (c <- cases) {
              checkCase(c)
              checkStreak(c)
              //checkUsage(c)
              checkUnreachable(c)
            }

            printStreakWarning()
            printCaseWarning()

            def orderOption(cases: List[CaseDef]) = cases match {
              case List(none @ CaseDef(ScalaNone(), EmptyTree, _), some) => List(some, none)
              case x => x
            }

            def matchOption(cases: List[CaseDef]) = cases match {
              case List(CaseDef(Apply(scala_Some @ TypeTree(), List(Bind(x1, Ident(nme.WILDCARD)))), EmptyTree, x2), CaseDef(ScalaNone(), EmptyTree, ScalaNone()))
                if (scala_Some.original is "scala.Some")
                && (x1.toString == x2.toString) =>

                warn(tree, UseOptionFlattenNotPatMatch)

              case List(CaseDef(Apply(scala_Some @ TypeTree(), List(Bind(x1, Ident(nme.WILDCARD)))), EmptyTree, Apply(TypeApply(Select(ScalaSome(), Name("apply")), _), List(x2))), CaseDef(ScalaNone(), EmptyTree, y))
                if (scala_Some.original is "scala.Some")
                && (x1.toString == x2.toString) =>

                warn(tree, UseOrElseNotPatMatch(y.toString))

              case List(CaseDef(Apply(scala_Some @ TypeTree(), List(Bind(x1, Ident(nme.WILDCARD)))), EmptyTree, x2), CaseDef(ScalaNone(), EmptyTree, y))
                if (scala_Some.original is "scala.Some")
                && (x1.toString == x2.toString) =>

                warn(tree, UseGetOrElseNotPatMatch(y.toString))

              case List(CaseDef(Apply(scala_Some @ TypeTree(), List(Bind(_, Ident(nme.WILDCARD)))), EmptyTree, Apply(TypeApply(Select(ScalaSome(), Name("apply")), _), List(y))), CaseDef(ScalaNone(), EmptyTree, ScalaNone()))
                if (scala_Some.original is "scala.Some") =>

                warn(tree, UseOptionMapNotPatMatch(y.toString))

              case List(CaseDef(Apply(scala_Some @ TypeTree(), List(Bind(_, Ident(nme.WILDCARD)))), EmptyTree, y), CaseDef(ScalaNone(), EmptyTree, ScalaNone()))
                if (scala_Some.original is "scala.Some") =>

                warn(tree, UseOptionFlatMapNotPatMatch(y.toString))

              case List(CaseDef(Apply(scala_Some @ TypeTree(), List(Bind(_, Ident(nme.WILDCARD)))), EmptyTree, y), CaseDef(ScalaNone(), EmptyTree, Literal(Constant(()))))
                if (scala_Some.original is "scala.Some") =>

                warn(tree, UseOptionForeachNotPatMatch(y.toString))

              case List(CaseDef(Apply(scala_Some @ TypeTree(), List(Bind(x1, Ident(nme.WILDCARD)))), EmptyTree, Literal(Constant(true))), CaseDef(ScalaNone(), EmptyTree, Literal(Constant(false))))
                if (scala_Some.original is "scala.Some") =>

                warn(tree, UseOptionIsDefinedNotPatMatch)

              case List(CaseDef(Apply(scala_Some @ TypeTree(), List(Bind(x1, Ident(nme.WILDCARD)))), EmptyTree, Literal(Constant(false))), CaseDef(ScalaNone(), EmptyTree, Literal(Constant(true))))
                if (scala_Some.original is "scala.Some") =>

                warn(tree, UseOptionIsEmptyNotPatMatch)

              case List(CaseDef(Apply(scala_Some @ TypeTree(), List(Bind(x1, Ident(nme.WILDCARD)))), EmptyTree, y), CaseDef(ScalaNone(), EmptyTree, Literal(Constant(false))))
                if (scala_Some.original is "scala.Some") =>

                warn(tree, UseOptionExistsNotPatMatch(y.toString))

              case List(CaseDef(Apply(scala_Some @ TypeTree(), List(Bind(x1, Ident(nme.WILDCARD)))), EmptyTree, y), CaseDef(ScalaNone(), EmptyTree, Literal(Constant(true))))
                if (scala_Some.original is "scala.Some") =>

                warn(tree, UseOptionForallNotPatMatch(y.toString))

              case _ =>
            }
            matchOption(orderOption(cases))
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
          //Workaround: ranges, where it's acceptable, e.g. if (6 < a && a < 10) ...
          case Apply(Select(
            yoda @ Apply(Select(Literal(Constant(_)), func1), List(notLiteral1)), _logicOp),
            List(Apply(Select(notLiteral2, func2), List(_arg2))))
            if (func1.toString matches "[$](greater|less)([$]eq)?") && !isLiteral(notLiteral1)
            && (func2.toString matches "[$](greater|less)([$]eq)?") && (notLiteral1 equalsStructure notLiteral2)
            && (func1.toString.take(5) == func2.toString.take(5)) => // cheap way of saying < and <= can appear in range together

            nowarnPositions += yoda.pos

          case Apply(Select(Literal(Constant(_)), func), List(notLiteral))
            if (func.toString matches "[$](greater|less|eq)([$]eq)?")
            && !isLiteral(notLiteral)
            && !(notLiteral is "x") //Workaround: for synthetic "abc".filter('a'==) ... TODO: skips all yoda conditions with x
            && !(notLiteral is "x$1") => //Workaround: for synthetic "abc".filter('a' == _)

            warn(tree, YodaConditions)

          /// Unnecessary Ifs
          case If(cond, Literal(Constant(true)), Literal(Constant(false))) =>
            warn(cond, UseConditionDirectly())
          case If(cond, Literal(Constant(false)), Literal(Constant(true))) =>
            warn(cond, UseConditionDirectly(negated = true))
          case If(cond, Assign(id1, _), Assign(id2, _)) //TODO: .this workaround for object-local variables sucks... false negatives
            if (id1.toString == id2.toString) && !(id1.toString contains ".this") =>
            warn(cond, UseIfExpression(id1.toString))
          case If(cond, Apply(Select(id1, setter1), List(_)), Apply(Select(id2, setter2), List(_)))
            if (setter1 endsWith "_$eq") && (setter2 endsWith "_$eq") && (id1.toString == id2.toString) && !(id1.toString contains ".this") =>
            warn(cond, UseIfExpression(id1.toString))
          //TODO: showRaw hack - find flag for macro code instead
          case If(cond, Block(block, ret), Block(_, _)) if (isReturnStatement(ret) || block.exists(isReturnStatement)) && !showRaw(tree).contains("$macro$") => // Idea from oclint
            warn(cond, UnnecessaryElseBranch)
          case If(_cond, a, b) if (a equalsStructure b) && (a.children.nonEmpty) =>
            //TODO: empty if statement (if (...) { }) triggers this - issue warning for that case?
            //TODO: test if a single statement counts as children.nonEmpty
            warn(a, DuplicateIfBranches)
          case If(_cond1, a, If(_cond2, b, c))
            if (a.children.nonEmpty && ((a equalsStructure b) || (a equalsStructure c)))
            || (b.children.nonEmpty && (b equalsStructure c)) =>
            //TODO: could be made recursive, but probably no need

            warn(a, DuplicateIfBranches)

          /// Find repeated (sub)conditions in if-else chains, that will never hold
          // Caches conditions separated by OR, and checks all subconditions separated by either AND or OR
          case If(_cond, t, e) if {
            def getSubConds(cond: Tree)(op: Name): List[Tree] =
              List(cond) ++ (cond match {
                case Apply(Select(left, opp), List(right)) if op == opp =>
                  getSubConds(left)(op) ++ getSubConds(right)(op)
                case _ =>
                  Nil
              })
            val conds = mutable.ListBuffer[Tree]()
            def elseIf(tree: Tree): Unit = {
              tree match {
                case If(cond, t, e) =>
                  val subCondsOr = getSubConds(cond)(nme.ZOR)
                  val subCondsAnd = getSubConds(cond)(nme.ZAND)

                  for (newCond <- (subCondsOr ++ subCondsAnd); oldCond <- conds) {
                    if ((newCond equalsStructure oldCond) && !(newCond.toString.toLowerCase contains "random")) {
                      warn(newCond, IdenticalIfElseCondition)
                    }
                  }

                  conds ++= subCondsOr

                  elseIf(e)

                  /// Detects some nonsensical if expressions of the type: if (a == b || ...) a else b
                  (subCondsOr ++ subCondsAnd) filter {
                    case Apply(Select(a, op), List(b))
                      if (op == nme.EQ || op == nme.NE)
                      && (((a equalsStructure t) && (b equalsStructure e)) || ((a equalsStructure e) && (b equalsStructure t))) => true
                    case _ => false
                  } foreach { subCond =>
                    warn(subCond, InvariantCondition(always = true, "cause the same return value"))
                  }
                case _ => //Ignore
              }
            }
            elseIf(tree)

            false
          } => //Fallthrough

          /// if (cond1) { if (cond2) { ... } } is the same as if (cond1 && cond2) { ... }
          case If(_cond1, iff @ If(_cond2, _body, else1), else2)
            if (else1 equalsStructure else2)
            && !(nowarnMergeNestedIfsPositions contains iff.pos) =>

            warn(tree, MergeNestedIfs)

          /// ifdowhile loop (idea by OpenSSL Valhalla Rampage) :)
          case If(cond1, LabelDef(doWhile1, List(), Block(_body, If(cond2, Apply(Ident(doWhile2), List()), Literal(Constant(()))))), Literal(Constant(())))
            if (cond1 equalsStructure cond2)
            && (doWhile1.toString startsWith "doWhile")
            && (doWhile1.toString == doWhile2.toString) =>

            warn(tree, IfDoWhile)

          //// Multiple-statement checks
          case Block(init, last) =>
            val block = init :+ last

            /// Check for unused variable values
            sealed trait AssignStatus
            case object Unknown extends AssignStatus
            case object Defined extends AssignStatus
            case object Unused extends AssignStatus
            case object Used extends AssignStatus

            val assigns = mutable.HashMap[Name, AssignStatus]() withDefaultValue Unknown
            def checkAssigns(tree: Tree, onlySetUsed: Boolean): Unit = {
              tree match {
                case ClassDef(_, _, _, _) | DefDef(_, _, _, _, _, _) =>
                  // see issue 21 - skip assignments that are not executed immediately

                //TODO: It could check if it gets set in all branches - Ignores currently
                case If(_, _, _) | Match(_, _) =>
                  for (t <- tree.children) checkAssigns(t, onlySetUsed = true)

                case ValDef(mods, id, _, right) if mods.isMutable =>
                  //TODO: shadowing warning doesn't work, even if I make sure each tree is visited once, and even if I don't traverse inner Blocks
                  //if (assigns contains id) warn(tree, "Variable "+id.toString+" is being shadowed here.")
                  checkAssigns(right, onlySetUsed)

                  assigns(id) =
                    if((right.tpe.widen weak_<:< DoubleClass.tpe)
                    || (right.tpe.widen <:< BooleanClass.tpe)
                    || (right.tpe.widen <:< StringClass.tpe)
                    || (right isAny ("null", "scala.None")))
                      Defined //Ignore these initial values
                    else
                      Unused

                case Assign(Ident(id), right) =>
                  checkAssigns(right, onlySetUsed)
                  if (!onlySetUsed) assigns(id) match {
                    case Unknown => //Ignore
                    case Defined => assigns(id) = Unused
                    case Used    => assigns(id) = Unused
                    case Unused  => warn(tree, VariableAssignedUnusedValue(id.toString))
                  }

                case Ident(id) =>
                  assigns(id) = Used

                case tree =>
                  //for (Ident(id) <- tree; if assigns(id) == Unused()) assigns(id) == Used()
                  for (t <- tree.children) checkAssigns(t, onlySetUsed)
              }
            }

            /// Warning on exiting a block with unused values (broken/disabled)
            /*val unused = (assigns filter { _._2 == Unused() } map { _._1.toString.trim } mkString ", ")
            if (!unused.isEmpty) warn(block.last, "Variable(s) "+unused+" have an unused value before here.")*/

            for (stmt <- block) { checkAssigns(stmt, onlySetUsed = false) }

            /// Unthrown exception
            //TODO: only finds instances smack in the middle of blocks, and only new Exception somethings
            for (stmt <- init) {
              if (stmt.tpe.widen <:< ExceptionClass.tpe) stmt match {
                case Apply(Select(New(_), nme.CONSTRUCTOR), Nil) => warn(stmt, UnthrownException)
                case _ => //Ignore
              }
            }

            //// Checks on two subsequent statements
            (block zip block.tail) foreach {
              /// Probably mistake in swaping two variables: a = b, b = b
              //TODO: there could be other expressions between the assigns
              case (Assign(id1, id2), Assign(id2_, id1_)) if (id1 equalsStructure id1_) && (id2 equalsStructure id2_) =>
                warn(id1_, MalformedSwap)

              /// "...; val x = value; x }" at the end of a method (disabled)
              //TODO: disabled - I usually have commented debug outputs in between these
              //TODO: this could be generalized in the new unused value code above
              /*case (v @ ValDef(_, id1, _, _), l @ Ident(id2)) if id1.toString == id2.toString && (l eq last) =>
                warn(v, "You don't need that temp variable.")*/

              /// Doing the same thing twice
              case (if1 @ If(cond1, _, _), _if2 @ If(cond2, _, _))
                if (cond1 equalsStructure cond2)
                && (if1 match {
                  case If(Ident(_), _, _) => //Ignore single booleans - probably debug/logging
                    false
                  case If(Select(Ident(_), nme.UNARY_!), _, _) => //Ignore single booleans - probably debug/logging
                    false
                  case If(cond, t, f) if (getUsed(cond) & (getAssigned(t) ++ getAssigned(f))).nonEmpty => //Ignore if assigning variables which appear in condition
                    false
                  case If(_, notBlock, empty)
                    if !notBlock.isInstanceOf[Block] && !notBlock.isInstanceOf[ValDef]
                    && (empty equalsStructure Literal(Constant(()))) => //Ignore simple things - probably debug/logging
                    false
                  case _ =>
                    true
                }) =>

                warn(cond2, IdenticalIfCondition)

              case (s1, s2)
                if (s1 equalsStructure s2)
                && !(s1 is staticSelect("scala", "Predef.println()"))
                && !(s1.toString contains "Next") && !(s1.toString contains "next") =>

                nowarnPositions += s2.pos
                warn(s1, IdenticalStatements)

              case _ =>
            }

          /// Literal negative index for a collection (obsolete?)
          case Apply(Select(_seq, _apply), List(Literal(Constant(index: Int))))
            if methodImplements(tree.symbol, SeqLikeApply) && index < 0 =>
            warn(tree, IndexingWithNegativeNumber)

          /// Literal division by zero (obsoleted by abs-int? not for all types, sadly...)
          // Can't always check double/float, as even the parser will change it to Infinity
          case Apply(Select(num, op), List(denomLiteral @ Literal(Constant(denom))))
            if (op == nme.DIV || op == nme.MOD)
            && (num.tpe.widen weak_<:< DoubleClass.tpe)
            && (denomLiteral.tpe.widen weak_<:< DoubleClass.tpe)
            && (denom == 0 || denom == 1) =>

            if (denom == 0) {
              warn(tree, DivideByZero)
            } else if (denom == 1) {
              if (op == nme.DIV) warn(tree, DivideByOne)
              else if (op == nme.MOD && (num.tpe.widen weak_<:< LongClass.tpe)) warn(tree, ModuloByOne)
            }
          case Apply(Select(numLiteral @ Literal(Constant(num)), op), List(denom))
            if (op == nme.DIV || op == nme.MOD)
            && (numLiteral.tpe.widen weak_<:< DoubleClass.tpe)
            && (denom.tpe.widen weak_<:< DoubleClass.tpe)
            && (num == 0) =>

            warn(tree, ZeroDivideBy)

          /// Option of an Option
          //TODO: make stricter if you want, but Ident(_) could get annoying if someone out there is actually using this :)
          case ValDef(_, _, _, value) if isOptionOption(value) =>
            warn(tree, OptionOfOption)

          /// Inferred type Nothing, Any, M[Nothing], or M[Any] (idea by OlegYch)
          case ValDef(mods, name, tpe, body)
            if !mods.isParameter
            && !(name.toString.trim matches "res[0-9]+") //Workaround: for REPL
            && !(name.toString.trim == "$result") //Workaround: for REPL/Tests
            && ((tpe.toString contains "Any") || (tpe.toString contains "Nothing")) // Gets rid of Stuff[_]
            && (containsAnyType(tpe.tpe) || containsNothingType(tpe.tpe))
            && (inferred contains tpe.pos)
            && !(body.isInstanceOf[New]) =>

            body match {
              case Apply(Select(New(_), nme.CONSTRUCTOR), _) =>
              case TypeApply(Select(_, Name("asInstanceOf")), _) =>
              case Apply(TypeApply(Select(_collection, Name("apply")), types), _elems)
                if types.exists(t => t.isInstanceOf[TypeTree] && t.asInstanceOf[TypeTree].original != null) =>
              case Ident(_) =>
              case _ => warn(tree, UndesirableTypeInference(tpe.tpe.toString))
            }

          /// Putting null into Option (idea by Smotko)
          case DefDef(_, _, _, _, tpe, body) if (tpe.toString matches "Option\\[.*\\]") &&
            (body match {
              case n @ Literal(Constant(null)) => warn(n, AssigningOptionToNull); true
              case Block(_, n @ Literal(Constant(null))) => warn(n, AssigningOptionToNull); true
              case _ => false
            }) => //Ignore
          case ValDef(_, _, tpe, body) if (tpe.toString matches "Option\\[.*\\]") &&
            (body match {
              case n @ Literal(Constant(null)) => warn(n, AssigningOptionToNull); true
              case Block(_, n @ Literal(Constant(null))) => warn(n, AssigningOptionToNull); true
              case _ => false
            }) => //Ignore
          case Assign(left, right) if (left.tpe.toString matches "Option\\[.*\\]") &&
            (right match {
              case n @ Literal(Constant(null)) => warn(n, AssigningOptionToNull); true
              case Block(_, n @ Literal(Constant(null))) => warn(n, AssigningOptionToNull); true
              case _ => false
            }) => //Ignore

          /// Null checking instead of Option wrapping
          case If(Apply(Select(expr1, nme.EQ), List(Literal(Constant(null)))), ScalaNone(), Apply(TypeApply(Select(ScalaSome(), Name("apply")), List(TypeTree())), List(expr2)))
            if (expr1 equalsStructure expr2) =>

            warn(tree, WrapNullWithOption)

          case If(Apply(Select(expr1, nme.NE), List(Literal(Constant(null)))), Apply(TypeApply(Select(ScalaSome(), Name("apply")), List(TypeTree())), List(expr2)), ScalaNone())
            if (expr1 equalsStructure expr2) =>

            warn(tree, WrapNullWithOption)

          /// Passing null into method param expecting Option
          //TODO: only first parameter list is checked. Multiple is Apply(Apply(_ List(1)), List(2)), ... but has same fun.symbol
          case Apply(fun, args)
            if args.nonEmpty
            && !fun.isInstanceOf[Apply]
            && fun.symbol.isMethod
            && {
              val NullLiteral = Literal(Constant(null))
              val params = fun.symbol.asMethod.paramss.head
              for ((param, arg) <- params.zip(args)) {
                if (arg equalsStructure NullLiteral) {
                  var paramType = param.typeSignature
                  if (paramType.baseClasses.head.fullName == "scala.<byname>") {
                    paramType = paramType.typeArgs.head
                  }
                  if (paramType.baseClasses.head.tpe =:= OptionClass.tpe) {
                    warn(arg, PassingNullIntoOption)
                  }
                }
              }

              false
            } => // Fallthrough

          /// Comparing Option to None instead of using isDefined (disabled)
          /*case Apply(Select(opt, op), List(scala_None)) if (op == nme.EQ || op == nme.NE) && (scala_None is "scala.None") =>
            warn(tree, "Use .isDefined instead of comparing to None")*/

          /// filter(...).headOption is better written as find(...)
          case Select(Apply(Select(col, Name("filter")), List(Function(List(ValDef(_, _, _, _)), _))), Name("headOption")) =>

            warn(tree, UseFindNotFilterHead(identOrCol(col)))

          /// orElse(Some(...)).get is better written as getOrElse(...)
          case Select(Apply(TypeApply(Select(option, Name("orElse")), _), List(Apply(TypeApply(someApply @ Select(ScalaSome(), Name("apply")), List(TypeTree())), List(_value)))), Name("get")) =>

            warn(someApply, UseGetOrElseOnOption(identOrOpt(option)))

          /// if (opt.isDefined) opt.get else something is better written as getOrElse(something) and similar warnings
          //TODO: improve the warning text, and curb the code duplication
          case If(Select(opt1, Name("isDefined")), getCase @ Select(opt2, Name("get")), elseCase) //duplication
            if (opt1 equalsStructure opt2) && !(elseCase.tpe.widen <:< NothingClass.tpe) =>

            if (elseCase equalsStructure Literal(Constant(null))) warn(opt2, UseOptionOrNull(identOrOpt(opt2), identOrOpt(opt2) + ".isDefined"))
            else if (getCase.tpe.widen <:< elseCase.tpe.widen)    warn(opt2, UseOptionGetOrElse(identOrOpt(opt2), identOrOpt(opt2) + ".isDefined"))

          case If(Select(Select(opt1, Name("isDefined")), nme.UNARY_!), elseCase, getCase @ Select(opt2, Name("get"))) //duplication
            if (opt1 equalsStructure opt2) && !(elseCase.tpe.widen <:< NothingClass.tpe) =>

            if (elseCase equalsStructure Literal(Constant(null))) warn(opt2, UseOptionOrNull(identOrOpt(opt2), "!" + identOrOpt(opt2) + ".isDefined"))
            else if (getCase.tpe.widen <:< elseCase.tpe.widen)    warn(opt2, UseOptionGetOrElse(identOrOpt(opt2), "!" + identOrOpt(opt2) + ".isDefined"))

          case If(Select(opt1, Name("isEmpty")), elseCase, getCase @ Select(opt2, Name("get"))) //duplication
            if (opt1 equalsStructure opt2) && !(elseCase.tpe.widen <:< NothingClass.tpe) =>

            if (elseCase equalsStructure Literal(Constant(null))) warn(opt2, UseOptionOrNull(identOrOpt(opt2), identOrOpt(opt2) + ".isEmpty"))
            else if (getCase.tpe.widen <:< elseCase.tpe.widen)    warn(opt2, UseOptionGetOrElse(identOrOpt(opt2), identOrOpt(opt2) + ".isEmpty"))

          case If(Select(Select(opt1, Name("isEmpty")), nme.UNARY_!), getCase @ Select(opt2, Name("get")), elseCase) //duplication
            if (opt1 equalsStructure opt2) && !(elseCase.tpe.widen <:< NothingClass.tpe) =>

            if (elseCase equalsStructure Literal(Constant(null))) warn(opt2, UseOptionOrNull(identOrOpt(opt2), "!" + identOrOpt(opt2) + ".isEmpty"))
            else if (getCase.tpe.widen <:< elseCase.tpe.widen)    warn(opt2, UseOptionGetOrElse(identOrOpt(opt2), "!" + identOrOpt(opt2) + ".isEmpty"))

          case If(Apply(Select(opt1, nme.NE), List(ScalaNone())), getCase @ Select(opt2, Name("get")), elseCase) //duplication
            if (opt1 equalsStructure opt2) && !(elseCase.tpe.widen <:< NothingClass.tpe) =>

            if (elseCase equalsStructure Literal(Constant(null))) warn(opt2, UseOptionOrNull(identOrOpt(opt2), identOrOpt(opt2) + "!= None"))
            else if (getCase.tpe.widen <:< elseCase.tpe.widen)    warn(opt2, UseOptionGetOrElse(identOrOpt(opt2), identOrOpt(opt2) + "!= None"))

          case If(Apply(Select(opt1, nme.EQ), List(ScalaNone())), elseCase, getCase @ Select(opt2, Name("get"))) //duplication
            if (opt1 equalsStructure opt2) && !(elseCase.tpe.widen <:< NothingClass.tpe) =>

            if (elseCase equalsStructure Literal(Constant(null))) warn(opt2, UseOptionOrNull(identOrOpt(opt2), identOrOpt(opt2) + "== None"))
            else if (getCase.tpe.widen <:< elseCase.tpe.widen)    warn(opt2, UseOptionGetOrElse(identOrOpt(opt2), identOrOpt(opt2) + "== None"))

          case Select(qualifier, name) if {//#microcosm

            //TODO: Possibly a bug - tried with regular class too
            import scala.language.reflectiveCalls

            object FilterFuncName {
              def unapply(func: Name): Option[String] = func match {
                case Name("filter"|"filterNot") => Some(func.toString)
                case _ => None
              }
            }
            object EmptyFuncName  {
              def unapply(func: Name): Option[String] = func match {
                case Name("isEmpty"|"nonEmpty"|"isDefined") => Some(func.toString)
                case _ => None
              }
            }

            /// find(...).isDefined is better written as exists(...)
            def warnUseExistsNotFindIsDefined(col: String, isDefinedFunc: String, pos: Position): Unit = {
              val replacements = ("exists", isDefinedFunc == "isEmpty")
              warn(pos, UseExistsNotFindIsDefined(col, replacements, isDefinedFunc))
            }

            ///[!]filter[Not](cond).[is|non]Empty -> [!]seq.[exists|forall]([!]cond) ... o_O
            //TODO: parse cond for simple _ ==/!= x conditions to improve suggestions
            def warnUseExistsNotFilterIsEmpty(col: String, stmtNegated: Boolean, filterFunc: String, emptyFunc: String, pos: Position): Unit = {
              val replacements = (
                if (filterFunc == "filter") {
                  if (emptyFunc == "isEmpty" ^ stmtNegated) {
                    Seq(("exists", true, false), ("forall", false, true))
                  } else {
                    Seq(("exists", false, false))
                  }
                } else {
                  if (emptyFunc == "isEmpty" ^ stmtNegated) {
                    Seq(("forall", false, false))
                  } else {
                    Seq(("exists", false, true), ("forall", true, false))
                  }
                }
              )
              warn(pos, UseExistsNotFilterIsEmpty(col, replacements, stmtNegated, filterFunc, emptyFunc))
              //Utils.noWarnPositions += filterEmpty.posHolder.pos // Prevents re-warning on negation
            }

            (qualifier, name) match {
              /// sortBy(...).head/last is better written as min/maxBy(...)
              case (Apply(Apply(TypeApply(Select(col, Name("sortBy")), _), List(_func)), List(ordering)), HeadOrLastName(func))
                if (col.tpe.baseClasses.exists(_.tpe =:= TraversableOnceClass.tpe))
                && (ordering.contains("Ordering")) =>

                val replacement = if (func == "head") "minBy" else "maxBy"
                warn(tree, UseMinOrMaxNotSort(identOrCol(col), "sortBy", func, replacement))

              case (Apply(ArrayOpsOrAugmentedString(), List(Apply(Apply(TypeApply(Select(col, Name("sortBy")), _), List(_func)), List(ordering)))), HeadOrLastName(func))
                if (ordering.contains("Ordering")) =>

                val replacement = if (func == "head") "minBy" else "maxBy"
                warn(tree, UseMinOrMaxNotSort(identOrCol(col), "sortBy", func, replacement))

              /// sorted(...).head/last is better written as min/max
              case (Apply(TypeApply(Select(col, Name("sorted")), _), List(ordering)), HeadOrLastName(func))
                if (col.tpe.baseClasses.exists(_.tpe =:= TraversableOnceClass.tpe))
                && (ordering.contains("Ordering")) =>

                val replacement = if (func == "head") "min" else "max"
                warn(tree, UseMinOrMaxNotSort(identOrCol(col), "sorted", func, replacement))

              case (Apply(ArrayOpsOrAugmentedString(), List(Apply(TypeApply(Select(col, Name("sorted")), _), List(ordering)))), HeadOrLastName(func))
                if (ordering.contains("Ordering")) =>

                val replacement = if (func == "head") "min" else "max"
                warn(tree, UseMinOrMaxNotSort(identOrCol(col), "sorted", func, replacement))

              case (Apply(pos @ Select(col, Name("find")), _cond), EmptyFuncName(emptyFunc))
                if col.tpe.baseClasses.exists(_.tpe =:= TraversableClass.tpe) =>

                warnUseExistsNotFindIsDefined(identOrCol(col), emptyFunc, pos.pos)

              case (Apply(ArrayOpsOrAugmentedString(), List(Apply(pos @ Select(col, Name("find")), List(_cond)))), EmptyFuncName(emptyFunc)) =>

                warnUseExistsNotFindIsDefined(identOrCol(col), emptyFunc, pos.pos)

              case (Apply(Select(Apply(ArrayOpsOrAugmentedString(), List(pos @ col)), Name("find")), List(_cond)), EmptyFuncName(emptyFunc)) =>

                warnUseExistsNotFindIsDefined(identOrCol(col), emptyFunc, pos.pos)

              case (Select(Apply(pos @ Select(col, FilterFuncName(filterFunc)), _cond), EmptyFuncName(emptyFunc)), nme.UNARY_!)
                if col.tpe.baseClasses.exists(c => c.tpe =:= TraversableOnceClass.tpe || c.tpe =:= OptionClass.tpe) =>

                warnUseExistsNotFilterIsEmpty(identOrCol(col), true, filterFunc, emptyFunc, pos.pos)

              case (Apply(pos @ Select(col, FilterFuncName(filterFunc)), _cond), EmptyFuncName(emptyFunc))
                if col.tpe.baseClasses.exists(c => c.tpe =:= TraversableOnceClass.tpe || c.tpe =:= OptionClass.tpe) =>

                warnUseExistsNotFilterIsEmpty(identOrCol(col), false, filterFunc, emptyFunc, pos.pos)

              case (Apply(Select(Apply(Select(Apply(ArrayOpsOrAugmentedString(), List(pos @ col)), FilterFuncName(filterFunc)), List(_cond)), EmptyFuncName(emptyFunc)), List()), nme.UNARY_!) =>

                warnUseExistsNotFilterIsEmpty(identOrCol(col), false, filterFunc, emptyFunc, pos.pos)

              case (Apply(ArrayOpsOrAugmentedString(), List(Apply(pos @ Select(col, FilterFuncName(filterFunc)), List(_cond)))), EmptyFuncName(emptyFunc)) =>

                warnUseExistsNotFilterIsEmpty(identOrCol(col), false, filterFunc, emptyFunc, pos.pos)

              case (Apply(Select(Apply(ArrayOpsOrAugmentedString(), List(pos @ col)), FilterFuncName(filterFunc)), List(_cond)), EmptyFuncName(emptyFunc)) =>

                warnUseExistsNotFilterIsEmpty(identOrCol(col), false, filterFunc, emptyFunc, pos.pos)

              case _ =>
            }

            false
          } => //Fallthrough

          /// exists(a == ...) is better written as contains(...)
          case Apply(Select(col, Name("exists")), List(Function(List(ValDef(_, param1, _, _)), Apply(Select(param2, Name("$eq$eq"|"eq")), List(id @ Ident(_))))))
            if (col.tpe.baseClasses.exists(c => c.tpe =:= TraversableOnceClass.tpe || (c.tpe =:= OptionClass.tpe && !Properties.versionString.contains("2.10"))))
            && (param1.toString == param2.toString) =>

            warn(tree, UseContainsNotExistsEquals(identOrCol(col), id.toString, param2.toString, id.toString))

          case Apply(Select(col, Name("exists")), List(Function(List(ValDef(_, param1, _, _)), Apply(Select(id @ Ident(_), Name("$eq$eq"|"eq")), List(param2)))))
            if (col.tpe.baseClasses.exists(c => c.tpe =:= TraversableOnceClass.tpe || (c.tpe =:= OptionClass.tpe && !Properties.versionString.contains("2.10"))))
            && (param1.toString == param2.toString) =>

            warn(tree, UseContainsNotExistsEquals(identOrCol(col), id.toString, id.toString, param2.toString))

          case Apply(Select(col, Name("exists")), List(Function(List(ValDef(_, param1, _, _)), Apply(Select(param2, Name("$eq$eq"|"eq")), List(Literal(Constant(lit)))))))
            if (col.tpe.baseClasses.exists(c => c.tpe =:= TraversableOnceClass.tpe || (c.tpe =:= OptionClass.tpe && !Properties.versionString.contains("2.10"))))
            && (param1.toString == param2.toString) =>

            warn(tree, UseContainsNotExistsEquals(identOrCol(col), String.valueOf(lit), param2.toString, String.valueOf(lit)))


          /// fold(true)(acc && xxx) => forall
          /// fold(false)(acc || xxx) => exists
          //TODO: fix foldRight, reduceRight
          case Apply(Apply(TypeApply(Select(col, Name(fold @ ("fold"|"foldLeft"|"$div$colon"))), List(_)), List(lit @ Literal(Constant(start)))), List(Function(List(ValDef(_, arg1, _, _), ValDef(_, arg2, _, _)), Apply(Select(arg1u, op), List(arg2u)))))
            if (col.tpe.baseClasses.exists(_.tpe =:= TraversableOnceClass.tpe))
            && (((true == start || false == start) // linter:disable:YodaConditions
                && ((arg1.toString == arg1u.toString) || (arg1.toString == arg2u.toString))
                && (op == nme.ZAND || op == nme.ZOR))
              || ((lit.tpe.widen weak_<:< DoubleClass.tpe)
                && (arg2u.tpe.widen =:= lit.tpe.widen)
                && ((arg1u.tpe.widen weak_<:< DoubleClass.tpe) || (arg2u.tpe.widen weak_<:< DoubleClass.tpe))
                && (Set(arg1.toString, arg2.toString) == Set(arg1u.toString, arg2u.toString))
                && (op == nme.ADD || op == nme.MUL))) =>

            op match {
              case nme.ZAND | nme.ZOR =>
                if (start == true && (op == nme.ZAND)) {
                  warn(tree, UseQuantifierFuncNotFold(identOrCol(col), "forall", fold))
                } else if (start == false && (op == nme.ZOR)) {
                  warn(tree, UseQuantifierFuncNotFold(identOrCol(col), "exists", fold))
                } else {
                  warn(tree, InvariantReturn(fold, start.toString))
                }
              case nme.ADD =>
                val replacement = (if (start == 0) "sum" else "sum + " + start)
                warn(tree, UseFuncNotFold(identOrCol(col), replacement, fold))
              case nme.MUL =>
                if (start == 0) {
                  warn(tree, InvariantReturn(fold, start.toString))
                } else {
                  val replacement = (if (start == 1) "product" else "product * " + start)
                  warn(tree, UseFuncNotFold(identOrCol(col), replacement, fold))
                }
              case _ => // Nope
            }

          /// reduce(acc && xxx) => forall
          /// reduce(acc || xxx) => exists
          case Apply(TypeApply(Select(col, Name(reduce @ ("reduce"|"reduceLeft"))), List(_)), List(Function(List(ValDef(_, arg1, _, _), ValDef(_, arg2, _, _)), Apply(Select(arg1u, op), List(arg2u)))))
            if (((arg1.toString == arg1u.toString) && (arg1u.tpe.widen <:< BooleanClass.tpe)
                ||(arg1.toString == arg2u.toString) && (arg2u.tpe.widen <:< BooleanClass.tpe))
                && (op == nme.ZAND || op == nme.ZOR))
              || (((arg1u.tpe.widen weak_<:< DoubleClass.tpe) || (arg2u.tpe.widen weak_<:< DoubleClass.tpe))
                && (Set(arg1.toString, arg2.toString) == Set(unwrapNumber(arg1u).toString, unwrapNumber(arg2u).toString))
                && (op == nme.ADD || op == nme.MUL || op.isAny("min", "max"))) =>

            op match {
              case nme.ZAND => warn(tree, UseQuantifierFuncNotFold(identOrCol(col), "forall", reduce))
              case nme.ZOR  => warn(tree, UseQuantifierFuncNotFold(identOrCol(col), "exists", reduce))
              case nme.ADD  => warn(tree, UseFuncNotReduce(identOrCol(col), "sum", reduce))
              case nme.MUL  => warn(tree, UseFuncNotReduce(identOrCol(col), "product", reduce))
              case _        => warn(tree, UseFuncNotReduce(identOrCol(col), op.toString, reduce))
            }

          /// col.map(...).map(...)
          //TODO: Too noisy e.g. col.map(_.x).map( more than 2 references to x ) would be uglier
          case Apply(TypeApply(Select(Apply(Apply(TypeApply(Select(col, Name("map")), _), List(Function(List(ValDef(_, _, _, _)), _))), List(_canBuildFrom)), Name("map")), _), List(Function(List(ValDef(_, _, _, _)), _)))
            if (col.tpe.baseClasses.exists(_.tpe =:= TraversableClass.tpe)) =>

            warn(tree, MergeMaps)

          /// swap operations col.map(...).take(...)
          //TODO: head, last could be like that too
          case Select(Apply(Apply(TypeApply(Select(col, Name("map")), _), List(Function(List(ValDef(_, _, _, _)), _))), List(canBuildFrom)), Name(func @ ("take"|"takeRight"|"drop"|"dropRight"|"headOption"|"lastOption"|"init"|"tail"|"slice")))
            if (col.tpe.baseClasses.exists(_.tpe =:= TraversableClass.tpe))
            && (canBuildFrom.toString contains "canBuildFrom") =>

            warn(tree, FuncFirstThenMap(func))

          case Select(Apply(Select(col, Name("map")), List(Function(List(ValDef(_, _, _, _)), _))), Name(func @ ("take"|"takeRight"|"drop"|"dropRight"|"headOption"|"lastOption"|"init"|"tail"|"slice")))
            if (col.tpe.baseClasses.exists(_.tpe =:= TraversableClass.tpe)) =>

            warn(tree, FuncFirstThenMap(func))

          /// swap operations col.sortWith(...).filter(...)
          case Apply(Select(Apply(Select(col, Name("sortWith")), List(Function(List(ValDef(_, _, _, _), ValDef(_, _, _, _)), _))), Name("filter"|"filterNot")), List(Function(List(ValDef(_, _, _, _)), _)))
            if (col.tpe.baseClasses.exists(_.tpe =:= TraversableClass.tpe)) =>

            warn(tree, FilterFirstThenSort)

          /// swap operations col.sortBy(...).filter(...)
          case Apply(Select(Apply(Apply(TypeApply(Select(col, Name("sortBy")), _), List(Function(List(ValDef(_, _, _, _)), _))), List(ordering)), Name("filter"|"filterNot")), List(Function(List(ValDef(_, _, _, _)), _)))
            if (col.tpe.baseClasses.exists(_.tpe =:= TraversableClass.tpe))
            && (ordering.contains("Ordering")) =>

            warn(tree, FilterFirstThenSort)

          /// swap operations col.sorted.filter(...)
          case Apply(Select(Apply(TypeApply(Select(col, Name("sorted")), _), List(ordering)), Name("filter"|"filterNot")), List(Function(List(ValDef(_, _, _, _)), _)))
            if (col.tpe.baseClasses.exists(_.tpe =:= TraversableClass.tpe))
            && (ordering.contains("Ordering")) =>

            warn(tree, FilterFirstThenSort)

          /// flatMap(if (...) Seq(x) else Seq(y)) is better written as map(if (...) x else y)
          //TODO: actually check all returns of the lambda
          case Apply(TypeApply(Select(col, Name("flatMap")), _), List(Function(List(ValDef(_, _param, _, _)), If(_, Apply(TypeApply(Select(col1, Name("apply")), _), List(_elt1)), Apply(TypeApply(Select(col2, Name("apply")), _), List(_elt2))))))
            if (col.tpe.baseClasses.exists(_.tpe =:= TraversableClass.tpe))
            && (col1.tpe.baseClasses.exists(_.tpe =:= TraversableFactoryClass.tpe))
            && (col2.tpe.baseClasses.exists(_.tpe =:= TraversableFactoryClass.tpe)) =>

            warn(tree, UseMapNotFlatMap(identOrCol(col)))

          /// flatMap(if (...) x else Nil/None) is better written as filter(...)
          case Apply(TypeApply(Select(col, Name("flatMap")), _), List(Function(List(ValDef(_, param, _, _)), If(_, e1, e2)))) =>

            // Swap branches, to simplify the matching
            val (expr1, expr2) = if (e1 endsWithAny (".Nil", ".None")) (e1, e2) else (e2, e1)

            (expr1, expr2) match {
              case (nil, Apply(TypeApply(Select(col, Name("apply")), _), List(Ident(id)))) // <- col is shadowing
                if (col.toString matches "((scala[.])?collection[.])?immutable[.].*")
                && (nil endsWith ".Nil")
                && (id.toString == param.toString) =>

                warn(tree, UseFilterNotFlatMap(identOrCol(col)))

              case (Apply(_Option2Iterable1, List(ScalaNone())), Apply(_Option2Iterable2, List(Apply(TypeApply(Select(ScalaSome(), Name("apply")), _), List(Ident(id))))))
                if (id.toString == param.toString) =>

                warn(tree, UseFilterNotFlatMap(identOrCol(col)))

              case _ =>
                //println((showRaw(expr1), showRaw(expr2)))
            }

            /// Warning about using Option.{ headOption, lastOption, tail }
            case Select(Apply(_option2Iterable, List(opt)), Name(func @ ("headOption"|"lastOption"|"init"|"tail")))
              if (opt.tpe.widen.baseClasses.exists(_.tpe =:= OptionClass.tpe)) =>

              warn(tree, AvoidOptionMethod(func))

            /// Warnings about using Option.size, which is probably a bug (use .isDefined instead)
            case t @ Select(Apply(option2Iterable, List(opt)), Name("size")) if (option2Iterable.toString contains "Option.option2Iterable") =>

              if (opt.tpe.widen.typeArgs.exists(tp => tp.widen <:< StringClass.tpe))
                warn(t, AvoidOptionStringSize)
              else if (opt.tpe.widen.typeArgs.exists(tp => tp.widen.baseClasses.exists(_.tpe =:= TraversableClass.tpe)))
                warn(t, AvoidOptionCollectionSize)
              else
                warn(t, AvoidOptionMethod("size", "use Option.isDefined instead."))

          /// Use x.transform instead of x = x.map(...)
          case Assign(id1, Apply(Apply(TypeApply(Select(Apply(xArrayOps, List(id2)), Name("map")), List(_, _)), List(_func)), List(Apply(TypeApply(canBuildFrom, List(_)), List(_typeTag)))))
            if (id1.toString == id2.toString)
            && (xArrayOps.contains("ArrayOps"))
            && (canBuildFrom.toString == "scala.this.Array.canBuildFrom") =>

            warn(tree, TransformNotMap("col")) //FIXME: name's inside xArrayOps, right?

          case Assign(id1, col @ Apply(Apply(TypeApply(Select(id2, Name("map")), List(_, _)), List(_func)), List(_)))
            if (id1.toString == id2.toString) && col.tpe.baseClasses.exists(_.tpe =:= MutableSeqLike.tpe) =>

            warn(tree, TransformNotMap(identOrCol(col)))

          /// Checks for duplicate mappings in a Map
          case Apply(TypeApply(Select(map, Name("apply")), _), args)
            if (map.tpe.baseClasses.exists(_.tpe <:< MapFactoryClass.tpe)) =>

            val keys = args flatMap {
              case Apply(TypeApply(Select(Apply(arrowAssoc, List(lhs)), Name(_arrow @ ("$minus$greater"|"$u2192"))), _), _rhs)
                if (arrowAssoc.toString matches "scala[.](this[.])?Predef[.](any2)?ArrowAssoc.+")
                && (lhs.isInstanceOf[Literal] || lhs.isInstanceOf[Ident]) => //TODO: support pure functions someday
                List(lhs)

              case Apply(tuple2apply, List(lhs, _rhs))
                if (tuple2apply startsWith "scala.Tuple2.apply")
                && (lhs.isInstanceOf[Literal] || lhs.isInstanceOf[Ident]) =>
                List(lhs)

              case _k => //TODO: unknown mapping format
                Nil
            }

            val unitResult = keys.foldLeft(List.empty[Tree])((acc, newItem) =>
              if (acc.exists(item => item equalsStructure newItem)) {
                warn(newItem, DuplicateKeyInMap)
                acc
              } else {
                newItem :: acc
              }
            )

          //// Inefficient use of .size, instead of is/nonEmpty (Idea by non)
          /// Inefficient use of List.size, instead of is/nonEmpty
          case Apply(Select(pos @ Select(obj, SizeOrLengthName(size)), op), List(Literal(Constant(x))))
            if (obj.tpe.widen.baseClasses.exists(_.tpe =:= ListClass.tpe) || obj.tpe.widen <:< StringClass.tpe)
            && (isEmptyCompare(x, op)) =>

            val replacementFunc = if (op == nme.EQ || op == nme.LE || op == nme.LT) "isEmpty" else "nonEmpty"

            warn(pos, InefficientUseOfListSize(identOrCol(obj), replacementFunc, size))

          /// Inefficient use of String.size, instead of is/nonEmpty (disabled)
          /*case Apply(Select(obj, op), List(Literal(Constant(""))))
            if (obj.tpe.widen <:< StringClass.tpe)
            && (op == nme.EQ || op == nme.NE) =>

            if (op == nme.EQ)
              warn(tree, "Use isEmpty instead of comparing to empty string.")
            else
              warn(tree, "Use nonEmpty instead of comparing to empty string.")*/

          /// Passing a block that returns a function to map, instead of a function -- http://scalapuzzlers.com/#pzzlr-001
          case Apply(TypeApply(Select(collection, Name("map"|"foreach"|"filter"|"filterNot"/*//TODO: add more*/)), _), List(block @ Block(list, Function(List(ValDef(paramMods, _, _, _)), _funcBody))))
            if paramMods.isSynthetic
            && list.nonEmpty // col.map(func), where func is an already defined function
            && (list match { // eta expansion, similar to above
              case List(ValDef(_, eta, _, _)) if eta.startsWith("eta$") => false
              case _ => true
            })
            && collection.tpe.baseClasses.exists(_.tpe =:= TraversableClass.tpe) =>

            warn(block, OnceEvaluatedStatementsInBlockReturningFunction)

          /// Integer division in an expression assigned to a floating point variable
          case Assign(varName, body) if isFloatingPointType(varName) && hasIntegerDivision(body) =>
            warn(body, IntDivisionAssignedToFloat)
          case Apply(Select(_var, varSetter), List(body)) if (varSetter endsWith "_$eq") && isFloatingPointType(body) && hasIntegerDivision(body) =>
            warn(body, IntDivisionAssignedToFloat)
          case ValDef(_, _, tpe, body) if isFloatingPointType(tpe) && hasIntegerDivision(body) =>
            warn(body, IntDivisionAssignedToFloat)
          case Literal(Constant(_)) if (intLiteralDiv contains tree.pos) && isFloatingPointType(tree.tpe) =>
            warn(tree, IntDivisionAssignedToFloat)

          /// col.flatten instead of col.filter(_.isDefined).map(_.get)
          case Apply(TypeApply(Select(Apply(Select(col, Name(filter)), List(Function(List(ValDef(_, p1, _, _)), Select(p1_, Name(isDefinedEmpty))))), Name("map")), _), List(Function(List(ValDef(_, p2, _, _)), Select(p2_, Name("get")))))
            if col.tpe.widen.baseClasses.exists(_.tpe =:= TraversableClass.tpe)
            && ((filter == "filter" && (isDefinedEmpty == "isDefined" || isDefinedEmpty == "nonEmpty"))
              ||(filter == "filterNot" && isDefinedEmpty == "isEmpty"))
            && (p1.toString == p1_.toString) && (p1_.tpe.widen.baseClasses.exists(_.tpe =:= OptionClass.tpe))
            && (p2.toString == p2_.toString) && (p2_.tpe.widen.baseClasses.exists(_.tpe =:= OptionClass.tpe)) =>

            warn(tree, UseFlattenNotFilterOption(identOrCol(col), filter, isDefinedEmpty))

          /// col.count(...) instead of col.filter(...).size/length (idea from pippi)
          case Select(Apply(Select(col, Name("filter")), List(_func)), SizeOrLengthName(size))
            if col.tpe.widen.baseClasses.exists(c => c.tpe =:= TraversableOnceClass.tpe || c.tpe =:= OptionClass.tpe) =>

            warn(tree, UseCountNotFilterLength(identOrCol(col), size))

          case Select(Apply(ArrayOpsOrAugmentedString(), List(Apply(Select(col, Name("filter")), List(_func)))), SizeOrLengthName(size)) =>

            warn(tree, UseCountNotFilterLength(identOrCol(col), size))

          /// col.exists(...) instead of col.count(...) == 0 (or similar)
          case Apply(Select(Apply(Select(col, Name("count")), List(_func)), op), List(Literal(Constant(x))))
            if col.tpe.widen.baseClasses.exists(c => c.tpe =:= TraversableOnceClass.tpe) && isEmptyCompare(x, op)
            && !(op == nme.EQ || op == nme.LE || op == nme.LT) =>

            warn(tree, UseExistsNotCountCompare(identOrCol(col)))

          case Select(Select(Select(col, Name("reverse")), Name("tail")), Name("reverse"))
            if col.tpe.widen.baseClasses.exists(c => c.tpe =:= TraversableClass.tpe) =>

            warn(tree, UseInitNotReverseTailReverse(identOrCol(col)))

          case Select(Apply(ArrayOpsOrAugmentedString(), List(Select(Apply(ArrayOpsOrAugmentedString(), List(Select(Apply(ArrayOpsOrAugmentedString(), List(col)), Name("reverse")))), Name("tail")))), Name("reverse")) =>

            warn(tree, UseInitNotReverseTailReverse(identOrCol(col)))

          case Select(Apply(Select(Select(col, Name("reverse")), Name("take")), _), Name("reverse"))
            if col.tpe.widen.baseClasses.exists(c => c.tpe =:= TraversableClass.tpe) =>

            warn(tree, UseTakeRightNotReverseTakeReverse(identOrCol(col)))

          case Select(Apply(ArrayOpsOrAugmentedString(), List(Apply(Select(Apply(ArrayOpsOrAugmentedString(), List(Select(Apply(ArrayOpsOrAugmentedString(), List(col)), Name("reverse")))), Name("take")), _))), Name("reverse")) =>

            warn(tree, UseTakeRightNotReverseTakeReverse(identOrCol(col)))

          case Select(Select(col, Name("reverse")), Name(head @ ("head"|"headOption")))
            if col.tpe.widen.baseClasses.exists(c => c.tpe =:= TraversableClass.tpe) =>

            warn(tree, UseLastNotReverseHead(identOrCol(col), option = head == "headOption"))

          case Select(Apply(ArrayOpsOrAugmentedString(), List(Select(Apply(ArrayOpsOrAugmentedString(), List(col)), Name("reverse")))), Name(head @ ("head"|"headOption"))) =>

            warn(tree, UseLastNotReverseHead(identOrCol(col), option = head == "headOption"))

          case Select(Select(col, Name("reverse")), Name(func @ ("map"|"iterator")))
            if col.tpe.widen.baseClasses.exists(c => c.tpe =:= TraversableClass.tpe) =>

            warn(tree, UseFuncNotReverse(identOrCol(col), func))

          case Select(Apply(ArrayOpsOrAugmentedString(), List(Select(Apply(ArrayOpsOrAugmentedString(), List(col)), Name("reverse")))), Name(func @ ("map"|"iterator"))) =>

            warn(tree, UseFuncNotReverse(identOrCol(col), func))

          case Apply(Select(col, Name("apply")), List(Literal(Constant(0))))
            if col.tpe.widen.baseClasses.exists(c => c.tpe =:= ListClass.tpe) =>

            warn(tree, UseHeadNotApply(identOrCol(col)))

          case Apply(Select(col, Name("apply")), List(Apply(Select(Select(col1, SizeOrLengthName(_)), nme.SUB), List(Literal(Constant(1))))))
            if col.tpe.widen.baseClasses.exists(c => c.tpe =:= ListClass.tpe)
            && (col equalsStructure col1) =>

            warn(tree, UseLastNotApply(identOrCol(col)))

          case If(cond, thenp, elsep) if {//#microcosm

            def warnUseHeadOrLastOptionNotIf(headOrLast: String, col: Tree): Unit = {
              headOrLast match {
                case "head" => warn(tree, UseHeadOptionNotIf(identOrCol(col)))
                case "last" => warn(tree, UseLastOptionNotIf(identOrCol(col)))
              }
            }

            (cond, thenp, elsep) match {
              // if (list.nonEmpty) Some(list.head) else None
              case (Select(col, Name("nonEmpty")), Apply(TypeApply(Select(ScalaSome(), Name("apply")), _), List(Select(col1, HeadOrLastName(headOrLast)))), ScalaNone()) if (col equalsStructure col1) =>

                warnUseHeadOrLastOptionNotIf(headOrLast, col)

              // if (list.isEmpty) None else Some(list.head)
              case (Select(col, Name("isEmpty")), ScalaNone(), Apply(TypeApply(Select(ScalaSome(), Name("apply")), _), List(Select(col1, HeadOrLastName(headOrLast))))) if (col equalsStructure col1) =>

                warnUseHeadOrLastOptionNotIf(headOrLast, col)

              // if (!list.isEmpty) Some(list.head) else None
              case (Select(Select(col, Name("isEmpty")), nme.UNARY_!), Apply(TypeApply(Select(ScalaSome(), Name("apply")), _), List(Select(col1, HeadOrLastName(headOrLast)))), ScalaNone()) if (col equalsStructure col1) =>

                warnUseHeadOrLastOptionNotIf(headOrLast, col)

              // if (str.nonEmpty) Some(str.head) else None
              case (Select(Apply(ArrayOpsOrAugmentedString(), List(col)), Name("nonEmpty")), Apply(TypeApply(Select(ScalaSome(), Name("apply")), _), List(Select(Apply(ArrayOpsOrAugmentedString(), List(col1)), HeadOrLastName(headOrLast)))), ScalaNone()) if (col equalsStructure col1) =>

                warnUseHeadOrLastOptionNotIf(headOrLast, col)

              // if (str.isEmpty) None else Some(s.head)
              case (Apply(Select(col, Name("isEmpty")), _), ScalaNone(), Apply(TypeApply(Select(ScalaSome(), Name("apply")), _), List(Select(Apply(ArrayOpsOrAugmentedString(), List(col1)), HeadOrLastName(headOrLast))))) if (col equalsStructure col1) =>

                warnUseHeadOrLastOptionNotIf(headOrLast, col)

              // if (!str.isEmpty) Some(str.head) else None
              case (Select(Apply(Select(col, Name("isEmpty")), _), nme.UNARY_!), Apply(TypeApply(Select(ScalaSome(), Name("apply")), _), List(Select(Apply(ArrayOpsOrAugmentedString(), List(col1)), HeadOrLastName(headOrLast)))), ScalaNone()) if (col equalsStructure col1) =>

                warnUseHeadOrLastOptionNotIf(headOrLast, col)

              case _ =>
            }

            false
          } => //Fallthrough

          case Apply(TypeApply(Select(col, Name("zip")), _), List(Select(col1, Name("indices"))))
            if (col equalsStructure col1)
            && col.tpe.widen.baseClasses.exists(c => c.tpe =:= TraversableClass.tpe) =>

            warn(tree, UseZipWithIndexNotZipIndices(identOrCol(col)))

          case Apply(TypeApply(Select(Apply(ArrayOpsOrAugmentedString(), List(col)), Name("zip")), _), List(Select(Apply(ArrayOpsOrAugmentedString(), List(col1)), Name("indices"))))
            if (col equalsStructure col1) =>

            warn(tree, UseZipWithIndexNotZipIndices(identOrCol(col)))

          /// Use partial function directly - temporary variable is unnecessary (idea by yzgw)
          case Apply(_, List(Function(List(ValDef(mods, x_1, typeTree: TypeTree, EmptyTree)), Match(x_1_, _))))
            if (((x_1 is "x$1") && (x_1_ is "x$1") && (mods.isSynthetic) && (mods.isParameter)) // _ match { ... }
            ||  ((x_1.toString == x_1_.toString) && !(mods.isSynthetic) && (mods.isParameter))) // x => x match { ... }
            && (typeTree.original == null) => // Fails on: x: Type => x match { ... }

            val param = if (x_1 is "x$1") "_" else x_1.toString + " => " + x_1.toString

            //TODO: also detects for (x <- col) x match { ... } ... current workaround with filter has false negatives
            val line = tree.pos.lineContent
            if (!List("for", "<-").exists(line.contains(_))) {
              warn(tree, PassPartialFunctionDirectly(param))
            }

          /// Using the implicit ordering for Unit is probably wrong
          case Apply(Apply(TypeApply(Select(_, Name(minOrMaxBy @ ("minBy"|"maxBy"))), _), List(Function(_, Block(_, u @ Literal(Constant(())))))), List(Select(scala_math_Ordering, Name("Unit"))))
            if (scala_math_Ordering is "math.this.Ordering") =>

            warn(u, UnitImplicitOrdering(minOrMaxBy))

          case _ =>
            //if (tree.toString contains "...") println(showRaw(tree))
        }
      } catch catcher finally finalizer(tree)
    }
  }

  //// Abstract Interpreter
  private[this] object PostTyperInterpreterComponent extends PluginComponent {
    val global = LinterPlugin.this.global
    import global._

    override val runsAfter = List("typer")

    val phaseName = "linter-typed-interpreter"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: global.CompilationUnit): Unit = {
        if (!unit.isJava) {
          nowarnPositions.clear
          new PostTyperInterpreterTraverser(unit).traverse(unit.body)
        }
      }
    }

    class PostTyperInterpreterTraverser(unit: CompilationUnit) extends Traverser {
      implicit val unitt: CompilationUnit = unit
      var treePosHolder: Tree = null
      val utils = new Utils[global.type](global)
      import utils._

      import definitions.{ AnyClass, AnyValClass, NothingClass, PredefModule, ObjectClass, Object_== }
      import definitions.{ OptionClass, SeqClass, TraversableClass, ListClass, StringClass }
      import definitions.{ DoubleClass, FloatClass, CharClass, ByteClass, ShortClass, IntClass, LongClass, BooleanClass }

      def checkRegex(reg: String): Unit = {
        try {
          reg.r // Throws PatternSyntaxException if invalid regex
          if (reg matches "[\\^]?([|]+|([(][|]*[)])+)*[$]?")
            warn(treePosHolder, RegexWarning("Regex "+reg+" matches only empty string", error = false))
        } catch {
          case e: java.util.regex.PatternSyntaxException =>
            warn(treePosHolder, RegexWarning(e.getDescription))
          case _: Exception =>
        }
      }

      // Data structure ideas:
      // 1. common trait for Value, Collection, StringAttrs
      // 2. for Values: empty -> any, and add "nonValues" or "conditions" to cover
      //    if (a == 0) ..., if (a%2 == 0) ... even for huge collections

      //// Integer/List value data structure
      object Values {
        lazy val empty = new Values()
        def apply(i: Int, name: String = ""): Values = new Values(name = name, values = Set(i))
        def apply(low: Int, high: Int): Values = new Values(ranges = Set((low, high)))
      }
      class Values(
          val ranges: Set[(Int, Int)] = Set.empty,
          val values: Set[Int] = Set.empty,
          val conditions: Set[Int => Boolean] = Set.empty, // Currently obeyed only ifEmpty
          val name: String = "",
          val isSeq: Boolean = false,
          val actualSize: Int = -1
        ) {

        //require(isEmpty || isValue || isSeq || (ranges.size == 1 && values.size == 0) || (ranges.size == 0 && values.size > 0))
        //println(this)

        def conditionsContain(i: Int): Boolean = conditions.forall(c => c(i))

        // Experimental alternative to empty
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

        def addRange(low: Int, high: Int): Values = new Values(ranges + (if (low > high) (high, low) else (low, high)), values, conditions, name, false, -1)
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
          if (t.values.size == 0 && t.ranges.size == 1) {
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

        // Discard values, that are inside ranges
        def optimizeValues: Values = new Values(ranges, values.filter(v => !rangesContain(v)), conditions, name, isSeq, actualSize)
        //TODO: def optimizeRanges =

        def isEmpty: Boolean = this.size == 0
        def nonEmpty: Boolean = this.size > 0
        def isValue: Boolean = this.values.size == 1 && this.ranges.isEmpty //TODO: this is stupid and buggy, and woudn't exist if I didn't mix all types into one class
        def getValue: Int = if (isValue) values.head else throw new Exception()
        def isValueForce: Boolean = (this.values.size == 1 && this.ranges.isEmpty) || (ranges.size == 1 && this.size == 1)
        def getValueForce: Int = if (isValue) values.head else if (ranges.size == 1 && this.size == 1) ranges.head._1 else throw new Exception()

        def max: Int = math.max(if (values.nonEmpty) values.max else Int.MinValue, if (ranges.nonEmpty) ranges.maxBy(_._2)._2 else Int.MinValue)
        def min: Int = math.min(if (values.nonEmpty) values.min else Int.MaxValue, if (ranges.nonEmpty) ranges.minBy(_._1)._1 else Int.MaxValue)

        def dropValue(i: Int): Values = new Values(
          ranges.flatMap { case (low, high) =>
            if (i > low && i < high) List((low, i-1), (i+1, high)) else
            if (i == low && i < high) List((low+1, high)) else
            if (i > low && i == high) List((low, high-1)) else
            if (i == low && i == high) Nil else
            List((low, high))
          },
          values - i,
          Set.empty, //ADD: conditions?
          name)

        // Beware of some operations over ranges.
        def map(func: Int => Int, rangeSafe: Boolean = true): Values = //ADD: conditions?
          if (rangeSafe) {
            new Values(
              ranges.map { case (preLow, preHigh) =>
                  val (low, high) = (func(preLow), func(preHigh))
                  if (low > high) (high, low) else (low, high)
              },
              values.map(func))
          } else {
            if (this.ranges.nonEmpty && this.size <= 100001) //ADD: this only checks small ranges, also can still get slow
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
              val (left, right) = (this.applyCond(expr1)._1, this.applyCond(expr2)._1)
              new Values(
                left.ranges ++ right.ranges,
                left.values ++ right.values,
                left.conditions ++ right.conditions,
                this.name)

            // Pass on String functions
            case strFunc @ Apply(Select(string, _func), _params)
              if string.tpe != null && string.tpe.widen <:< StringClass.tpe =>

              computeExpr(strFunc)

            case strFunc @ Select(Apply(scala_augmentString, List(_string)), _func)
              if (scala_augmentString.toString endsWith "augmentString") =>

              computeExpr(strFunc)

            //ADD: expr op expr that handles idents right
            case Apply(Select(Ident(v), op), List(expr)) if v.toString == this.name && computeExpr(expr).isValue =>

              val value = computeExpr(expr).getValue
              val out: Values = op match {
                case nme.EQ =>
                  (if (this.nonEmpty) {
                    if (this.contains(value)) {
                      if (this.forallEquals(value)) alwaysHold = true
                      Values(value).addName(name)
                    } else {
                      neverHold = true
                      this.makeUnknown
                    }
                  } else if (!this.conditionsContain(value)) {
                    neverHold = true
                    this.makeUnknown
                  } else {
                    this.makeUnknown
                  }).addCondition(_ == value)
                case nme.NE =>
                  (if (this.contains(value)) {
                    val out = this.dropValue(value)
                    if (out.isEmpty) neverHold = true
                    out
                  } else if (this.nonEmpty) {
                    alwaysHold = true
                    this
                  } else {
                    this.makeUnknown
                  }).addCondition(_ != value)
                case nme.GT => new Values(
                    ranges.flatMap { case (low, high) => if (low > value) Some((low, high)) else if (high > value) Some((value+1, high)) else None },
                    values.filter(_ > value),
                    Set(_ > value),
                    this.name)
                case nme.GE => new Values(
                    ranges.flatMap { case (low, high) => if (low >= value) Some((low, high)) else if (high >= value) Some((value, high)) else None },
                    values.filter(_ >= value),
                    Set(_ >= value),
                    this.name)
                case nme.LT => new Values(
                    ranges.flatMap { case (low, high) => if (high < value) Some((low, high)) else if (low < value) Some((low, value-1)) else None },
                    values.filter(_ < value),
                    Set(_ < value),
                    this.name)
                case nme.LE => new Values(
                    ranges.flatMap { case (low, high) => if (high <= value) Some((low, high)) else if (low <= value) Some((low, value)) else None },
                    values.filter(_ <= value),
                    Set(_ <= value),
                    this.name)
                case _ =>
                  //println("applyCond: "+showRaw( a ));
                  this.makeUnknown
              }

              if ((Set[Name](nme.GT, nme.GE, nme.LT, nme.LE) contains op) && this.nonEmpty) {
                if (out.isEmpty) neverHold = true
                if (out.size == this.size) alwaysHold = true
              }

              out
            case Apply(Select(expr1, op), List(expr2)) =>
              val (left, right) = (computeExpr(expr1), computeExpr(expr2))
              var out = Values.empty.addActualSize(this.actualSize)
              op match {
                case nme.EQ | nme.NE =>
                  if (left.isValue && right.isValue && left.getValue == right.getValue) {
                    if (op == nme.EQ) alwaysHold = true else neverHold = true
                    if (op == nme.EQ && (left.name == this.name || right.name == this.name)) out = this
                  } else if (left.nonEmpty && right.nonEmpty && (left.min > right.max || right.min > left.max)) {
                    if (op != nme.EQ) alwaysHold = true else neverHold = true
                    if (op != nme.EQ && (left.name == this.name || right.name == this.name)) out = this
                  } else if (left.name == this.name && right.isValueForce && op == nme.EQ) {
                    out = Values(right.getValueForce).addName(this.name)
                  } else if (right.name == this.name && left.isValueForce && op == nme.EQ) { // Yoda conditions
                    out = Values(left.getValueForce).addName(this.name)
                  }
                  if (left.isEmpty && left.conditions.nonEmpty && right.isValue && !left.conditionsContain(right.getValue)) {
                    neverHold = true
                  }

                case nme.GT | nme.LE =>
                  if (left.isValue && right.isValue) {
                    if (left.getValue > right.getValue) {
                      if (op == nme.GT) alwaysHold = true else neverHold = true
                      if (op == nme.GT && (left.name == this.name || right.name == this.name)) out = this
                    } else {
                      if (op != nme.GT) alwaysHold = true else neverHold = true
                      if (op != nme.GT && (left.name == this.name || right.name == this.name)) out = this
                    }
                  } else if (left.nonEmpty && right.nonEmpty) {
                    if (left.max <= right.min) {
                      if (op != nme.GT) alwaysHold = true else neverHold = true
                      if (op != nme.GT && (left.name == this.name || right.name == this.name)) out = this
                    } else if (left.min > right.max) {
                      if (op == nme.GT) alwaysHold = true else neverHold = true
                      if (op == nme.GT && (left.name == this.name || right.name == this.name)) out = this
                    }
                  }

                case nme.GE | nme.LT =>
                  if (left.isValue && right.isValue) {
                    if (left.getValue >= right.getValue) {
                      if (op == nme.GE) alwaysHold = true else neverHold = true
                      if (op == nme.GE && (left.name == this.name || right.name == this.name)) out = this
                    } else {
                      if (op != nme.GE) alwaysHold = true else neverHold = true
                      if (op != nme.GE && (left.name == this.name || right.name == this.name)) out = this
                    }
                  } else if (left.nonEmpty && right.nonEmpty) {
                    if (left.max < right.min) {
                      if (op != nme.GE) alwaysHold = true else neverHold = true
                      if (op != nme.GE && (left.name == this.name || right.name == this.name)) out = this
                    } else if (left.min >= right.max) {
                      if (op == nme.GE) alwaysHold = true else neverHold = true
                      if (op == nme.GE && (left.name == this.name || right.name == this.name)) out = this
                    }
                  }

                case _ =>
              }

              if (this.name == out.name) out.addConditions(this.conditions) else out
            case Select(expr, op) =>
              computeExpr(expr).applyUnary(op)
              Values.empty.addActualSize(this.actualSize)

            case expr =>
              //println("expr: "+showRaw(expr))
              computeExpr(expr)
              Values.empty.addActualSize(this.actualSize)
          }


          // Check if condition will always or never hold
          if (neverHold) warn(condExpr, InvariantCondition(always = false, "hold"))
          if (alwaysHold) warn(condExpr, InvariantCondition(always = true, "hold"))

          //TODO: verify filter = ...
          if (!isUsed(condExpr, this.name, filter = "size|length|head|last")) (this, this) else (out, if (neverHold) this else if (alwaysHold) Values.empty else this - out)
        }

        //TODO: does this even work? the v map is suspect and ugly
        def -(value: Values): Values = {
          if (this.isEmpty || value.isEmpty) {
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
          case Name(name) => name match {
            case "signum" => this.map(a => math.signum(a)).addCondition({ a => Set(-1, 0, +1) contains a })
            case "abs" =>
              new Values(//ADD: conditions
                ranges.map { case (preLow, preHigh) =>
                  val (low, high) = (if (preLow > 0) preLow else 0, math.max(math.abs(preLow), math.abs(preHigh)))
                  if (low > high) (high, low) else (low, high)
                },
                values.map(a => math.abs(a))).addCondition(_ >= 0)

            case "size"|"length" => if (this.actualSize != -1) Values(this.actualSize) else Values.empty.addCondition(_ >= 0)
            case headOrLast @ ("head"|"last") =>
              // Only works for one element :)
              if (this.actualSize == 0) warn(treePosHolder, DecomposingEmptyCollection(headOrLast))
              if (this.actualSize == 1 && this.size == 1) Values(this.getValueForce) else Values.empty
            case tailOrInit @ ("tail"|"init") if this.actualSize != -1 =>
              if (this.actualSize == 0) {
                warn(treePosHolder, DecomposingEmptyCollection(tailOrInit))
                Values.empty
              } else {
                Values.empty.addActualSize(this.actualSize - 1)
              }

            case "toIndexedSeq"|"toList"|"toSeq"|"toVector" => this //only immutable
            case "distinct" if this.actualSize != -1 =>
              val out = this.distinct
              out.addActualSize(out.size)

            case "reverse" => this // Will hold, while Set is used for values
            case "max" if this.nonEmpty => Values(this.max)
            case "min" if this.nonEmpty => Values(this.min)
            case "sum" if this.nonEmpty && this.distinct.size == this.actualSize => Values(this.sum)

            case "isEmpty" if (this.actualSize != -1) =>
              warn(treePosHolder, InvariantCondition(always = this.actualSize == 0, "hold"))
              Values.empty
            case "nonEmpty" if (this.actualSize != -1) =>
              warn(treePosHolder, InvariantCondition(always = this.actualSize > 0, "hold"))
              Values.empty

            case _ =>
              //val raw = showRaw( treePosHolder );
              //println("applyUnary: op:"+op+" thisval:"+this+" tree:"+treePosHolder.toString+"\n"+raw);
              Values.empty
          }
        }
        //// Apply a binary operation on a Integer/List variable
        def applyBinary(op: Name, right: Values): Values = {
          val left = this

          val (func, isRangeSafe): ((Int, Int) => Int, Boolean) =
            op match {
              case nme.ADD => (_ + _, true)
              case nme.SUB => (_ - _, true)
              case nme.MUL => (_ * _, false) //ADD: are all these really unsafe for ranges (where you only process first and last)
              case nme.AND => (_ & _, false)
              case nme.OR  => (_ | _, false)
              case nme.XOR => (_ ^ _, false)
              case nme.LSL => (_ << _, false)
              case nme.LSR => (_ >>> _, false)
              case nme.ASR => (_ >> _, false)
              case nme.MOD if right.size == 1 && right.getValueForce != 0 => (_ % _, false)
              case nme.DIV if right.size == 1 && right.getValueForce != 0 => (_ / _, false)
              case Name("apply"|"take"|"drop"|"max"|"min"|"contains"|"map"|"count") => ((a: Int, b: Int) => throw new Exception(), false) // Check code below
              case _ => return Values.empty
           }

          val out: Values = (
            if (op.toString == "count") {
              (if (left.actualSize == 0) Values(0) else if (left.actualSize > 0) Values.empty.addCondition(_ < left.actualSize) else Values.empty).addCondition(_ >= 0)
            } else if (left.isEmpty || right.isEmpty) {
              //ADD: x & 2^n is Set(2^n, 0) and stuff like that :)
              if (left.contains(0) || right.contains(0)) {
                if (Set[Name](nme.MUL, nme.AND) contains op) Values(0) else Values.empty
              } else {
                Values.empty
              }
            } else if (op.toString == "apply") {
              //TODO: if you wanted actual values, you need to save seq type and refactor values from Set to Seq
              if (left.isSeq && left.actualSize == 1 && left.size == 1 && right.isValueForce && right.getValueForce == 0) Values(left.getValueForce) else left
            } else if (op.toString == "map") {
              right
            } else if (op.toString == "contains") {
              if (right.isValue) {
                warn(treePosHolder, InvariantCondition(always = left.contains(right.getValue), "return true"))
              }
              Values.empty
            } else if (op.toString == "max") {
              if (left.isValue && right.isValue) {
                if (left.getValue >= right.getValue) {
                  warn(treePosHolder, InvariantExtrema(max = true, returnsFirst = true))
                } else {
                  warn(treePosHolder, InvariantExtrema(max = true, returnsFirst = false))
                }
                Values(math.max(left.getValue, right.getValue))
              } else if (left.isValue && !right.isValue) {
                if (left.getValue >= right.max) {
                  warn(treePosHolder, InvariantExtrema(max = true, returnsFirst = true))
                  Values(left.getValue)
                } else if (left.getValue <= right.min) {
                  warn(treePosHolder, InvariantExtrema(max = true, returnsFirst = false))
                  right
                } else {
                  Values.empty
                }
              } else if (!left.isValue && right.isValue) {
                if (right.getValue >= left.max) {
                  warn(treePosHolder, InvariantExtrema(max = true, returnsFirst = false))
                  Values(right.getValue)
                } else if (right.getValue <= left.min) {
                  warn(treePosHolder, InvariantExtrema(max = true, returnsFirst = true))
                  left
                } else {
                  Values.empty
                }
              } else {
                Values.empty
              }
            } else if (op.toString == "min") {
              if (left.isValue && right.isValue) {
                if (left.getValue <= right.getValue) {
                  warn(treePosHolder, InvariantExtrema(max = false, returnsFirst = true))
                } else {
                  warn(treePosHolder, InvariantExtrema(max = false, returnsFirst = false))
                }
                Values(math.min(left.getValue, right.getValue))
              } else if (left.isValue && !right.isValue) {
                if (left.getValue <= right.min) {
                  warn(treePosHolder, InvariantExtrema(max = false, returnsFirst = true))
                  Values(left.getValue)
                } else if (left.getValue > right.max) {
                  warn(treePosHolder, InvariantExtrema(max = false, returnsFirst = false))
                  right
                } else {
                  Values.empty
                }
              } else if (!left.isValue && right.isValue) {
                if (right.getValue <= left.min) {
                  warn(treePosHolder, InvariantExtrema(max = false, returnsFirst = false))
                  Values(right.getValue)
                } else if (right.getValue > left.max) {
                  warn(treePosHolder, InvariantExtrema(max = false, returnsFirst = true))
                  left
                } else {
                  Values.empty
                }
              } else {
                Values.empty
              }
            } else if (op.toString == "take") {
              if (left.isSeq && left.actualSize != -1 && right.isValue) {
                if (right.getValueForce >= left.actualSize) {
                  warn(treePosHolder, UnnecessaryMethodCall("take"))
                  this
                } else {
                  if (right.getValueForce <= 0) warn(treePosHolder, ProducesEmptyCollection)
                  Values.empty.addName(name).addActualSize(math.max(0, right.getValueForce))
                }
              } else {
                Values.empty
              }
            } else if (op.toString == "drop") {
              if (left.isSeq && left.actualSize != -1 && right.isValue) {
                if (right.getValueForce <= 0) {
                  warn(treePosHolder, UnnecessaryMethodCall("drop"))
                  this
                } else {
                  if (left.actualSize-right.getValueForce <= 0) warn(treePosHolder, ProducesEmptyCollection)
                  Values.empty.addName(name).addActualSize(math.max(0, left.actualSize-right.getValueForce))
                }
              } else {
                Values.empty
              }
            } else if (left.isValue && right.isValue) {
              if (left.getValue == right.getValue && !left.name.isEmpty) {
                if (op == nme.SUB) warn(treePosHolder, OperationAlwaysProducesZero("subtraction"))
                if (op == nme.XOR) warn(treePosHolder, OperationAlwaysProducesZero("exclusive or"))
              }
              Values(func(left.getValue, right.getValue))
            } else if (!left.isValue && right.isValue) {
              left.map(a => func(a, right.getValue), rangeSafe = isRangeSafe)
            } else if (left.isValue && !right.isValue) {
              right.map(a => func(left.getValue, a), rangeSafe = isRangeSafe)
            } else {
              //ADD: join ranges, but be afraid of the explosion :)
              if (left eq right) {
                op match {
                  //case nme.ADD => left.map(a => a+a) // nope, wrong
                  //case nme.MUL => left.map(a => a*a)
                  case nme.DIV if (!left.contains(0)) => Values(1) //TODO: never gets executed?
                  case nme.SUB | nme.XOR =>
                    if (op == nme.SUB) warn(treePosHolder, OperationAlwaysProducesZero("subtraction"))
                    if (op == nme.XOR) warn(treePosHolder, OperationAlwaysProducesZero("exclusive or"))
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
                if (left.isValue && right.isValue)
                  (math.min(left.getValue, right.getValue), math.max(left.getValue, right.getValue))
                else if (left.isValue)
                  (left.getValue, left.getValue)
                else //if (right.isValue)
                  (right.getValue, right.getValue)
              )

              if (op == nme.AND) out.addConditions(_ <)*/
            case _ => out
          }
        }

        // Approximate size
        def size: Int = values.size + ranges.foldLeft(0)((acc, range) => acc + (range._2 - range._1) + 1)

        override def toString: String =
          "Values("+(if (name.size > 0) name+")(" else "")+(values.map(_.toString) ++ ranges.map(a => a._1+"-"+a._2)).mkString(",")+", "+isSeq+", "+actualSize+")"
      }

      val SeqLikeObject: Symbol = rootMirror.getModuleByName(newTermName("scala.collection.GenSeq"))
      val SeqLikeClass: Symbol = rootMirror.getClassByName(newTermName("scala.collection.SeqLike"))
      val SeqLikeContains: Symbol = SeqLikeClass.info.member(newTermName("contains"))
      val SeqLikeApply: Symbol = SeqLikeClass.info.member(newTermName("apply"))
      val SeqLikeGenApply: Symbol = SeqLikeObject.info.member(newTermName("apply"))
      def methodImplements(method: Symbol, target: Symbol): Boolean =
        try { method == target || method.allOverriddenSymbols.contains(target) } catch { case _: NullPointerException => false }

      //TODO: extend with more functions... and TEST TEST TEST TEST
      //// Attempt to compute part of the AST, and return Integer/List value
      def computeExpr(tree: Tree): Values = {
        if (doNotTraverse contains tree) return Values.empty
        treePosHolder = tree
        val out: Values = tree match {
          case Literal(Constant(value: Int)) => Values(value)
          //TODO: I don't think .this. ones work at all...
          case Select(This(_type), termName) =>
            val name = termName.toString
            val n = (if (name.contains(".this.")) name.substring(name.lastIndexOf('.')+1) else name).trim
            vals(n)
          case Ident(termName) =>
            val name = termName.toString
            val n = (if (name.contains(".this.")) name.substring(name.lastIndexOf('.')+1) else name).trim
            //println(n+": "+vals(n))
            if (vals contains n) vals(n) else if ((defModels contains n) && defModels(n).isLeft) defModels(n).left.get else Values.empty
          case Apply(Ident(termName), _) if defModels contains termName.toString =>
            val n = termName.toString
            if (vals contains n) vals(n) else if ((defModels contains n) && defModels(n).isLeft) defModels(n).left.get else Values.empty

          // String size
          /*case Apply(Select(Ident(id), length), Nil) if stringVals.exists(_.name == Some(id.toString)) && length.toString == "length" =>
            val exactValue = stringVals.find(_.name == Some(id.toString)).map(_.exactValue)
            exactValue.map(v => if (v.isDefined) Values(v.get.size) else Values.empty).getOrElse(Values.empty)

          case Select(Apply(Select(predef, augmentString), List(Ident(id))), size)
            if stringVals.exists(_.name == Some(id.toString)) && predef.toString == "scala.this.Predef" && augmentString.toString == "augmentString" && size.toString == "size" =>
            val exactValue = stringVals.find(_.name == Some(id.toString)).map(_.exactValue)
            exactValue.map(v => Values(v.size)).getOrElse(Values.empty)*/

          // String stuff (TODO: there's a copy up at applyCond)
          case _strFunc @ Apply(Select(string, func), params) if string.tpe.widen <:< StringClass.tpe =>
            StringAttrs.stringFunc(string, func, params).right.getOrElse(Values.empty)

          case _strFunc @ Select(Apply(scala_augmentString, List(string)), func)
            if (scala_augmentString.toString endsWith "augmentString") =>

            StringAttrs.stringFunc(string, func).right.getOrElse(Values.empty)

          case _strFunc @ Apply(Select(Apply(scala_augmentString, List(string)), func), params)
            if (scala_augmentString.toString endsWith "augmentString") =>

            StringAttrs.stringFunc(string, func, params).right.getOrElse(Values.empty)

            case Apply(Select(Apply(scala_StringContext_apply, _literalList), interpolator), _paramList)
              if (scala_StringContext_apply.toString == "scala.StringContext.apply")
              && (interpolator.toString == "s") =>

            StringAttrs(tree)

            Values.empty

          /// Division by zero
          case pos @ Apply(Select(num, op), List(expr))
            if (op == nme.DIV || op == nme.MOD)
            && (num.tpe.widen weak_<:< DoubleClass.tpe) && {
              val denom = computeExpr(expr)
              if (denom.isValue && denom.getValue == 1) {
                if (op == nme.DIV) {
                  warn(pos, DivideByOne)
                } else if (op == nme.MOD && (num.tpe.widen weak_<:< LongClass.tpe)) {
                  warn(pos, ModuloByOne)
                }

                false //Fallthrough
              } else if (denom.contains(0)) {
                warn(pos, DivideByZero)

                true
              } else {
                val numer = computeExpr(num)
                if (numer.isValue && numer.getValue == 0) {
                  warn(pos, ZeroDivideBy)
                }

                false //Fallthrough
              }
            } =>
            Values.empty

          // Range
          case Apply(Select(Apply(scala_Predef_intWrapper, List(Literal(Constant(low: Int)))), Name(toOrUntil @ ("to"|"until"))), List(Literal(Constant(high: Int))))
            if (scala_Predef_intWrapper.toString endsWith "Wrapper") =>

            val high2 = if (toOrUntil == "to") high else high-1
            new Values(Set((low, high2)), Set.empty, Set.empty, "", isSeq = true, high2-low)

          /// Use (low until high) instead of (low to high-1)
          case Apply(Select(Apply(scala_Predef_intWrapper, List(expr1)), Name(toOrUntil @ ("to"|"until"))), List(expr2))
            if (scala_Predef_intWrapper.toString endsWith "Wrapper") =>

            if (toOrUntil == "to") {
              (expr1, expr2) match {
                case (Literal(Constant(_)), Apply(Select(expr, nme.SUB), List(Literal(Constant(1))))) =>
                  if (expr match {
                    case Ident(_) => true
                    case Select(Ident(_), Name("size"|"length")) => true // size value
                    case Apply(Select(Ident(_), Name("size"|"length")), Nil) => true // size getter
                    case Select(Apply(_implicitWrapper, List(Ident(_))), Name("size"|"length")) => true // wrapped size
                    case _ => false
                  }) warn(treePosHolder, UseUntilNotToMinusOne)
                case _ =>
              }
            }

            if (computeExpr(expr1).isValue && computeExpr(expr2).isValue) {
              val (low, high) = (computeExpr(expr1).getValue, computeExpr(expr2).getValue + (if (toOrUntil == "to") 0 else -1))

              new Values(Set((low, high)), Set.empty, Set.empty, name = "", isSeq = true, actualSize = high-low)
            } else {
              Values.empty
            }

          case t @ Apply(TypeApply(genApply @ Select(_, _), _), genVals)
            if methodImplements(genApply.symbol, SeqLikeGenApply) && (!t.tpe.widen.toString.contains("collection.mutable.")) =>

            val values = genVals.map(computeExpr)
            if (values.forall(_.isValue)) new Values(values = values.map(_.getValue).toSet, isSeq = true, actualSize = values.size) else Values.empty.addActualSize(genVals.size)

          //TODO: Array isn't immutable
          //case Apply(Select(Select(scala, scala_Array), apply), genVals) if (scala_Array.toString == "Array") =>

          //TODO: is this for array or what?
          case Select(Apply(arrayOps @ Select(_, _intArrayOps), List(expr)), op) if (arrayOps.toString == staticSelect("scala", "Predef.intArrayOps")) =>
            computeExpr(expr).applyUnary(op)

          case Apply(TypeApply(t @ Select(_expr, _op), _), List(scala_math_Ordering_Int)) if scala_math_Ordering_Int.toString.endsWith("Int") => // .max .min
            computeExpr(t)

          case Apply(Select(scala_math_package, op), params) if scala_math_package.toString == "scala.math.`package`" =>
            op.toString match {
              case "abs"|"signum" if params.size == 1 => computeExpr(params.head).applyUnary(op)
              case "max"|"min"    if params.size == 2 => computeExpr(params(0)).applyBinary(op, computeExpr(params(1)))
              case _ => Values.empty
            }

          /// Parameter of Random.nextInt might be lower than 1 (runtime exception)
          case Apply(Select(scala_util_Random, nextInt), params) if nextInt.toString == "nextInt" && scala_util_Random.tpe <:< rootMirror.getClassByName(newTermName("scala.util.Random")).tpe =>
            if (params.size == 1) {
              val param = computeExpr(params.head)
              if (param.nonEmpty) {
                if (param.min <= 0) {
                  warn(treePosHolder, InvalidParamToRandomNextInt)
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

          case Apply(Apply(TypeApply(Select(valName, map), List(_, _)), List(Function(List(ValDef(_, paramName, _, EmptyTree)), expr))), _) if (map.toString == "map") =>
            //List(TypeApply(Select(Select(This(newTypeName("immutable")), scala.collection.immutable.List), newTermName("canBuildFrom")), List(TypeTree()))))

            pushDefinitions()

            val res = computeExpr(valName)
            vals += paramName.toString -> res
            //println(">        "+vals)
            val out = computeExpr(expr)

            popDefinitions()
            //println(">        "+out)
            out

          case If(_, expr1, expr2) =>
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

            //if (e1.isValue && e2.isValue) new Values(values = e1.values ++ e2.values) else Values.empty
            if (expr1.tpe <:< NothingClass.tpe) {
              e2
            } else if (expr2.tpe <:< NothingClass.tpe) {
              e1
            } else if (!e1.isSeq && !e2.isSeq && e1.nonEmpty && e2.nonEmpty) {
              new Values(values = e1.values ++ e2.values, ranges = e1.ranges ++ e2.ranges)
            } else {
              Values.empty
            }

          case _ =>
            //val raw = showRaw( a ); if (!exprs.contains(raw) && raw.size < 700 && raw.size > "EmptyTree".size)println("computeExpr: "+treePosHolder.toString+"\n"+raw); exprs += raw
            //for (Ident(id) <- a) if (stringVals.exists(_.name == Some(id.toString))) {println("id: "+id+"  "+showRaw( a )); }
            //println("computeExpr: "+showRaw( a ))
            Values.empty
        }
        //println(out+"  "+showRaw(tree))
        out
      }
      //val exprs = mutable.HashSet[String]()

      var vals = Map.empty[String, Values] withDefaultValue Values.empty
      var vars = Set.empty[String]
      var stringVals = Set.empty[StringAttrs]
      var defModels = Map.empty[String, Either[Values, StringAttrs]] withDefaultValue Left(Values.empty)
      var labels = Map.empty[String, Tree]
      def discardVars(): Unit = {
        for (v <- vars) vals += v -> Values.empty
      }
      def discardVars(tree: Tree): Unit = {
        for (v <- vars; if isAssigned(tree, v)) {
          vals += v -> Values.empty
          stringVals = stringVals.filter(v => v.name.isDefined && !(vars contains v.name.get))
        }
      }
      def discardVars(tree: Tree, force: Set[String]): Unit = {
        for (v <- vars; if isAssigned(tree, v) || (force contains v)) {
          vals += v -> Values.empty
          stringVals = stringVals.filter(v => v.name.isDefined && !(vars contains v.name.get))
        }
      }
      def discardVars(force: Set[String]): Unit = {
        for (v <- vars; if (force contains v)) {
          vals += v -> Values.empty
          stringVals = stringVals.filter(v => v.name.isDefined && !(vars contains v.name.get))
        }
      }

      // vals, vars, stringVals, defModels
      val backupStack = mutable.Stack[(Map[String, Values], Set[String], Set[StringAttrs], Map[String, Either[Values, StringAttrs]])]()
      def pushDefinitions(): Unit = {
        val nonUnitResult = backupStack.push((vals, vars, stringVals, defModels))
      }
      def popDefinitions(): Unit = {
        // Discards new and discarded vars also
        val varsCurr = vars
        val (valsBack, varsBack, stringValsBack, defModelsBack) = backupStack.pop
        vals = valsBack
        vars = varsBack
        discardVars((varsCurr | varsBack) -- (varsCurr & varsBack))
        stringVals = stringValsBack
        defModels = defModelsBack
      }

      def forLoop(tree: Tree): Unit = {
        treePosHolder = tree
        //TODO: actually anything that takes (A <: (a Number) => _), this is awful
        val funcs = "foreach|map|filter(Not)?|exists|find|flatMap|forall|groupBy|count|((drop|take)While)|(min|max)By|partition|span"

        val (param, values, body, func, collection) = tree match {
          case Apply(TypeApply(Select(collection, func), _), List(Function(List(ValDef(_, param, _, _)), body))) if (func.toString matches funcs) =>
            //println(showRaw(collection))
            val values = computeExpr(collection).addName(param.toString)
            //println(values)
            //if (values.isEmpty) {
                //println("not a collection I know("+tree.pos+"): "+showRaw(collection))
                //return
            //}

            (param.toString, values, body, func.toString, collection)
          case _ =>
            //println("not a loop("+tree.pos+"): "+showRaw(tree))
            return
        }

        if (values.nonEmpty) {
          vals += param -> values.notSeq

          val exceptions =
            (func == "foreach" || param.contains("$") || collection.tpe.toString.startsWith("scala.collection.immutable.Range"))

          //TODO: verify filter = ...
          if (!isUsed(body, param, filter = "size|length|head|last") && !exceptions) warn(tree, UnusedForLoopIteratorValue)

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
          if (param.tpe.widen <:< IntClass.tpe && intParam.size >= 1) {
            if (intParam.isValue) {
              new StringAttrs(exactValue = Some(intParam.getValue.toString))
            } else {
              val maxLen = math.max(intParam.max.toString.length, intParam.min.toString.length)
              new StringAttrs(minLength = 1, trimmedMinLength = 1, maxLength = maxLen, trimmedMaxLength = maxLen)
            }
          } else if (param match { case Literal(Constant(_)) => true case _  => false }) param match { // groan.
            case Literal(Constant(null)) => new StringAttrs(exactValue = Some("null"))
            case Literal(Constant(a))    => new StringAttrs(Some(a.toString))
          } else if (param.tpe.widen <:< CharClass.tpe)  new StringAttrs(minLength = 1, trimmedMinLength = 0, maxLength = 1, trimmedMaxLength = 0)
          else if (param.tpe.widen <:< ByteClass.tpe)    new StringAttrs(minLength = 1, trimmedMinLength = 1, maxLength = 4, trimmedMaxLength = 4)
          else if (param.tpe.widen <:< ShortClass.tpe)   new StringAttrs(minLength = 1, trimmedMinLength = 1, maxLength = 6, trimmedMaxLength = 6)
          else if (param.tpe.widen <:< IntClass.tpe)     new StringAttrs(minLength = 1, trimmedMinLength = 1, maxLength = 11, trimmedMaxLength = 11)
          else if (param.tpe.widen <:< LongClass.tpe)    new StringAttrs(minLength = 1, trimmedMinLength = 1, maxLength = 20, trimmedMaxLength = 20)
          // http://stackoverflow.com/questions/1701055/what-is-the-maximum-length-in-chars-needed-to-represent-any-double-value :)
          else if (param.tpe.widen <:< DoubleClass.tpe)  new StringAttrs(minLength = 1, trimmedMinLength = 1, maxLength = 1079, trimmedMaxLength = 1079)
          else if (param.tpe.widen <:< FloatClass.tpe)   new StringAttrs(minLength = 1, trimmedMinLength = 1, maxLength = 154, trimmedMaxLength = 154)
          else if (param.tpe.widen <:< BooleanClass.tpe) new StringAttrs(minLength = 4, trimmedMinLength = 4, maxLength = 5, trimmedMaxLength = 5)
          else {
            if (!(param.tpe.widen <:< StringClass.tpe) && (param.tpe.baseClasses exists { _.tpe =:= TraversableClass.tpe })) {
              // Collections: minimal is Nil or Type()

              // Adapted from src/library/scala/collection/TraversableLike.scala
              val stringPrefix = {
                var string = param.tpe.toString
                val idx1 = string.lastIndexOf('.' : Int)
                if (idx1 != -1) string = string.substring(idx1 + 1)
                val idx2 = string.indexOf('$')
                if (idx2 != -1) string = string.substring(0, idx2)
                val idx3 = string.indexOf('[')
                if (idx3 != -1) string = string.substring(0, idx3)

                //Workaround: for Nil and for Seq which goes to List or ArrayBuffer
                if (string == "type" || string == "Seq") "" else string + "("
              }

              val minLen = stringPrefix.length+2
              new StringAttrs(
                minLength = minLen,
                trimmedMinLength = minLen,
                prefix = stringPrefix,
                suffix = ")",
                knownPieces = Set(stringPrefix, ")"))
            } else {
              //TODO: Discover moar
              //if (!(param.tpe.widen <:< StringClass.tpe) && !(param.tpe.widen <:< AnyClass.tpe))println(((str, param), (param.tpe, param.tpe.widen)))

              StringAttrs(param)
            }
          }
        }
        // scalastyle:on magic.number

        //// Tries to execute string functions and returns either a String or Int representation
        def stringFunc(string: Tree, funcName: Name, params: List[Tree] = List.empty[Tree]): Either[StringAttrs, Values] = {
          val str = StringAttrs(string)
          val func = funcName.toString
          val paramsSize = params.size
          lazy val intParam = if (paramsSize == 1 && params.head.tpe.widen <:< IntClass.tpe) computeExpr(params.head) else Values.empty
          lazy val intParams = if (params.forall(_.tpe.widen <:< IntClass.tpe)) params.map(computeExpr) else Nil //option?
          lazy val stringParam: StringAttrs = if (paramsSize == 1 && params.head.tpe.widen <:< StringClass.tpe) StringAttrs(params.head) else empty
          lazy val stringParams: Seq[StringAttrs] = if (params.forall(_.tpe.widen <:< StringClass.tpe)) params.map(StringAttrs.apply) else Nil

          //println((string, func, params, str, intParam))
          //println(str.exactValue)
          //if (!(string.tpe.widen <:< StringClass.tpe))
          //if ((string.tpe.widen <:< StringClass.tpe))println((string, func, params, str, intParam))

          // We can get some information, even if the string is unknown
          //if (str == StringAttrs.empty) {
          //  Left(empty)
          //} else
          func match {
            case "size"|"length" if paramsSize == 0 =>
              Right(
                str.exactValue
                  .map(v => Values(v.size))
                  .getOrElse(
                    if (str.getMinLength == str.getMaxLength)
                      Values(str.getMinLength)
                    else if (str.getMinLength == 0 && str.getMaxLength == Int.MaxValue)
                      Values.empty
                    else
                      Values(str.getMinLength, str.getMaxLength)
                  ).addCondition(_ >= 0).addCondition(_ < str.getMaxLength)
              )
            case "toString" if paramsSize == 0 =>
              Left(toStringAttrs(string))
            case "$plus"|"concat" if paramsSize == 1 =>
              //println(str.toString +" + "+toStringAttrs(params.head).toString + " == "+ (str + toStringAttrs(params.head)).toString)
              Left(str + toStringAttrs(params.head))
            case "$times" =>
              Left(str * intParam)

            case "intern" =>
              Left(str)

            case "head"|"last" =>
              if (str.maxLength == 0)
                warn(treePosHolder, DecomposingEmptyCollection(func, "string"))

              Left(empty)

            case "init"|"tail" =>
              if (str.exactValue.isDefined) {
                if (str.exactValue.get.isEmpty) {
                  warn(treePosHolder, DecomposingEmptyCollection(func, "string"))
                  Left(empty)
                } else {
                  Left(new StringAttrs(str.exactValue.map(a => if (func == "init") a.init else a.tail)))
                }
              } else {
                if (str.maxLength == 0) {
                  warn(treePosHolder, DecomposingEmptyCollection(func, "string"))
                }

                Left(new StringAttrs(
                  minLength = math.max(str.minLength-1, 0),
                  maxLength = if (str.maxLength != Int.MaxValue) math.max(str.maxLength-1, 0) else Int.MaxValue))
              }

            case "capitalize" if paramsSize == 0 => Left(str.capitalize)
            case "distinct"   if paramsSize == 0 => Left(str.distinct)
            case "reverse"    if paramsSize == 0 => Left(str.reverse)
            case "count"      if paramsSize == 1 =>
              val out = Values.empty.addCondition(_ >= 0)
              Right(if (str.getMaxLength != Int.MaxValue) out.addCondition(_ < str.getMaxLength) else out)
            case "filter"|"filterNot" if paramsSize == 1 =>
              Left(str.removeExactValue.zeroMinLengths)

            case "indexOf"|"lastIndexOf" if paramsSize == 1 =>
              if (str.exactValue.isDefined && stringParam.exactValue.isDefined) {
                if (func == "indexOf")
                  Right(Values(str.exactValue.get.indexOf(stringParam.exactValue.get)))
                else
                  Right(Values(str.exactValue.get.lastIndexOf(stringParam.exactValue.get)))
              } else if (str.getMaxLength < Int.MaxValue) {
                Right(Values.empty.addConditions(_ >= -1, _ < str.getMaxLength))
              } else {
                Right(Values.empty.addConditions(_ >= -1))
              }

            // These also come in (Char/String) versions
            case "stringPrefix" if paramsSize == 0 && str.exactValue.isDefined => Left(new StringAttrs(str.exactValue.map(_.stringPrefix)))
            case "stripLineEnd" if paramsSize == 0 && str.exactValue.isDefined => Left(new StringAttrs(str.exactValue.map(_.stripLineEnd)))
            case "stripMargin"  if paramsSize == 0 && str.exactValue.isDefined => Left(new StringAttrs(str.exactValue.map(_.stripMargin)))

            //
            case "mkString" if paramsSize == 0 =>
                warn(treePosHolder, UnnecessaryMethodCall("mkString"))

                Left(str)
            case "mkString" if paramsSize == 1 && str.exactValue.isDefined && stringParam.exactValue.isDefined =>
                val p0 = stringParam.exactValue.get

                Left(new StringAttrs(Some(str.exactValue.get.mkString(p0))))
            //
            case "toUpperCase" => Left(str.toUpperCase)
            case "toLowerCase" => Left(str.toLowerCase)
            case "trim" =>  Left(str.trim)
            case "nonEmpty"|"isEmpty" =>
              if (str.alwaysNonEmpty) warn(treePosHolder, UnnecessaryStringNonEmpty)
              if (str.alwaysIsEmpty) warn(treePosHolder, UnnecessaryStringIsEmpty)
              Left(empty)
            case "hashCode" if str.exactValue.isDefined =>
              Right(Values(str.exactValue.get.hashCode))
            /// String.to{Int, Long, Float, Double} conversion will likely fail (runtime exception)
            case "toInt" if str.exactValue.isDefined =>
              try {
                Right(Values(str.exactValue.get.toInt))
              } catch {
                case _: NumberFormatException =>
                  warn(treePosHolder, InvalidStringConversion(func))
                  Left(empty)
              }
            case "toLong" if str.exactValue.isDefined =>
              try {
                str.exactValue.get.toLong
              } catch {
                case _: NumberFormatException =>
                  warn(treePosHolder, InvalidStringConversion(func))
              }
              Left(empty)
            case "toDouble"|"toFloat" if str.exactValue.isDefined =>
              try {
                str.exactValue.get.toDouble
              } catch {
                case _: NumberFormatException =>
                  warn(treePosHolder, InvalidStringConversion(func))
              }
              Left(empty)

            // str.func(String)
            case "charAt"|"codePointAt"|"codePointBefore"|"substring"
                |"apply"|"drop"|"take"|"dropRight"|"takeRight" if intParam.isValue =>
              val param = intParam.getValue
              lazy val string = str.exactValue.get //lazy to avoid None.get... didn't use monadic, because I was lazy

              //println((string, param))

              //TODO: use reflection maybe?
              //TODO: could do some prefix/suffix enhancements
              try func match {
                case "charAt"|"apply" =>
                  if (str.exactValue.isDefined) { string.charAt(param); Left(empty) } else if (param < 0) throw new IndexOutOfBoundsException else Left(empty)
                case "codePointAt" =>
                  if (str.exactValue.isDefined) Right(Values(string.codePointAt(param))) else if (param < 0) throw new IndexOutOfBoundsException else Left(empty)
                case "codePointBefore" =>
                  if (str.exactValue.isDefined) Right(Values(string.codePointBefore(param))) else if (param < 1) throw new IndexOutOfBoundsException else Left(empty)
                case "substring" =>
                  if (str.exactValue.isDefined) Left(new StringAttrs(Some(string.substring(param)))) else if (param < 0) throw new IndexOutOfBoundsException else Left(empty)
                case "drop" =>
                  if (str.exactValue.isDefined)
                    Left(new StringAttrs(Some(string.drop(param))))
                  else
                    Left(new StringAttrs(minLength = math.max(str.minLength-param, 0), maxLength = if (str.maxLength != Int.MaxValue) math.max(str.maxLength-param, 0) else Int.MaxValue))
                case "take" =>
                  if (str.exactValue.isDefined)
                    Left(new StringAttrs(Some(string.take(param))))
                  else
                    Left(new StringAttrs(minLength = math.min(param, str.minLength), maxLength = math.max(param, 0)))
                case "dropRight" =>
                  if (str.exactValue.isDefined)
                    Left(new StringAttrs(Some(string.dropRight(param))))
                  else
                    Left(new StringAttrs(minLength = math.max(str.minLength-param, 0), maxLength = if (str.maxLength != Int.MaxValue) math.max(str.maxLength-param, 0) else Int.MaxValue))
                case "takeRight" =>
                  if (str.exactValue.isDefined)
                    Left(new StringAttrs(Some(string.takeRight(param))))
                  else
                    Left(new StringAttrs(minLength = math.min(param, str.minLength), maxLength = math.max(param, 0)))
                case _ =>
                  Left(empty)
              } catch {
                case _: IndexOutOfBoundsException =>
                  /// String index will likely cause an IndexOutOfBoundsException (runtime exception)
                  warn(params.head, LikelyIndexOutOfBounds("out of bounds"))
                  Left(empty)
                case _: Exception =>
                  Left(empty)
              }

            //str.func(Int, Int)
            case "substring"|"codePointCount" if intParams.size == 2 && intParams.forall(_.isValue) =>
              lazy val string = str.exactValue.get //lazy to avoid None.get... didn't use monadic, because I was lazy
              val param: Seq[Int] = intParams.map(_.getValue)

              try func match {
                case "substring" =>
                  if (str.exactValue.isDefined)
                    Left(new StringAttrs(Some(string.substring(param(0), param(1)))))
                  else if (param(0) < 0 || param(1) < param(0) || param(0) >= str.getMaxLength || param(1) >= str.getMaxLength)
                    throw new IndexOutOfBoundsException
                  else if (param(0) == param(1))
                    Left(new StringAttrs(Some("")))
                  else
                    Left(empty)
                case "codePointCount" =>
                  if (str.exactValue.isDefined)
                    Right(Values(string.codePointCount(param(0), param(1))))
                  else if (param(0) < 0 || param(1) < param(0) || param(0) >= str.getMaxLength || param(1) >= str.getMaxLength)
                    throw new IndexOutOfBoundsException
                  else
                    Left(empty)
                case _ =>
                  Left(empty)
              } catch {
                case _: IndexOutOfBoundsException =>
                  warn(params.head, LikelyIndexOutOfBounds("out of bounds"))
                  Left(empty)
                case _: Exception =>
                  Left(empty)
              }

            // str.func(String)
            /// Try to verify String contains, startsWith, endsWith
            case "contains"|"startsWith"|"endsWith"|"equals"|"$eq$eq"|"$bang$eq"|"matches" =>
              for (result <- (func match {
                case "contains"        => (str contains stringParam)
                case "startsWith"      => (str startsWith stringParam)
                case "endsWith"        => (str endsWith stringParam)
                case "equals"|"$eq$eq" => (str equals stringParam)
                case "$bang$eq"        => (str nequals stringParam)
                case "matches"         => (str matches stringParam)
                case _ => None
              })) {
                val function = if (func == "$eq$eq") "equals" else if (func == "$bang$eq") "not equals" else func
                warn(params.head, InvariantReturn(function, result.toString))
              }

              Left(empty)

            //str.func(String, String)
            case "replace" if stringParams.size == 2 && stringParams.forall(_.exactValue.isDefined) =>
              val (p0, p1) = (stringParams(0).exactValue.get, stringParams(1).exactValue.get)

              if (p0 == p1) {
                warn(treePosHolder, UnnecessaryMethodCall("replace"))

                Left(str)
              } else if (str.exactValue.isDefined) {
                Left(new StringAttrs(Some(str.exactValue.get.replace(p0, p1))))
              } else {
                Left(empty)
              }

            case "replaceAll"|"replaceFirst" if stringParams.size == 2 && stringParams.forall(_.exactValue.isDefined) =>
              val (p0, p1) = (stringParams(0).exactValue.get, stringParams(1).exactValue.get)

              //TODO: nopenopenopenope - matches the end of replaceX func, because this gets traversed multiple times with the wrong pos
              val posFiltering = treePosHolder.toString matches (".*?[ .]"+ func + """ *[(].*, *("{2}|"{6}) *[)]""")
              if (posFiltering && p1.isEmpty) {
                if (p0.matches(PlainStringRegex+"\\$"))
                  warn(treePosHolder, RegexWarning(s"""This $func("${p0}", "") can be substituted with stripSuffix("${cleanPlainRegex(p0.dropRight(1))}")""", error = false))
                if (p0.matches("[\\^]"+PlainStringRegex))
                  warn(treePosHolder, RegexWarning(s"""This $func("${p0}", "") can be substituted with stripPrefix("${cleanPlainRegex(p0.drop(1))}")""", error = false))
              }
              if (func == "replaceAll" && p0.matches(PlainStringRegex) && !p1.contains("$"))
                warn(treePosHolder, RegexWarning(s"""This $func("${p0}", "${p1}") can likely be substituted with replace("${cleanPlainRegex(p0)}", "${p1}")""", error = false))

              if (str.exactValue.isDefined) {
                try {
                  Left(new StringAttrs(Some(if (func == "replaceAll") str.exactValue.get.replaceAll(p0, p1) else str.exactValue.get.replaceFirst(p0, p1))))
                } catch {
                  case e: java.util.regex.PatternSyntaxException =>
                    Left(empty)
                }
              } else if ((p0 matches """\[[^\]]+\]""") && p1.size == 1) { //keeps the same length
                Left(str.justLengths)
              } else if (p1 == "") { //length is 0..size
                Left(str.justLengths.zeroMinLengths)
              } else {
                Left(empty)
              }
            /// String format checks (runtime exception)
            case "format" if (params.nonEmpty) && !(params.head.tpe.widen <:< rootMirror.getClassByName(newTermName("java.util.Locale")).tpe) =>
              //TODO: see http://docs.oracle.com/javase/7/docs/api/java/util/Formatter.html
              // in some cases at least length could be inferred, but option parser would need to be implemented

              val parActual = params map {
                case param if (param.tpe.widen <:< IntClass.tpe) => computeExpr(param)
                case param if (param.tpe.widen <:< StringClass.tpe) => StringAttrs(param)
                case Literal(Constant(x)) => x
                case _ => StringAttrs.empty
              }

              val valuesDefined = parActual forall {
                case v: Values => v.isValue
                case s: StringAttrs => s.exactValue.isDefined // Nonliterals are StringAttrs.empty, and go to false here
                case _ => true // Literals are defined
              }

              val prefixReg = "([^%]*).*".r
              val suffixReg = ".*?[^%]( [^%]*)".r
              def getPrefix(s: String): String = try { val prefixReg(out) = s; out } catch { case _: MatchError => "" }
              def getSuffix(s: String): String = try { val suffixReg(out) = s; out } catch { case _: MatchError => "" }

              var outStr = Left(empty)
              if (str.exactValue.isDefined) {
                val strValue = str.exactValue.get
                val percentCnt = strValue.count(_ == '%')
                if (percentCnt == 0) {
                  warn(string, InvalidStringFormat("There are no percent signs in the format string.", exception = false))
                } else if (parActual.length > percentCnt) {
                  warn(string, InvalidStringFormat("There are more parameters being passed to format, than there are percent signs in the format string.", exception = false))
                }
                if (valuesDefined) {
                  try {
                    outStr = Left(new StringAttrs(exactValue = Some(strValue.format(parActual map {
                      case v: Values => v.getValue
                      case s: StringAttrs => s.exactValue.get
                      case x => x
                    }:_*))))
                  } catch {
                    case e: java.util.UnknownFormatConversionException =>
                      warn(string, InvalidStringFormat(e.toString))
                    case e: java.util.IllegalFormatConversionException if !e.getMessage.contains("!= java.lang.String") => //TODO: Why this condition?
                      warn(string, InvalidStringFormat(e.toString))
                    case e: java.util.MissingFormatArgumentException =>
                      warn(string, InvalidStringFormat(e.toString))
                    case e: java.util.DuplicateFormatFlagsException =>
                      warn(string, InvalidStringFormat(e.toString))
                    case _: Exception =>
                  }
                } else {
                  try {
                    strValue.format(parActual.map(_ => null):_*)
                    val prefix = getPrefix(strValue)
                    val suffix = getSuffix(strValue)
                    outStr = Left(new StringAttrs(
                      minLength = math.max(prefix.length, suffix.length),
                      trimmedMinLength = math.max(prefix.trim.length, suffix.trim.length),
                      prefix = prefix,
                      suffix = suffix,
                      knownPieces = Set(prefix, suffix)))
                  } catch {
                    case e: java.util.UnknownFormatConversionException =>
                      warn(string, InvalidStringFormat(e.toString))
                    case e: java.util.MissingFormatArgumentException =>
                      warn(string, InvalidStringFormat(e.toString))
                    case e: java.util.DuplicateFormatFlagsException =>
                      warn(string, InvalidStringFormat(e.toString))
                    case _: Exception =>
                  }
                }
              } else if (str.prefix != "" || str.suffix != "") {
                val prefix = str.prefix
                val suffix = str.suffix
                outStr = Left(new StringAttrs(
                  minLength = math.max(prefix.length, suffix.length),
                  trimmedMinLength = math.max(prefix.trim.length, suffix.trim.length),
                  prefix = prefix,
                  suffix = suffix,
                  knownPieces = Set(prefix, suffix)))
              }

              outStr
            case _ =>
              //if (str.exactValue.isDefined)println((str, func, params))
              Left(empty)
          }
        }


        // avoids an undebugged infinite loop
        val cache = mutable.Map[Tree, StringAttrs]()

        //// Try to convert an AST into a String value
        def apply(tree: Tree): StringAttrs = {
          def traverseString(tree: Tree): StringAttrs = tree match {
            case Literal(Constant(null)) => new StringAttrs(exactValue = Some("null"))
            case Literal(Constant(c)) =>
              //if (stringVals.filter(s => s.name.isDefined && !(vars contains s.name.get)).exists(_.exactValue == Some(c.toString))) {
                //warn(tree, "You have defined that string as a val already, maybe use that?")
              //}

              new StringAttrs(exactValue = Some(c.toString))

            case Ident(name) =>
              stringVals
                .find(_.name.exists(_ == name.toString))
                .getOrElse(empty)

            // String interpolator
            case Apply(Select(Apply(scala_StringContext_apply, literalList), interpolator), paramList)
              if (scala_StringContext_apply.toString == "scala.StringContext.apply")
              && (interpolator.toString == "s") =>

              try {
                if (paramList.isEmpty) {
                  warn(tree, EmptyStringInterpolator)
                  apply(literalList.head)
                } else {
                  var out = empty
                  var i = 0
                  do {
                    out +=
                      (if (i%2 == 0 && i/2 < literalList.size) apply(literalList(i/2))
                      else if (i/2 < literalList.size) apply(paramList(i/2))
                      else new StringAttrs(exactValue = Some("")))

                    i += 1
                  } while (i < literalList.length + paramList.length)
                  out
                }
              } catch {
                case _: Exception =>
                  empty
              }

            case Apply(Ident(name), _params) if defModels contains name.toString =>
              defModels
                .find(m => m._1 == name.toString && m._2.isRight).fold(empty)(_._2.right.get)

            case If(_cond, expr1, expr2) =>
              val (e1, e2) = (traverseString(expr1), traverseString(expr2))

              if (expr1.tpe <:< NothingClass.tpe) {
                e2
              } else if (expr2.tpe <:< NothingClass.tpe) {
                e1
              } else {
                new StringAttrs(
                  minLength = math.min(e1.getMinLength, e2.getMinLength),
                  trimmedMinLength = math.min(e1.getTrimmedMinLength, e2.getTrimmedMinLength),
                  maxLength = math.max(e1.getMaxLength, e2.getMaxLength),
                  trimmedMaxLength = math.max(e1.getTrimmedMaxLength, e2.getTrimmedMaxLength))
              }

            case Apply(augmentString, List(expr)) if (augmentString.toString == staticSelect("scala", "Predef.augmentString")) =>
              StringAttrs(expr)

            // Implicit toString
            case Apply(Select(expr1, nme.ADD), List(expr2)) if (expr1.tpe.widen <:< StringClass.tpe ^ expr2.tpe.widen <:< StringClass.tpe) =>
              toStringAttrs(expr1) + toStringAttrs(expr2)

            // Pass on functions on strings
            //TODO: maybe check if some return string and can be computed
            case Apply(Select(str, func), params) =>
              cache.getOrElseUpdate(tree, stringFunc(str, func, params).left.getOrElse(empty))

            case Select(Apply(scala_augmentString, List(string)), func) if (scala_augmentString.toString endsWith "augmentString") =>
              stringFunc(string, func).left.getOrElse(empty)

            case _ =>
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
          val name: Option[String] = None, // Move to outer map?
          private val minLength: Int = 0,
          private val trimmedMinLength: Int = 0,
          private val maxLength: Int = Int.MaxValue,
          private val trimmedMaxLength: Int = Int.MaxValue,
          private var prefix: String = "",
          private var suffix: String = "",
          private var knownPieces: Set[String] = Set.empty[String]) {

        // Keep this in mind, make getters
        if (exactValue.isDefined) {
          prefix = exactValue.get
          suffix = exactValue.get
          knownPieces = Set.empty[String]
        }
        //println(this)

        def equals(s: StringAttrs): Option[Boolean] =
          if (s.getMinLength > this.getMaxLength || this.getMinLength > s.getMaxLength) Some(false)
          else if (s.exactValue.isDefined) this.equals(s.exactValue.get)
          else if (this.exactValue.isDefined) s.equals(this.exactValue.get)
          else None
        def equals(s: String): Option[Boolean] = {
          if (exactValue.isDefined) {
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
          if (s.getMinLength > getMaxLength) Some(false)
          else s.exactValue.flatMap(this.contains)
        def contains(s: String): Option[Boolean] =
          if (exactValue.isDefined) Some(exactValue.get contains s)
          else if ((prefix contains s) || (suffix contains s) || (knownPieces exists { _ contains s })) Some(true)
          else if (s.length > getMaxLength) Some(false)
          else None

        def startsWith(s: StringAttrs): Option[Boolean] =
          if (s.getMinLength > getMaxLength) Some(false)
          else s.exactValue.flatMap(this.startsWith)
        def startsWith(s: String): Option[Boolean] =
          if (s.length > getMaxLength) Some(false)
          else if (prefix.isEmpty || s.length > prefix.length) None
          else Some(prefix startsWith s)

        def endsWith(s: StringAttrs): Option[Boolean] =
          if (s.getMinLength > getMaxLength) Some(false)
          else s.exactValue.flatMap(this.endsWith)
        def endsWith(s: String): Option[Boolean] =
          if (s.length > getMaxLength) Some(false)
          else if (suffix.isEmpty || s.length > suffix.length) None
          else Some(suffix endsWith s)

        def matches(s: StringAttrs): Option[Boolean] = {
          if ((s startsWith "^") == Some(true) || ((s endsWith "$") == Some(true) && (s.suffix.length == 1 || (s endsWith "\\$") == Some(false))))
            warn(treePosHolder, SuspiciousMatches("This regex starts with ^ or ends with $. The matches method always matches the entire string."))

          s.exactValue.flatMap(this.matches)
        }
        def matches(s: String): Option[Boolean] = {
          if ((s startsWith "^") || ((s endsWith "$") && !(s endsWith "\\$")))
            warn(treePosHolder, SuspiciousMatches("This regex starts with ^ or ends with $. The matches method always matches the entire string."))

          if (s.matches("[.][*]"+PlainStringRegex+"[.][*]"))
            warn(treePosHolder, SuspiciousMatches(s"""This matches("$s") could likely be replaced by contains("${cleanPlainRegex(s.drop(2).dropRight(2))}")"""))
          if (s.matches("[\\^]?"+PlainStringRegex+"[.][*]"))
            warn(treePosHolder, SuspiciousMatches(s"""This matches("$s") could likely be replaced by startsWith("${cleanPlainRegex(s.stripPrefix("^").dropRight(2))}")"""))
          if (s.matches("[.][*]"+PlainStringRegex+"[$]?"))
            warn(treePosHolder, SuspiciousMatches(s"""This matches("$s") could likely be replaced by endsWith("${cleanPlainRegex(s.drop(2).stripSuffix("$"))}")"""))
          if (s.matches("[\\^]?"+PlainStringRegex+"[$]?"))
            warn(treePosHolder, SuspiciousMatches(s"""This matches("$s") could likely be replaced by equals("${cleanPlainRegex(s.stripPrefix("^").stripSuffix("$"))}")"""))

          exactValue.map(_ matches s)
        }

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
          val newMinLength = exactValue.fold(getTrimmedMinLength)(_.trim.size)
          val newMaxLength = exactValue.fold(getTrimmedMaxLength)(_.trim.size)
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
        def justLengths: StringAttrs = new StringAttrs(None, name, getMinLength, getTrimmedMinLength, getMaxLength, trimmedMaxLength, "", "", Set.empty)

        def alwaysIsEmpty: Boolean = getMaxLength == 0
        def alwaysNonEmpty: Boolean = getMinLength > 0

        def getMinLength: Int = exactValue.fold(minLength)(_.size)
        def getMaxLength: Int = exactValue.fold(maxLength)(_.size)
        def getTrimmedMinLength: Int = exactValue.fold(trimmedMinLength)(_.trim.size)
        def getTrimmedMaxLength: Int = exactValue.fold(trimmedMaxLength)(_.trim.size)

        def +(s: String): StringAttrs =
          new StringAttrs(
            exactValue = if (this.exactValue.isDefined) Some(this.exactValue.get + s) else None,
            minLength = this.getMinLength + s.length,
            trimmedMinLength = this.getTrimmedMinLength + s.trim.length, //TODO: can be made more exact
            maxLength = if (this.maxLength == Int.MaxValue) Int.MaxValue else this.getMaxLength + s.length,
            trimmedMaxLength =
              if (this.getTrimmedMaxLength == Int.MaxValue) Int.MaxValue
              //else if (this.exactValue.isDefined) (this.exactValue.get + s).trim.size // This case is covered in getTrimmedMaxLength if exactValue is known
              else this.getMaxLength + s.length,
            prefix = if (this.exactValue.isDefined) this.exactValue.get + s else this.prefix,
            suffix = this.suffix + s,
            knownPieces = this.knownPieces)

        def +(s: StringAttrs): StringAttrs =
          new StringAttrs(
            exactValue = if (this.exactValue.isDefined && s.exactValue.isDefined) Some(this.exactValue.get + s.exactValue.get) else None,
            minLength = this.getMinLength + s.getMinLength,
            trimmedMinLength = this.getTrimmedMinLength + s.getTrimmedMinLength, //TODO: can be made more exact
            maxLength = if (this.getMaxLength == Int.MaxValue || s.getMaxLength == Int.MaxValue) Int.MaxValue else this.getMaxLength + s.getMaxLength,
            trimmedMaxLength =
              if (this.getTrimmedMaxLength == Int.MaxValue || s.getTrimmedMaxLength == Int.MaxValue) Int.MaxValue
              //else if (this.isDefined && s.isDefined) (this.exactValue.get + s.exactValue.get).trim.size // This case is covered in getTrimmedMaxLength if exactValue is known
              else this.getMaxLength + s.getMaxLength,
            prefix = if (this.exactValue.isDefined) this.exactValue.get + s.prefix else this.prefix,
            suffix = if (s.exactValue.isDefined) this.suffix + s.suffix else s.suffix,
            knownPieces = this.knownPieces ++ s.knownPieces + (this.suffix + s.prefix))

        /// String multiplication with value <= 0 warning
        def *(n: Values): StringAttrs = {
          if (n.isValue) {
            this * n.getValue
          } else if (n.nonEmpty) {
            if (n forallLower 2) {
              if (n.max == 1) {
                new StringAttrs(
                  minLength = 0,
                  trimmedMinLength = 0,
                  maxLength = this.getMaxLength,
                  trimmedMaxLength = this.getTrimmedMaxLength)
              } else {
                warn(treePosHolder, StringMultiplicationByNonPositive)
                new StringAttrs(exactValue = Some(""))
              }
            } else {
              new StringAttrs(
                minLength = if (n.min > 0) this.getMinLength*n.min else 0,
                trimmedMinLength = if (n.min > 0) this.getTrimmedMinLength*n.min else 0)
            }
          } else {
            StringAttrs.empty
          }
        }
        def *(n: Int): StringAttrs =
          if (n <= 0) {
            warn(treePosHolder, StringMultiplicationByNonPositive)
            new StringAttrs(Some(""))
          } else {
            new StringAttrs(
              exactValue = if (this.exactValue.isDefined) Some(this.exactValue.get*n) else None,
              minLength = this.getMinLength*n,
              trimmedMinLength = this.getTrimmedMinLength*n, //ADD: can be made more exact
              maxLength = if (this.getMaxLength == Int.MaxValue) Int.MaxValue else this.getMaxLength*n,
              trimmedMaxLength = if (this.getTrimmedMaxLength == Int.MaxValue) Int.MaxValue else this.getTrimmedMaxLength*n,
              prefix = this.prefix,
              suffix = this.suffix,
              knownPieces = this.knownPieces ++ (if (n >= 2) Set(this.suffix+this.prefix) else Nil))
          }

        //TODO: wait what... I hope this isn't needed anywhere :)
        override def hashCode: Int = exactValue.hashCode + name.hashCode + minLength + trimmedMinLength + maxLength + trimmedMaxLength
        override def equals(that: Any): Boolean = that match {
          case that: StringAttrs => (this.exactValue.isDefined && that.exactValue.isDefined && this.exactValue.get == that.exactValue.get)
          case that: String => this.exactValue.exists(_ == that)
          case _ => false
        }

        override def toString: String =
         "StringAttrs" + (if (exactValue.isDefined) "("+exactValue.get+")" else (name, getMinLength, getTrimmedMinLength, getMaxLength, getTrimmedMaxLength, prefix, suffix, knownPieces))
      }

      def traverseBlock(tree: Tree): Unit = {
        pushDefinitions()
        traverse(tree)
        popDefinitions()
      }

      var superTraverse = true
      def catcher(): PartialFunction[Throwable, Unit] = {
        case _: NullPointerException => //Ignore
        case _: NoSuchMethodError => //Ignore
        case _: StackOverflowError => superTraverse = false
        case e: Exception => //e.printStackTrace//TODO: Print details and ask user to report it
      }
      def finalizer(tree: Tree): Unit = {
        if (superTraverse) try { super.traverse(tree) } catch catcher else doNotTraverse += tree
      }

      override def traverse(tree: Tree): Unit = try {
        superTraverse = true
        if (doNotTraverse contains tree) { superTraverse = false; return }
        treePosHolder = tree
        tree match {
          /// Very hacky support for some var interpretion
          /*case ValDef(m: Modifiers, varName, _, value) if (m.hasFlag(MUTABLE)) =>
            vars += varName.toString
            vals(varName.toString) = computeExpr(value)
            //println("assign: "+(vals))*/
          case Assign(varName, value) if vars contains varName.toString =>
            vals += varName.toString -> computeExpr(value)
            //println("reassign: "+(vals))

          case LabelDef(label, Nil, If(_, _, _)) if {
            discardVars(tree, (vars & getUsed(tree)))
            labels += label.toString -> tree
            false
          } => //Fallthrough
          case Apply(label, Nil) if (labels contains label.toString) && {
            //TODO: check if there is an infinite...
            // vals.filter(a => vars contains a._1).map(a => println(a._1, a._2.applyCond(labels(label.toString))._2))

            discardVars(labels(label.toString))//, (vars & getUsed(labels(label.toString))).toSeq:_*)

            labels -= label.toString
            false
          } => //Fallthrough
          case e if {
            // Throw away assigns inside other blocks
            discardVars(e)
            false
          } => //Fallthrough

          case forloop @ Apply(TypeApply(Select(_collection, _), _), List(Function(List(ValDef(_, _, _, _)), _))) =>
            forLoop(forloop)

          /// Assertions checks (assert, assume, require)
          case Apply(Select(scala_Predef, Name("assert"|"assume"|"require")), List(condExpr))
            if (scala_Predef.tpe.widen <:< PredefModule.tpe) =>

            // We can apply these conditions to vals - if they don't hold, it'll throw an exception anyway
            // and they'll reset at the end of the current block
            vals = vals.map(a => (a._1, a._2.applyCond(condExpr)._1)).withDefaultValue(Values.empty)

          /// String checks
          //case s @ Literal(Constant(str: String)) if stringVals.filter(s => s.name.isDefined && !(vars contains s.name.get)).find(_.exactValue == Some(str)).isDefined =>
            //warn(s, "You have defined that string as a val already, maybe use that?")

          case ValDef(m: Modifiers, valName, _, str @ Literal(Constant(_: String))) if (!m.isMutable && !m.isFinal && !m.hasDefault) =>
            //if (stringVals.filter(s => s.name.isDefined && !(vars contains s.name.get)).exists(_.exactValue == Some(str)))
              //warn(s, "You have defined that string as a val already, maybe use that?")
            //stringVals += str

            val str2 = StringAttrs(str).addName(valName.toString)
            //println("str2: "+str2)
            if (str2.exactValue.isDefined || str2.getMinLength > 0) {
              stringVals += str2
            }
            //println("stringVals2: "+stringVals)

          case ValDef(m: Modifiers, valName, _, Literal(Constant(a: Int))) if (!m.isMutable && !m.hasDefault) =>
            val valNameStr = valName.toString.trim
            vals += valNameStr -> Values(a, valNameStr)
            //println(vals(valName.toString))

          case ValDef(m: Modifiers, valName, _, expr) if (!m.hasDefault) =>
            //if !m.hasFlag(MUTABLE) /*&& !m.hasFlag(LAZY)) && !computeExpr(expr).isEmpty*/ => //&& computeExpr(expr).isValue =>
            //ADD: aliasing... val a = i, where i is an iterator, then 1/i-a is divbyzero
            //ADD: isSeq and actualSize

            if (expr.tpe.widen <:< StringClass.tpe) {
              val str = StringAttrs(expr).addName(valName.toString) //StringAttrs.toStringAttrs(expr)
              //println("str1: "+str)
              if (str.exactValue.isDefined || str.getMinLength > 0) {
                //println(str)
                stringVals += str
              }
              //println("stringVals1: "+stringVals)
            }

            // private[this] var k = 4 messes up a few things
            if (!(m.isPrivateLocal && m.isMutable)) {
              val valNameStr = valName.toString
              val res = computeExpr(expr).addName(valNameStr)
              vals += valNameStr -> res
              if (m.isMutable) vars += valNameStr
            }


            //println("newVal: "+computeExpr(expr).addName(valNameStr))
            //println("newVal: "+vals(valName.toString))
            //println(showRaw(expr))

            expr match {
              case e => //Block(_, _) | If(_, _, _) =>
                //println(expr)
                pushDefinitions()

                traverse(expr)

                popDefinitions()
              //case _ =>
            }

          case Match(pat, cases) if pat.tpe.toString != "Any @unchecked" && cases.size >= 2 =>
            for (c <- cases) {
              pushDefinitions()

              //TODO: c.pat can override some variables
              traverse(c.body)

              popDefinitions()
            }

          case If(condExpr, t, f) => //TODO: moved to computeExpr?
            val backupVals = vals
            val backupStrs = stringVals

            pushDefinitions()
            super.traverse(condExpr)
            popDefinitions()

            pushDefinitions()

            //println(vals)
            vals = backupVals.map(a => (a._1, a._2.applyCond(condExpr)._1)).withDefaultValue(Values.empty)
            //println(vals)
            //ADD: if always true, or always false pass the return value, e.g. val a = 1; val b = if (a == 1) 5 else 4
            try { super.traverse(t) } catch catcher

            popDefinitions()
            pushDefinitions()

            vals = backupVals.map(a => (a._1, a._2.applyCond(condExpr)._2)).withDefaultValue(Values.empty)
            try { super.traverse(f) } catch catcher

            popDefinitions()
            //println(vals)

          /// Attempt to verify collection indices are correct
          //case pos @ Apply(Select(Ident(seq), apply), List(indexExpr))
          case pos @ Apply(Select(seq, _apply), List(indexExpr)) if methodImplements(pos.symbol, SeqLikeApply) =>
            //println(seq.toString)
            //println("indexExpr: "+computeExpr(indexExpr))
            //println(showRaw(indexExpr))
            if (vals.contains(seq.toString) && vals(seq.toString).actualSize != -1 && (computeExpr(indexExpr).existsGreater(vals(seq.toString).actualSize-1))) {
              warn(pos, LikelyIndexOutOfBounds("too large"))
            }
            if (computeExpr(indexExpr).existsLower(0)) {
              warn(pos, LikelyIndexOutOfBounds("negative"))
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
            try { super.traverse(block) } catch catcher

            val returnVal = last match {
              case Return(ret) =>
                def otherReturns: Boolean = {
                  for (Return(ret) <- b) return true
                  false
                }
                /// Unnecessary use of return keyword
                if (otherReturns) warn(last, UnnecessaryReturn)
                ret
              case a =>
                a
            }

            //defModels = backupDefModels

            /// Method always returns the same value
            if (returnCount(block) == 0 && throwsCount(block) == 0) {//ADD: can be made better - if sentences are easy to model
              val retVal = computeExpr(returnVal)
              if (retVal.isValue || (retVal.isSeq && retVal.size > 0)) {
                warn(last, InvariantReturn("method", retVal.getValue.toString))
              }
              popDefinitions()
              if (retVal.nonEmpty || retVal.conditions.nonEmpty || (retVal.isSeq && retVal.actualSize != -1)) {
                //println("ModeledV: "+name.toString+" "+retVal)
                defModels += name.toString -> Left(retVal)
              } else {
                val retVal = StringAttrs(returnVal)
                if ((retVal.getMinLength > 0 || retVal.getMaxLength < Int.MaxValue) && !(name.toString matches "[<$]init[$>]")) {
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

          case Apply(Select(str, Name(func @ ("matches"|"split"))), List(regExpr)) if (str.tpe.widen <:< StringClass.tpe) =>
            treePosHolder = regExpr
            StringAttrs(regExpr).exactValue.foreach(checkRegex)
            if (func == "matches") computeExpr(tree)

          case Apply(Select(str, Name("replaceAll"|"replaceFirst")), List(regExpr, _str)) if (str.tpe.widen <:< StringClass.tpe) =>
            treePosHolder = regExpr
            StringAttrs(regExpr).exactValue.foreach(checkRegex)
            computeExpr(tree)

          case Select(Apply(scala_Predef_augmentString, List(regExpr)), r)
            if (scala_Predef_augmentString.toString.endsWith(".augmentString") && r.toString == "r") =>
            treePosHolder = regExpr
            StringAttrs(regExpr).exactValue.foreach(checkRegex)

          /// Some format string passing
          case Apply(formatFunc, params)
            if ((formatFunc.toString endsWith "Predef.printf")
            || (formatFunc.toString endsWith "Console.printf")
            || (formatFunc.toString endsWith "lang.String.format"))
            && (params.size >= 2) && (params(1) match {
              case Typed(_, Ident(tpnme.WILDCARD_STAR)) => false
              case _ => true
            }) =>

            StringAttrs.stringFunc(params.head, newTermName("format"), params.tail)

          /// Checks conditions that use Option.size (there is a separate check for all uses of Option.size)
          //ADD: Generalize... move to applyCond completely, make it less hacky
          case optCond @ Apply(Select(Select(Apply(option2Iterable, List(_option)), size), op), List(expr))
            if (option2Iterable.toString contains "Option.option2Iterable")
            && size.toString == "size"
            && optCond.tpe.widen <:< BooleanClass.tpe =>

            pushDefinitions()

            val valName = "__foobar__" //TODO: shoot me :P

            vals += valName -> (new Values(values = Set(0, 1)))

            val cond = Apply(Select(Ident(newTermName(valName)), op), List(expr))
            cond.pos = optCond.pos

            vals(valName).applyCond(cond)

            popDefinitions()

          case Block(_stmts, _ret) =>
            //println("block: "+b)
            pushDefinitions()

            try { super.traverse(tree) } catch catcher

            /// Try to use vars - TODO: is probably buggy
            /*val block = stmts :+ ret
            val vars = mutable.HashSet[String]()
            block foreach {
              case ValDef(m: Modifiers, varName, _, value) if (m.hasFlag(MUTABLE)) =>
                vars += varName.toString
                vals(varName.toString) = computeExpr(value)
                println("assign: "+(vals))
              case Assign(varName, value) if vars contains varName.toString =>
                vals(varName.toString) = computeExpr(value)
                println("reassign: "+(vals))
              case e =>
                // Throw away assigns inside other blocks
                for (v <- vars; if isAssigned(e, v)) {
                  vals(v) = Values.empty
                  println("discard: "+(vals))
                }
            }
            */

            popDefinitions()

          /// Pass on expressions
          case a =>
            //TODO: Range and Lists work too, I think
            if (a.tpe != null && ((a.tpe <:< StringClass.tpe) || (a.tpe <:< AnyValClass.tpe))) computeExpr(a)

            //if (vals.nonEmpty)println("in: "+showRaw(tree))
            //if (vals.nonEmpty)println(">   "+vals);
            //if (showRaw(tree).startsWith("Literal") || showRaw(tree).startsWith("Constant"))println("in: "+showRaw(tree))
            //tree.children.foreach(traverse)
            try { super.traverse(tree) } catch catcher
        }
        //super.traverse(tree)
      } catch catcher
    }
  }

  private[this] object PostRefChecksComponent extends PluginComponent {
    val global = LinterPlugin.this.global
    import global._

    override val runsAfter = List("refchecks")

    val phaseName = "linter-refchecked"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: global.CompilationUnit): Unit = {
        nowarnPositions.clear
        if (!unit.isJava) new PostRefChecksTraverser(unit).traverse(unit.body)
      }
    }

    class PostRefChecksTraverser(unit: CompilationUnit) extends Traverser {
      implicit val unitt: CompilationUnit = unit

      var superTraverse = true
      def catcher(): PartialFunction[Throwable, Unit] = {
        case _: NullPointerException => //Ignore
        case _: NoSuchMethodError => //Ignore
        case _: StackOverflowError => superTraverse = false
        case e: Exception => //e.printStackTrace//TODO: Print details and ask user to report it
      }
      def finalizer(tree: Tree): Unit = {
        if (superTraverse) try { super.traverse(tree) } catch catcher
      }
      override def traverse(tree: Tree): Unit = try {
        superTraverse = true
        tree match {
          /// Unused method parameters
          case DefDef(mods: Modifiers, name, _, valDefs, _, body)
            //TODO: write tests for the special cases - isSynthetic might cover some
            //TODO: scalaz is a good codebase for finding interesting false positives
            //TODO: macro impl is special case?
            if (name.toString != "<init>" && !name.toString.contains("$default$"))
            && !body.isEmpty && body.toString != staticSelect("scala", "Predef.???")
            && !mods.isSynthetic && !(mods.isOverride || tree.symbol.isOverridingSymbol) =>

            // Get the parameters, except the implicit/synthetic ones
            val params =
              valDefs
                .flatMap(_.filterNot(valDef => valDef.mods.isImplicit || valDef.mods.isSynthetic || valDef.name.toString == "$this"))
                .map(_.name.toString)
                .toBuffer

            if (!(name.toString == "main" && params.size == 1 && params.head == "args")) { // Filter main method
              val used = for (Ident(name) <- tree if params contains name.toString) yield name.toString
              val unused = params -- used

              if (unused.size > 0) {
                warn(tree, UnusedParameter(unused, name.toString.stripSuffix("$extension")))
              }
            }

          case _ =>
        }
      } catch catcher finally finalizer(tree)
    }
  }

}
