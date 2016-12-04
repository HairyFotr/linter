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
import scala.tools.nsc.{ Global, Properties }

final object Utils {
  var linterOptions = LinterOptions()
  val nowarnPositions = mutable.HashSet[Global#Position]()
  val nowarnMergeNestedIfsPositions = mutable.HashSet[Global#Position]() //TODO: hack

  def warn(tree: Global#Tree, warning: Warning)(implicit unit: Global#CompilationUnit): Unit = {
    warn(tree.pos, warning)(unit)
  }
  def warn(tree: Global#Position, warning: Warning)(implicit unit: Global#CompilationUnit): Unit = {
    val checkSep = "([+:]|[,] )"
    val partSep = "[ :]"
    val checkReg = "[A-Z][a-zA-Z]+"
    if((linterOptions.disabledWarningNames contains warning.name)
    || (tree.pos.lineContent matches s".*// *linter${partSep}(nowarn|ignore|disable)(${partSep}(${checkReg}${checkSep})*${warning.name}(${checkSep}${checkReg})*)? *(//.*)?")
    || (nowarnPositions contains tree.pos)) {
      // skip
    } else {
      // scalastyle:off regex
      //TODO: after 2.10: CompilationUnit is deprecated, call global.reporter.warning
      unit.warning(tree.pos, if (linterOptions.printWarningNames) s"[${warning.name}] ${warning.message}" else warning.message)
      // scalastyle:on regex
    }
  }

  def toBang(b: Boolean): String = if (b) "!" else ""

  def staticSelect(prefix: String, suffix: String): String = {
    if (Properties.versionString.contains("2.10")
    ||  Properties.versionString.contains("2.11"))
      s"$prefix.this.$suffix"
    else
      s"$prefix.$suffix"
  }

  // Regex matches either non meta characters, or escaped meta characters.
  // Full list of Java regex meta characters is: `<([{\^-=$!|]})?*+.>`.
  // Characters `<-=!>` are meta only inside groups, which the regex prevents.
  // Closing brackets ] and } act as normal characters, if there is no opening brackets
  // False positives: none that I know of
  // False negatives: none trivial enough to warrant inclusion, I guess
  val PlainStringRegex = """([^(\[{\\^$|)?*+.]|[\\][(\[{\\^$|)?*+.]|\[[^\[\\^\]]\])*"""
  def cleanPlainRegex(s: String): String = s.replaceAll("""[\\](.)""", "$1").replaceAll("""\[(.)\]""", "$1")

}

// Put only those that need the right global here:
class Utils[G <: Global](val global: G) {
  import global._
  global.definitions.init

  def isUsed(t: Tree, name: String, filter: String = ""): Boolean = {
    val tree = t.asInstanceOf[Tree]
    var used = 0
    for (Ident(id) <- tree; if id.toString == name) used += 1
    //TODO: Only for select types, also, maybe this doesn't belong in all uses of isUsed (e.g. Assignment right after declaration)
    // isSideEffectFreeFor(...)
    if (filter != "") {
      for (Select(Ident(id), func) <- tree; if (func.toString matches filter) && (id.toString == name)) used -= 1
    }

    (used > 0)
  }

  def getUsed(tree: Tree): Set[String] = {
    var used = Set.empty[String]
    for (Ident(id) <- tree) used += id.toString
    used
  }

  object Name {
    def unapply(name: Name): Option[String] = Some(name.toString)
  }

  object HeadOrLastName {
    def unapply(func: Name): Option[String] = func match {
      case Name("head"|"last") => Some(func.toString)
      case _ => None
    }
  }

  object SizeOrLengthName {
    def unapply(func: Name): Option[String] = func match {
      case Name("size"|"length") => Some(func.toString)
      case _ => None
    }
  }

  object ScalaSome { def unapply(tree: Tree): Boolean = tree is "scala.Some" }
  object ScalaNone { def unapply(tree: Tree): Boolean = tree is "scala.None" }

  object ArrayOpsOrAugmentedString {
    def unapply(tree: Tree): Boolean = tree.containsAny("ArrayOps", "augmentString")
  }

  object FloatingPointNumber {
    import definitions.{ DoubleClass, FloatClass }
    def unapply(tree: Tree): Option[Tree] = tree match {
      case Apply(xWrapper, List(FloatingPointNumber(x))) if xWrapper.toString endsWith "Wrapper" => Some(x)
      case t if isSubtype(tree, DoubleClass.tpe) || isSubtype(tree, FloatClass.tpe) => Some(t)
      case _ => None
    }
  }

  // Just a way to make the Tree/Name-String comparisons more readable
  //@deprecated("Replace with extractor object or elaborate pattern match", "0.1.17")
  abstract class RichToStr[T](n: T) {
    val nstr = n.toString
    def is(str: String): Boolean = nstr == str
    def isAny(strs: String*): Boolean = strs contains nstr
    def startsWith(str: String): Boolean = nstr startsWith str
    def startsWithAny(strs: String*): Boolean = strs.exists(nstr startsWith _)
    def endsWith(str: String): Boolean = nstr endsWith str
    def endsWithAny(strs: String*): Boolean = strs.exists(nstr endsWith _)
    def contains(str: String): Boolean = nstr contains str
    def containsAny(strs: String*): Boolean = strs.exists(nstr contains _)
  }
  implicit class richTree(n: Tree) extends RichToStr[Tree](n)
  implicit class richName(n: Name) extends RichToStr[Name](n)

  import definitions.{ AnyClass, OptionClass }
  val JavaConversionsModule: Symbol = rootMirror.getModuleByName(newTermName("scala.collection.JavaConversions"))
  val SeqLikeClass: Symbol = rootMirror.getClassByName(newTermName("scala.collection.SeqLike"))
  val SeqLikeContains: Symbol = SeqLikeClass.info.member(newTermName("contains"))
  val SeqLikeApply: Symbol = SeqLikeClass.info.member(newTermName("apply"))

  val OptionGet: Symbol = OptionClass.info.member(nme.get)

  val IsInstanceOf = AnyClass.info.member(nme.isInstanceOf_)
  val AsInstanceOf = AnyClass.info.member(nme.asInstanceOf_)
  val ToString: Symbol = AnyClass.info.member(nme.toString_)

  def seqMemberType(seenFrom: Type): Type = {
    SeqLikeClass.tpe.typeArgs.head.asSeenFrom(seenFrom, SeqLikeClass)
  }

  def isSubtype(x: Tree, y: Tree): Boolean = { x.tpe.widen <:< y.tpe.widen }
  def isSubtype(x: Tree, y: Type): Boolean = { x.tpe.widen <:< y.widen }

  def methodImplements(method: Symbol, target: Symbol): Boolean =
    try { method == target || method.allOverriddenSymbols.contains(target) } catch { case _: NullPointerException => false }

  def isGlobalImport(selector: ImportSelector): Boolean = {
    selector.name == nme.WILDCARD && selector.renamePos == -1
  }

  def isOptionOption(t: Tree): Boolean =
    (t.tpe.widen.baseClasses.exists(_.tpe =:= OptionClass.tpe)
    && t.tpe.widen.typeArgs.exists(_.widen.baseClasses.exists(_.tpe =:= OptionClass.tpe)))

  def isLiteral(t: Tree): Boolean = t match {
    case Literal(_) => true
    case _ => false
  }

  def getAssigned(tree: Tree): Set[String] = {
    (for (Assign(Ident(id), _) <- tree) yield id.toString).toSet
    //TODO: non-local stuff (for (Apply(Select(id, setter), List(_)) <- tree; if setter.toString endsWith "_$eq") yield setter.dropRight(4)).toSet
  }

  def isAssigned(tree: Tree, name: String): Boolean = {
    for (Assign(Ident(id), _) <- tree; if id.toString == name)
      return true

    false
  }

  def returnCount(tree: Tree): Int = {
    var used = 0
    for (Return(id) <- tree) used += 1
    used
  }
  def throwsCount(tree: Tree): Int = {
    var used = 0
    for (Throw(id) <- tree) used += 1
    used
  }
}
