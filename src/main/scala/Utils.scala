package com.foursquare.lint

import scala.tools.nsc.Global
import collection.mutable

object Utils {
  val nowarnPositions = mutable.HashSet[Global#Position]()
  
  def warn(pos: Global#Position, msg: String)(implicit unit: Global#CompilationUnit) {
    val line = pos.lineContent
    if((line matches ".*// *nolint *") || (nowarnPositions contains pos)) {
      // skip
    } else {
      // scalastyle:off regex
      unit.warning(pos, msg)
      // scalastyle:on regex
    }
  }
  def warn(tree: Global#Tree, msg: String)(implicit unit: Global#CompilationUnit) {
    warn(tree.pos, msg)
  }

  // Is the body simple enough to ignore?
  def isBodySimple(body: Global#Tree): Boolean = !body.isInstanceOf[Global#Block]
}

// Put only those that need the right global here:
class Utils[Glob <: Global](val global: Glob) {
  import global._
  global.definitions.init

  def isUsed(t: Tree, name: String): Boolean = {
    val tree = t.asInstanceOf[Tree]
    var used = 0
    for(Ident(id) <- tree; if id.toString == name) used += 1
    //TODO: Only for select types, also, maybe this doesn't belong in all uses of isUsed (e.g. Assignment right after declaration)
    // isSideEffectFreeFor(...)
    for(Select(Ident(id), func) <- tree; if (func.toString matches "size|length|head|last") && (id.toString == name)) used -= 1
    
    (used > 0)
  }
  
  def getUsed(tree: Tree): Set[String] = {
    var used = Set[String]()
    for(Ident(id) <- tree) used += id.toString
    used
  }

  import definitions.{AnyClass, NothingClass, ObjectClass, Object_==, OptionClass, SeqClass}
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
      
  def getAssigned(tree: Tree): Set[String] = {
    (for(Assign(Ident(id), _) <- tree) yield id.toString).toSet
    //TODO: non-local stuff (for(Apply(Select(id, setter), List(_)) <- tree; if setter.toString endsWith "_$eq") yield setter.dropRight(4)).toSet
  }

  def isAssigned(tree: Tree, name: String): Boolean = {
    for(Assign(Ident(id), _) <- tree; if id.toString == name) 
      return true
    
    false
  }
    
  def returnCount(tree: Tree): Int = {
    var used = 0
    for(Return(id) <- tree) used += 1
    used
  }
  def throwsCount(tree: Tree): Int = {
    var used = 0
    for(Throw(id) <- tree) used += 1
    used
  }
}
