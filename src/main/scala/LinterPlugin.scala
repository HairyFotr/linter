// Copyright 2011 Foursquare Labs Inc. All Rights Reserved.

package com.foursquare.lint

import scala.reflect.generic.Flags
import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}

class LinterPlugin(val global: Global) extends Plugin {
  import global._

  val name = "linter"
  val description = ""
  val components = List[PluginComponent](LinterComponent)

  private object LinterComponent extends PluginComponent {
    import global._
    import global.definitions._

    val global = LinterPlugin.this.global

    override val runsAfter = List("typer")

    val phaseName = "linter"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: global.CompilationUnit): Unit = {
        new LinterTraverser(unit).traverse(unit.body)
      }
    }

    class LinterTraverser(unit: CompilationUnit) extends Traverser {
      import definitions.{AnyClass, OptionClass, SeqClass}

      val JavaConversionsModule: Symbol =
        definitions.getModule("scala.collection.JavaConversions")

      val SeqLikeClass: Symbol =
        definitions.getClass("scala.collection.SeqLike")

      val SeqLikeContains: Symbol =
        SeqLikeClass.info.member(newTermName("contains"))

      val SeqLikeA: Type =
        SeqLikeClass.tpe.typeArgs.head

      val AnyEquals: Symbol =
        AnyClass.info.member(newTermName("$eq$eq"))

      val AnyRefEquals: Symbol =
        AnyRefClass.info.member(newTermName("$eq$eq"))

      val GetMethod = newTermName("get")

      val EqualsMethod = newTermName("$eq$eq")

      def isSubtype(x: Tree, y: Tree): Boolean = {
        x.tpe.widen <:< y.tpe.widen
      }

      def methodImplements(method: Symbol, target: Symbol): Boolean = {
        method == target || method.allOverriddenSymbols.contains(target)
      }

      override def traverse(tree: Tree): Unit = tree match {
        case Apply(s @ Select(lhs, EqualsMethod), List(rhs)) if !(isSubtype(lhs, rhs) || isSubtype(rhs, lhs)) =>
          unit.warning(s.pos, "Calling == on values of incompatible types.")

        case Import(t, selectors) if selectors.exists(_.name == global.nme.WILDCARD) && t.symbol == JavaConversionsModule =>
          unit.warning(t.pos, "Conversions in scala.collection.JavaConversions._ are dangerous.")

        case a @ Apply(s@Select(seq, _), p)
          if methodImplements(s.symbol, SeqLikeContains)
          && !(p.head.tpe <:< SeqLikeA.asSeenFrom(seq.tpe, SeqLikeClass)) =>

          unit.warning(s.pos, "SeqLike[%s].contains(%s) will probably return false." format(SeqLikeA.asSeenFrom(seq.tpe, SeqLikeClass), p.head.tpe))

        case node @ Select(q, GetMethod) if q.symbol.isSubClass(OptionClass) =>
          if (!node.pos.source.path.contains("src/test")) {
            unit.warning(node.pos, "Calling .get on Option will throw an exception if the Option is None.")
          }

        case _ =>
          super.traverse(tree)
      }
    }
  }
}
