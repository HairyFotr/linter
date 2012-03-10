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
      override def traverse(tree: Tree): Unit = tree match {

        // ADD YOUR EXTRA TYPE CHECKS HERE
        // ADD YOUR EXTRA TYPE CHECKS HERE
        // ADD YOUR EXTRA TYPE CHECKS HERE

        case _ =>
          super.traverse(tree)
      }
    }
  }
}
