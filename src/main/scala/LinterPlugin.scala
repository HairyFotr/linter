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

class LinterPlugin(val global: Global) extends Plugin {
  import global._

  val name = "linter"
  val description = ""
  val components = List[PluginComponent](PreTyperComponent, LinterComponent)
  
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
      import scala.tools.nsc.symtab.Flags.IMPLICIT

      override def traverse(tree: Tree) {
        tree match {
          case DefDef(m: Modifiers, name, _, _, TypeTree(), _) => {
            if ((m.flags & IMPLICIT) != 0)
              unit.warning(tree.pos, "Implicit method %s needs explicit return type" format name)
          }
          case _ => 
        }
        super.traverse(tree)
      }
    }
  }


  private object LinterComponent extends PluginComponent {
    import global._

    val global = LinterPlugin.this.global

    override val runsAfter = List("typer")

    val phaseName = "linter-typed"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: global.CompilationUnit) {
        new LinterTraverser(unit).traverse(unit.body)
      }
    }

    class LinterTraverser(unit: CompilationUnit) extends Traverser {
      import definitions.{AnyClass, ObjectClass, Object_==, OptionClass, SeqClass}

      val JavaConversionsModule: Symbol = definitions.getModule(newTermName("scala.collection.JavaConversions"))
      val SeqLikeClass: Symbol = definitions.getClass(newTermName("scala.collection.SeqLike"))
      val SeqLikeContains: Symbol = SeqLikeClass.info.member(newTermName("contains"))
      val OptionGet: Symbol = OptionClass.info.member(nme.get)

      val DoubleClass: Symbol = definitions.getClass(newTermName("scala.Double"))
      val FloatClass: Symbol = definitions.getClass(newTermName("scala.Float"))

      def SeqMemberType(seenFrom: Type): Type = {
        SeqLikeClass.tpe.typeArgs.head.asSeenFrom(seenFrom, SeqLikeClass)
      }

      def isSubtype(x: Tree, y: Tree): Boolean = {
        x.tpe.widen <:< y.tpe.widen
      }
      def isSubtype(x: Tree, y: Type): Boolean = {
        x.tpe.widen <:< y.widen
      }

      def methodImplements(method: Symbol, target: Symbol): Boolean = {
        method == target || method.allOverriddenSymbols.contains(target)
      }

      def isGlobalImport(selector: ImportSelector): Boolean = {
        selector.name == nme.WILDCARD && selector.renamePos == -1
      }
      
      override def traverse(tree: Tree): Unit = tree match {
        case Select(fromFile, _) if fromFile.toString startsWith "scala.io.Source.fromFile" => //TODO: Too hacky :)
          val warnMsg = "You should close the file stream after use."
          unit.warning(fromFile.pos, warnMsg)

        case Apply(Select(lhs, nme.EQ), List(rhs)) if isSubtype(lhs, DoubleClass.tpe) || isSubtype(lhs, FloatClass.tpe) || isSubtype(rhs, DoubleClass.tpe) || isSubtype(rhs, FloatClass.tpe) =>
          //TODO: Fix false positive for """case class A(a: Float)"""
          //val warnMsg = "Exact comparison of floating point values is potentially unsafe."
          //unit.warning(tree.pos, warnMsg)
          
        case Apply(eqeq @ Select(lhs, nme.EQ), List(rhs)) if methodImplements(eqeq.symbol, Object_==) && !isSubtype(lhs, rhs) && !isSubtype(rhs, lhs) =>
          val warnMsg = "Comparing with == on instances of different types (%s, %s) will probably return false."
          unit.warning(eqeq.pos, warnMsg.format(lhs.tpe.widen, rhs.tpe.widen))

        case Import(pkg, selectors) if pkg.symbol == JavaConversionsModule && selectors.exists(isGlobalImport) =>
          unit.warning(pkg.pos, "Conversions in scala.collection.JavaConversions._ are dangerous.")
        
        case Import(pkg, selectors) if selectors.exists(isGlobalImport) =>
          //TODO: Too much noise - maybe if would be useful if told us non-IDE users which classes we're using
          //unit.warning(pkg.pos, "Wildcard imports should be avoided. Favor import selector clauses.")

        case Apply(contains @ Select(seq, _), List(target)) if methodImplements(contains.symbol, SeqLikeContains) && !(target.tpe <:< SeqMemberType(seq.tpe)) =>
          val warnMsg = "%s.contains(%s) will probably return false."
          unit.warning(contains.pos, warnMsg.format(seq.tpe.widen, target.tpe.widen))

        case get @ Select(_, nme.get) if methodImplements(get.symbol, OptionGet) => 
          unit.warning(tree.pos, "Calling .get on Option will throw an exception if the Option is None.")

        case If(Apply(Select(_, nme.EQ), List(Literal(Constant(null)))), Literal(Constant(false)), Literal(Constant(true))) =>
          // Removes both the null warning and if check for """case class A()""" ... (x$0.==(null) - see unapply AST of such case class)
        case Literal(Constant(null)) =>
          //TODO: Too much noise - limit in some way
          //unit.warning(tree.pos, "Using null is considered dangerous.")

        case Match(Literal(Constant(a)), _) =>
          unit.warning(tree.pos, "Pattern matching on a constant value "+a+".")

        case Match(pat, cases) if pat.tpe.toString != "Any @unchecked" && cases.size >= 2 =>
          //TODO: "Any @unchecked" seems to happen on the matching structures of actors - and they all return true :)
          //TODO: This handles multiple rules already - clean it up.
          case class EqCheck(streak: Int, tree: CaseDef)
          def printStreak(s: EqCheck) {
            if(s.streak == cases.size) {
              //This one always turns out to be a false positive
              //unit.warning(tree.pos, "All "+cases.size+" cases will return "+cases.head.body+", regardless of pattern value") 
            } else if(s.streak > 1) {
              unit.warning(s.tree.body.pos, s.streak+" neighbouring cases will return "+s.tree.body+", and should be merged.")   
            }
          }
          
          var curr = EqCheck(1, cases.head)
          var last = cases.head
          var someCase, noneCase, _Case = false
          val (someCaseReg, noneCaseReg, _CaseReg) = ("Some[\\[].*[\\]]", "None[.]type", "Option[\\[].*[\\]]") //TODO: Hacky hack hack -_-, sorry
          def checkRegs(caseTree: CaseDef) {
            val caseStr = caseTree.pat.tpe.toString
            someCase |= (caseStr matches someCaseReg)
            noneCase |= (caseStr matches noneCaseReg)
            _Case |= (caseStr matches _CaseReg)  
          }
          checkRegs(last)
          for(c <- cases.tail) {
            if(c.body equalsStructure last.body) {
              curr = EqCheck(curr.streak+1, c)
            } else {
              printStreak(curr)
              curr = EqCheck(1, c)
            }
            last = c
            checkRegs(last)
          }

          printStreak(curr)
          
          if(cases.size == 2 && ((someCase || _Case) && (noneCase || _Case) && (noneCase || someCase))) {
            unit.warning(tree.pos, "There are probably better ways of handling an Option (see: http://blog.tmorris.net/posts/scalaoption-cheat-sheet/)")
          }

        //TODO: Can I get the unprocessed condition string?
        case If(cond, Literal(Constant(true)), Literal(Constant(false))) =>
          unit.warning(cond.pos, "Remove the if and just use the condition: "+cond)
        case If(cond, Literal(Constant(false)), Literal(Constant(true))) =>
          unit.warning(cond.pos, "Remove the if and just use the negated condition: !("+cond+")")
        //case If(cond, Literal(Constant(a: Any)), Literal(Constant(b: Any))) if a == b =>
        case If(cond, a, b) if a equalsStructure b =>
          unit.warning(cond.pos, "Result will be "+a+" regardless of condition.")

        case If(cond @ Literal(Constant(a: Boolean)), _, _) => 
          //TODO: try to figure out things like (false && a > 5 && ...) (btw, this works if a is a final val)
          val warnMsg = "This condition will always be "+a+"."
          unit.warning(cond.pos, warnMsg)

        // cannot check double/float, as typer will automatically translate it to Infinity
        case divByZero @ Apply(Select(rcvr, nme.DIV), List(Literal(Constant(0))))
          if (rcvr.tpe <:< definitions.ByteClass.tpe
            ||rcvr.tpe <:< definitions.ShortClass.tpe
            ||rcvr.tpe <:< definitions.IntClass.tpe
            ||rcvr.tpe <:< definitions.LongClass.tpe) =>
          unit.warning(divByZero.pos, "Literal division by zero.")
        case divByZero @ Apply(Select(rcvr, nme.MOD), List(Literal(Constant(0))))
          if (rcvr.tpe <:< definitions.ByteClass.tpe
            ||rcvr.tpe <:< definitions.ShortClass.tpe
            ||rcvr.tpe <:< definitions.IntClass.tpe
            ||rcvr.tpe <:< definitions.LongClass.tpe) => 
          unit.warning(divByZero.pos, "Literal division by zero.")

        case _ =>
          super.traverse(tree)
      }
    }
  }
}
