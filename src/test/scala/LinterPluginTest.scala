/**
 * Copyright 2012 Foursquare Labs, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.foursquare.lint

import org.junit.{Before, Test}
import org.specs2.matcher.{StandardMatchResults, JUnitMustMatchers}
import util.matching.Regex
import collection.mutable

class LinterPluginTest extends JUnitMustMatchers with StandardMatchResults {
  var linterPlugin: LinterPlugin = null

  class Compiler {

    import java.io.{PrintWriter, StringWriter}
    import scala.io.Source
    import scala.tools.nsc.Settings
    import scala.tools.nsc.interpreter.{IMain, Results}
    import scala.tools.nsc.reporters.Reporter

    private val settings = new Settings
    val loader = manifest[LinterPlugin].erasure.getClassLoader
    settings.classpath.value = Source.fromURL(loader.getResource("app.class.path")).mkString
    settings.bootclasspath.append(Source.fromURL(loader.getResource("boot.class.path")).mkString)
    settings.deprecation.value = true // enable detailed deprecation warnings
    settings.unchecked.value = true // enable detailed unchecked warnings
    settings.Xwarnfatal.value = true
    // warnings cause compile failures too

    val stringWriter = new StringWriter()

    // This is deprecated in 2.9.x, but we need to use it for compatibility with 2.8.x
    private val interpreter = new IMain(settings, new PrintWriter(stringWriter)) {
      override protected def newCompiler(settings: Settings, reporter: Reporter) = {
        settings.outputDirs setSingleOutput virtualDirectory
        val compiler = super.newCompiler(settings, reporter)
        linterPlugin = new LinterPlugin(compiler)
        for (phase <- linterPlugin.components)
          compiler.asInstanceOf[ {def phasesSet: mutable.HashSet[tools.nsc.SubComponent]}].phasesSet += phase
        compiler
      }
    }

    def compileAndLint(code: String): Option[String] = {
      stringWriter.getBuffer.delete(0, stringWriter.getBuffer.length)
      val thunked = "() => { %s }".format(code)
      interpreter.interpret(thunked) match {
        case Results.Success => None
        case Results.Error => Some(stringWriter.toString)
        case Results.Incomplete => throw new Exception("Incomplete code snippet")
      }
    }
  }

  val compiler = new Compiler

  def check(code: String, expectedError: Option[Regex] = None) {
    // Either they should both be None or the expected error should be a
    // substring of the actual error.
    (expectedError, compiler.compileAndLint(code)) must beLike {
      case (None, None) => ok
      case (Some(exp), Some(act)) if exp.findFirstIn(act).isDefined => ok
    }
  }

  @Before
  def forceCompilerInit() {
    check( """1 + 1""", None)
  }

  @Test
  def testFromFile() {
    check( """scala.io.Source.fromFile("README.md")""", None)
    check( """scala.io.Source.fromFile("README.md").mkString""", Some("You should close the file stream after use.".r))
  }

  @Test
  def testHasVersusContains() {
    val msg = Some( """List\[Int\].contains\(.*String\) will probably return false.""".r)

    check( """val x = List(4); x.contains("foo")""", msg)

    // Set and Map have type-safe contains methods so we don't want to warn on
    // those.
    check( """val x = Set(4); x.contains(3)""", None)
    check( """val x = Map(4 -> 5); x.contains(3)""", None)
  }

  @Test
  def testNoOptionGet() {
    val msg = Some("Calling .get on Option will throw an exception if the Option is None.".r)

    check( """Option(10).get""", msg)
    check( """val x: Option[Int] = None ; x.get""", msg)
    check( """val x: Option[Int] = Some(3); x.get""", msg)
    check( """val x = None ; x.get""", msg)
    check( """val x = Some(3) ; x.get""", msg)

    check( """Map(1 -> "1", 2 -> "2").get(1)""")
  }

  @Test
  def testJavaConversionsImport() {
    val msg = Some("Conversions in scala.collection.JavaConversions._ are dangerous.".r)

    check("import scala.collection.JavaConversions._;", msg)
  }

  @Test
  def testAnyWildcardImport() {
    val msg = Some("Wildcard imports should be avoided. Favor import selector clauses.".r)

    check("import collection._;", msg)
  }

  @Test
  def testUnsafeEquals() {
    val msg = Some("Comparing with ==".r)

    // Should warn
    check("Nil == None", msg)
    check( """{
      val x: List[Int] = Nil
      val y: List[String] = Nil
      x == y
    }""", msg)

    // Should compile
    check( """ "foo" == "bar" """)
    check( """{
      val x: List[Int] = Nil
      val y: List[Int] = Nil
      x == y
    }""")
    check( """{
      val x: String = "foo"
      val y: String = "bar"
      x == y
    }""")
    check( """{
      val x: String = "foo"
      x == "bar"
    }""")
  }

  @Test
  def testNull() {
    check( """val a = null""", Some("Using null is considered dangerous.".r))
  }

  @Test
  def testCaseClassNull() {
    check( """case class A()""")
  }

  @Test
  def testCaseClassFloat() {
    check( """case class A(a: Float)""")
  }

  @Test
  def testUselessIf() {
    check( """val a,b = 5; if(a == b && b > 5) true else false""", Some(".+Remove the if and just use the condition.+".r))
    check( """val a,b = 5; if(a == b && b > 5) false else true""", Some(".+Remove the if and just use the negated condition.+".r))
  }

}
