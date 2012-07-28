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

import org.junit.{Before, Test}
import org.specs.SpecsMatchers

class LinterPluginTest extends SpecsMatchers {
  var linterPlugin: LinterPlugin = null

  class Compiler {
    import java.io.{PrintWriter, StringWriter}
    import scala.io.Source
    import scala.tools.nsc.{Global, Settings}
    import scala.tools.nsc.interpreter.{IMain, Results}
    import scala.tools.nsc.reporters.Reporter

    private val settings = new Settings
    val loader = manifest[LinterPlugin].erasure.getClassLoader
    settings.classpath.value = Source.fromURL(loader.getResource("app.class.path")).mkString
    settings.bootclasspath.append(Source.fromURL(loader.getResource("boot.class.path")).mkString)
    settings.deprecation.value = true // enable detailed deprecation warnings
    settings.unchecked.value = true // enable detailed unchecked warnings
    settings.Xwarnfatal.value = true // warnings cause compile failures too

    val stringWriter = new StringWriter()

    // This is deprecated in 2.9.x, but we need to use it for compatibility with 2.8.x
    private val interpreter = new IMain(settings, new PrintWriter(stringWriter)) {
      override protected def newCompiler(settings: Settings, reporter: Reporter) = {
        settings.outputDirs setSingleOutput virtualDirectory
        new Global(settings, reporter) {
          override protected def computeInternalPhases () {
            super.computeInternalPhases
            linterPlugin = new LinterPlugin(this)
            for (phase <- linterPlugin.components)
              phasesSet += phase
          }
        }
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
  def check(code: String, expectedError: Option[String] = None) {
    // Either they should both be None or the expected error should be a
    // substring of the actual error.
    (expectedError, compiler.compileAndLint(code)) must beLike {
      case (None, None) => true
      case (Some(exp), Some(act)) => act.contains(exp)
    }
  }

  @Before
  def forceCompilerInit(): Unit = {
    check("""1 + 1""", None)
  }

  @Test
  def testHasVersusContains(): Unit = {
    val msg = Some("SeqLike[Int].contains(java.lang.String) will probably return false.")

    check("""val x = List(4); x.contains("foo")""", msg)

    // Set and Map have type-safe contains methods so we don't want to warn on
    // those.
    check("""val x = Set(4); x.contains(3)""", None)
    check("""val x = Map(4 -> 5); x.contains(3)""", None)
  }

  @Test
  def testNoOptionGet(): Unit = {
    val msg = Some("Calling .get on Option will throw an exception if the Option is None.")

    check("""Option(10).get""", msg)
    check("""val x: Option[Int] = None ; x.get""", msg)
    check("""val x: Option[Int] = Some(3); x.get""", msg)
    check("""val x = None ; x.get""", msg)
    check("""val x = Some(3) ; x.get""", msg)

    check("""Map(1 -> "1", 2 -> "2").get(1)""")
  }

  @Test
  def testJavaConversionsImport(): Unit = {
    val msg = Some("Conversions in scala.collection.JavaConversions._ are dangerous.")

    check("import scala.collection.JavaConversions._;", msg)
  }
  
  @Test
  def testAnyWildcardImport(): Unit = {
    val msg = Some("Wildcard imports should be avoided.  Favor import selector clauses.")
    
    check("import org.specs._;", msg)
  }

  @Test
  def testUnsafeEquals(): Unit = {
    val msg = Some("Comparing with ==")

    // Should warn
    check("Nil == None", msg)
    check("""{
      val x: List[Int] = Nil
      val y: List[String] = Nil
      x == y
    }""", msg)

    // Should compile
    check(""" "foo" == "bar" """)
    check("""{
      val x: List[Int] = Nil
      val y: List[Int] = Nil
      x == y
    }""")
    check("""{
      val x: String = "foo"
      val y: String = "bar"
      x == y
    }""")
    check("""{
      val x: String = "foo"
      x == "bar"
    }""")
  }
}
