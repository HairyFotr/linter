// Copyright 2011 Foursquare Labs Inc. All Rights Reserved.

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
    val msg = Some("SeqLike[Int].contains(java.lang.String(\"foo\")) will probably return false.")

    check("""val x = List(4); x.contains("foo")""", msg)

    // Set and Map have type-safe contains methods so we don't want to warn on
    // those.
    check("""val x = Set(4); x.contains(3)""", None)
    check("""val x = Map(4 -> 5); x.contains(3)""", None)
  }

  @Test
  def testNoOptionGet(): Unit = {
    val msg = Some("Calling .get on Option will throw an exception if the Option is None.")

    check("""val x: Option[Int] = None ; x.get""", msg)
    check("""val x: Option[Int] = Some(3); x.get""", msg)
    check("""val x = None ; x.get""", msg)
    check("""val x = Some(3) ; x.get""", msg)
  }

  @Test
  def testJavaConversionsImport(): Unit = {
    val msg = Some("Conversions in scala.collection.JavaConversions._ are dangerous.")

    check("import scala.collection.JavaConversions._;", msg)
  }

  @Test
  def testUnsafeEquals(): Unit = {
    val msg = Some("Calling == on values of incompatible types.")

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
