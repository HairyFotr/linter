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
    check( """1 + 1""")
  }

  @Test
  def testFromFile() {
    check( """scala.io.Source.fromFile("README.md").mkString""", Some("You should close the file stream after use.".r))
    check( """scala.io.Source.fromFile("README.md")""")
  }

  @Test
  def testHasVersusContains() {
    val msg = Some( """List\[Int\].contains\(.*String\) will probably return false.""".r)

    check( """val x = List(4); x.contains("foo")""", msg)

    // Set and Map have type-safe contains methods so we don't want to warn on
    // those. 
    check( """val x = Set(4); x.contains(3)""")
    check( """val x = Map(4 -> 5); x.contains(3)""")
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
  def testClass() {
    check( """class A(a: Float, b: String)""")
  }

  @Test
  def testIfChecks() {
    val msg = Some("Remove the if ".r)
    check( """
      val a,b = 5
      if(a == b && b > 5) 
        true 
      else 
        false""", msg)
        
    check( """
      val a,b = 5
      if(a != b && b > 5)
        false
      else
        true""", msg)
  }
  @Test
  def testIfChecks2() {
    val msg = Some("""Result will always be""".r)
    check( """
      val a,b = 10
      if(b > 4)
        (1+1,a) 
      else
        (2,a)""", msg)
        
    check( """
      val a,b = 4
      if(b > 4)
        (1+1,a) 
      else if(b > 7)
        (2,a)""")
  }
  @Test
  def testIfChecks3() {
    check( """if(1 > 5) 7 else 8""", Some("This condition will always be false.".r))
    check( """if(1 < 5) 7 else 8""", Some("This condition will always be true.".r))
  }

  @Test
  def testCaseChecks() {
    val msg = Some("""[0-9]+ neighbouring cases will return .+ and should be merged.""".r)
    check( """
      val a = 7
      a match { 
        case 3 => println("hello") 
        case 4 => println("hello") 
        case 5 => println("hello") 
        case _ => println("how low") 
      }""", msg)
    
    check( """
      val a = 7;
      a match { 
        case 3 => println("hello1")
        case 4 => println("hello2")
        case 5 => println("hello3")
        case _ => println("how low")
      }""")
  }
  @Test
  def testCaseChecks2() {
    check( """
      5 match {
        case 3 => "hello"
        case _ => "hi"
      }""", Some("""Pattern matching on a constant value""".r))
    
    //Negative case now, but could be positive in the future
    //check( """val a = 5; a match { case 3 => "hello"; case _ => "hi" }""", Some("""Pattern matching on a constant value""").r)
    
    check( """
      var a = 5
      a match {
        case 3 => "hello";
        case _ => "hi"
      }""")
  }
  @Test
  def testCaseChecks3() {
    check( """
      val a = Option("")
      a match {
        case Some(x) => x
        case _ => null
      }""", Some("""There are probably better ways of handling an Option""".r))
  }
  @Test
  def testCaseChecks4() {
    check( """
      val a = true;
      a match {
        case true => 0
        case false => 1
      }""", Some("""This is probably better written as an if statement.""".r))
  }


}
