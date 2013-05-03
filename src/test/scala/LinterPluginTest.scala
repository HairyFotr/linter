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

import org.junit.{Ignore, Before, Test}
import org.specs2.matcher.{StandardMatchResults, JUnitMustMatchers}
import util.matching.Regex
import collection.mutable

// TODO: 
// * each test should have a positive and a negative case
// * if it's worth it, error msgs could come from outside
// * handle/test plugin settings (when settings are done)

class LinterPluginTest extends JUnitMustMatchers with StandardMatchResults {
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
        val linterPlugin = new LinterPlugin(compiler)
        for (phase <- linterPlugin.components)
          compiler.asInstanceOf[ {def phasesSet: mutable.HashSet[tools.nsc.SubComponent]}].phasesSet += phase
        compiler
      }
    }

    def compileAndLint(code: String): String = {
      stringWriter.getBuffer.delete(0, stringWriter.getBuffer.length)
      val thunked = "() => { %s }".format(code)
      interpreter.interpret(thunked) match {
        case Results.Success => ""
        case Results.Error => stringWriter.toString
        case Results.Incomplete => throw new Exception("Incomplete code snippet")
      }
    }
  }
  val compiler = new Compiler

  // A few hacks to scrap the boilerplate and better pinpoint the failing test
  def should(code: String, nt: Boolean = false)(implicit expectedMsg: String) {
    val cleanCode = code.trim.stripMargin
    (expectedMsg, compiler.compileAndLint(cleanCode)) must beLike {
      case (expected, actual) if (nt ^ actual.contains(expected)) => ok
      case _ => ko("in "+(if(nt) "negative case" else "positive case")+":\n  " + cleanCode.trim.replaceAll("\\n", "\n  "))
    }
  }
  def shouldnt(code: String)(implicit expectedError: String) { should(code, nt = true)(expectedError) }
  def noWarnings(code: String) { compiler.compileAndLint(code) must be ("") }

  @Before
  def forceCompilerInit() {
    compiler.compileAndLint("1 + 1")
  }
  
  @Test
  def caseClass__NoWarn() {
    noWarnings("""case class A()""")
    noWarnings("""case class A(a: Float)""")
    noWarnings("""class A(a: Float, b: String)""")
  }

  @Test 
  def Source_FromFile__close() {
    implicit val msg = "You should close the file stream after use."
     
    should("""scala.io.Source.fromFile("README.md").mkString""")
    //should("""scala.io.Source.fromFile("README.md")""")
    
    shouldnt("""val a = scala.io.Source.fromFile("README.md"); a.mkString(""); a.close()""")
    shouldnt("""def fromFile(s: String) = ""; fromFile("aaa").mkString("")""")
  }

  @Test
  def contains__types() {
    implicit val msg = ") will probably return false."

    should("""val x = List(4); x.contains("foo")""")

    // Set and Map have type-safe contains methods so we don't want to warn on those. 
    shouldnt("""val x = Set(4); x.contains(3)""")
    shouldnt("""val x = Map(4 -> 5); x.contains(3)""")
  }


  @Test
  def import__JavaConversions() {
    implicit val msg = "Conversions in scala.collection.JavaConversions._ are dangerous."
    should("import scala.collection.JavaConversions._;")
  }

  @Test
  @Ignore
  def import__wildcard() {
    implicit val msg = "Wildcard imports should be avoided. Favor import selector clauses."

    should("import collection._;")
    shouldnt("import util.Random")
  }

  @Test
  def equals__types() {
    implicit val msg = "Comparing with == on instances of different types"

    should("Nil == None")
    should("""
      |val x: List[Int] = Nil
      |val y: List[String] = Nil
      |x == y""")

    shouldnt(""" "foo" == "bar" """)
    shouldnt("""
      |val x: List[Int] = Nil
      |val y: List[Int] = Nil
      |x == y""")
    shouldnt("""
      |val x: String = "foo"
      |val y: String = "bar"
      |x == y""")
    shouldnt("""
      |val x: String = "foo"
      |x == "bar" """)
  }

  @Test
  @Ignore
  def null__check() {
    implicit val msg = "Using null is considered dangerous."
    
    should("""val a = null""")    
    shouldnt("""val a = 5""")
  }

  @Test
  def if__useCondition() {
    implicit val msg = "Remove the if and just use the"
    
    should("""
      |val a,b = 5
      |if(a == b && b > 5) 
      |  true 
      |else 
      |  false""")
    should("""
      |val a,b = 5
      |if(a != b && b > 5)
      |  false
      |else
      |  true""")

    shouldnt("""
      |val a,b = 5
      |if(a != b && b > 5)
      |  1+1
      |else
      |  true""")
  }
  
  @Test
  def if__sameBranches() {
    implicit val msg = "If statement branches have the same structure"
    should("""
      |val a,b = 10
      |if(b > 4)
      |  (1+1,a) 
      |else
      |  (2,a)""")
    should("""
      |val a,b = 4
      |if(b > 4)
      |  (1+1,a) 
      |else if(b > 7)
      |  (2,a)""")
      
    shouldnt("""
      |val a,b = 4
      |if(b > 4)
      |  (3,a) 
      |else if(b > 7)
      |  (2,a)""")      
  }
  
  @Test
  def if_condition() {
    should("""if(1 > 5) 7 else 8""")("This condition will always be false.")
    should("""if(1 < 5) 7 else 8""")("This condition will always be true.")
  }

  @Test
  def case_neigbouringCases() {
    implicit val msg = "neighbouring cases are identical"
    should("""
      |val a = 7
      |a match { 
      |  case 3 => println("hello") 
      |  case 4 => println("hello") 
      |  case 5 => println("hello") 
      |  case _ => println("how low") 
      |}""")

    //TODO: shouldn't warn on complicated guards that can't really be merged
    
    shouldnt("""
      |val a = 7
      |a match { 
      |  case 3 => println("hello1")
      |  case 4 => println("hello2")
      |  case 5 => println("hello3")
      |  case _ => println("how low")
      |}""")
    shouldnt("""
      |import scala.concurrent._
      |import ExecutionContext.Implicits.global
      |import scala.util.{Failure,Success}
      |future { 1+1 } andThen { case Success(a) => println("win") case Failure(s) => println("fail") }
    """)
  }
  
  @Test
  def case_constantValue() {
    implicit val msg = "Pattern matching on a constant value"
    
    should("""
      |5 match {
      |  case 3 => "hello"
      |  case _ => "hi"
      |}""")
    
    //TODO: should("""val a = 5; a match { case 3 => "hello"; case _ => "hi" }""")
    
    shouldnt("""
      |val a = 5
      |a match {
      |  case 3 => "hello";
      |  case _ => "hi"
      |}""")
  }
  
  @Test
  @Ignore
  def case_ifStatement() {
    implicit val msg = "This is probably better written as an if statement."
    
    should("""
      val a = true;
      a match {
        case true => 0
        case false => 1
      }""")
  }

  @Test
  @Ignore
  def case_useMonadic() {
    implicit val msg = "There are probably better ways of handling an Option"
    
    should("""
      val a = Option("")
      a match {
        case Some(x) => x
        case _ => null
      }""")
  }

  @Test
  @Ignore
  def option_get() {
    implicit val msg = "Calling .get on Option will throw an exception if the Option is None."

    should("""Option(10).get""")
    should("""val x: Option[Int] = None ; x.get""")
    should("""val x: Option[Int] = Some(3); x.get""")
    should("""val x = None ; x.get""")
    should("""val x = Some(3) ; x.get""")

    shouldnt("""Map(1 -> "1", 2 -> "2").get(1)""")
  }

  @Test
  def implicit_returnType() {
    implicit val msg = "needs explicit return type"
    
    should("""
      |import scala.language.implicitConversions
      |implicit def int2string(a: Int) = a.toString
    """)
    
    shouldnt("""
      |import scala.language.implicitConversions
      |implicit def int2string(a: Int): String = a.toString
    """)
  }

  @Test
  def def__redundantParameters() {
    implicit val msg = "not used in method"
    
    should("""def func(a:Int, b:Int) = a""")
    should("""def func(a:Int)(implicit b:Int) = b""")
    
    shouldnt("""def func(a:Int)(implicit b:Int) = a""")
    shouldnt("""def func(a:Int) = a""")
    shouldnt("""def func(a:Int) {}""")
    shouldnt("""def func(a:Int) = ???""")
  }

  @Test
  @Ignore
  def string_duplicatedLiterals() {
    implicit val msg = "String literal"
    
    should("""
      |val a = "hh"
      |val b = "hh"
      |val c = "hh"
      |val d = "hh"
      |val e = "hh"
    """)
    //TODO: can't really decide if this one is a bug or a feature
    should("""
      |val a = "hh"
      |val b = s"${a}hh"
      |val c = s" ${a}hh"
      |val d = s"  ${a}hh"
      |val e = s"  ${a}hh"
    """)
    
    shouldnt("""
      |val a = "hh"
      |val b = a
      |val c = b
      |val d = c
      |val e = d
    """)
  }
  
  @Test
  def string_alreadyDefined() {
    implicit val msg = "You have defined that string as a val already"
    
    should("""
      |val a = "hh"
      |val b = "hh"
    """)
    /*TODO: should("""{
      |val a = "hh"
      |val b = "hh" + "cde"
    }""")*/
    
    shouldnt("""
      |val a = "hh"
      |val b = a + "cde"
    """)
    shouldnt("""
      |var a = "hh"
      |a += "aa"
      |val b = a + "cde"
    """)
  }
  
  @Test
  @Ignore
  def instanceOf__check() {
    implicit val msg = "Avoid using asInstanceOf"
    
    should("""
      |val a = "aa"
      |a.replace(a.asInstanceOf[CharSequence], "bb".asInstanceOf[CharSequence])
    """)
    
   shouldnt("""
      |val a = "aa"
      |a.replace(a: CharSequence, "bb": CharSequence)
    """)
   }
   
  @Test
  def numeric_log1p() {
    implicit val msg = "Use math.log1p instead of math.log"

    should("""
      |val a = 4d
      |math.log(1 + a)
    """)
    should("""
      |val a = 4d
      |math.log(1d + a)
    """)

    shouldnt("""
      |val a = 4d
      |math.log(2d + a)
    """)
    shouldnt("""
      |val a = 4d
      |math.log1p(1 + a)
    """)
  }

}

