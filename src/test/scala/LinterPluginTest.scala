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
// * have longer tests, that maybe trigger several checks
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
    //settings.deprecation.value = true // enable detailed deprecation warnings
    //settings.unchecked.value = true // enable detailed unchecked warnings    
    settings.Xwarnfatal.value = true // warnings cause compile failures too

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
    (expectedMsg, compiler.compileAndLint(code)) must beLike {
      case (expected, actual) if (nt ^ actual.contains(expected)) => ok
      case _ => ko("in "+(if(nt) "negative case" else "positive case")+":\n" + code + "\n ")
    }
  }
  def shouldnt(code: String)(implicit expectedMsg: String) { should(code, nt = true)(expectedMsg) }
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
    implicit val msg = "Implicit conversions in collection.JavaConversions are dangerous"
    should("import scala.collection.JavaConversions._;")
  }

  @Test
  @Ignore
  def import__wildcard() {
    implicit val msg = "Wildcard imports should be avoided. Favor import selector clauses."

    should("import collection._;")
    shouldnt("import collection.mutable.HashSet;")
    shouldnt("import collection.mutable.{HashSet, ListBuffer};")
    shouldnt("import util.Random;")
  }

  @Test
  def equals__types() {
    implicit val msg = "Comparing with == on instances of different types"//(%s, %s) will probably return false.

    should("Nil == None")
    should("""
      val x: List[Int] = Nil
      val y: List[String] = Nil
      x == y""")//TODO: returns true, not false

    shouldnt(""" "foo" == "bar" """)
    shouldnt("""
      val x: List[Int] = Nil
      val y: List[Int] = Nil
      x == y""")
    shouldnt("""
      val x: String = "foo"
      val y: String = "bar"
      x == y""")
    shouldnt("""
      val x: String = "foo"
      x == "bar" """)
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
      val a,b = 5
      if(a == b && b > 5) 
        true 
      else 
        false""")
    should("""
      val a,b = 5
      if(a != b && b > 5)
        false
      else
        true""")

    shouldnt("""
      val a,b = 5
      if(a != b && b > 5)
        1+1
      else
        true""")
  }
  
  @Test
  def if__sameBranches() {
    implicit val msg = "If statement branches have the same structure"
    should("""
      val a,b = 10
      if(b > 4)
        (1+1,a) 
      else
        (2,a)""")
    should("""
      val a,b = 4
      if(b > 4)
        (1+1,a) 
      else if(b > 7)
        (2,a)""")
      
    shouldnt("""
      val a,b = 4
      if(b > 4)
        (3,a) 
      else if(b > 7)
        (2,a)""")      
  }
  
  @Test
  @Ignore
  def if__condition() {
    should("""if(1 > 5) 7 else 8""")("This condition will always be false.")
    should("""if(1 < 5) 7 else 8""")("This condition will always be true.")

    shouldnt("""while(1 < 5) { 7; 8 """)("This condition will always be true.")
  }

  @Test
  def case__neigbouringCases() {
    implicit val msg = "neighbouring cases are identical"
    should("""
      val a = 7
      a match { 
        case 3 => println("hello") 
        case 4 => println("hello") 
        case 5 => println("hello") 
        case _ => println("how low") 
      }""")

    //TODO: shouldn't warn on complicated guards that can't really be merged
    
    shouldnt("""
      val a = 7
      a match { 
        case 3 => println("hello1")
        case 4 => println("hello2")
        case 5 => println("hello3")
        case _ => println("how low")
      }""")
    shouldnt("""
      import scala.concurrent._
      import ExecutionContext.Implicits.global
      import scala.util.{Failure,Success}
      future { 1+1 } andThen { case Success(a) => println("win") case Failure(s) => println("fail") }
    """)
  }
  
  @Test
  def case__constantValue() {
    implicit val msg = "Pattern matching on a constant value"
    
    should("""
      5 match {
        case 3 => "hello"
        case _ => "hi"
      }""")
    
    //TODO: should("""val a = 5; a match { case 3 => "hello"; case _ => "hi" """)
    
    shouldnt("""
      val a = 5
      a match {
        case 3 => "hello";
        case _ => "hi"
      }""")
  }
  
  @Test
  @Ignore
  def case__ifStatement() {
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
  def case__useMonadic() {
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
  def option__get() {
    implicit val msg = "Calling .get on Option will throw an exception if the Option is None."

    should("""Option(10).get""")
    should("""val x: Option[Int] = None ; x.get""")
    should("""val x: Option[Int] = Some(3); x.get""")
    should("""val x = None ; x.get""")
    should("""val x = Some(3) ; x.get""")

    shouldnt("""Map(1 -> "1", 2 -> "2").get(1)""")
  }

  @Test
  @Ignore
  def implicit__returnType() {
    implicit val msg = "needs explicit return type"
    
    should("""
      import scala.language.implicitConversions
      implicit def int2string(a: Int) = a.toString
    """)
    
    shouldnt("""
      import scala.language.implicitConversions
      implicit def int2string(a: Int): String = a.toString
    """)
  }

  @Test
  def def__redundantParameters() {
    implicit val msg = "not used in method"
    
    should("""def func(a:Int, b:Int) = a""")
    should("""def func(a:Int)(implicit b:Int) = b""")
    
    shouldnt("""def func(a:Int)(implicit b:Int) = a""")
    shouldnt("""def func(a:Int) = a""")
    shouldnt("""def func(a:Int) = {}""")
    shouldnt("""def func(a:Int) {}""")
    shouldnt("""def func(a:Int) = ???""")
  }

  @Test
  @Ignore
  def string__duplicatedLiterals() {
    implicit val msg = "String literal"
    
    should("""
      val a = "hh"
      val b = "hh"
      val c = "hh"
      val d = "hh"
      val e = "hh"
    """)
    //TODO: can't really decide if this one is a bug or a feature
    should("""
      val a = "hh"
      val b = s"${a}hh"
      val c = s" ${a}hh"
      val d = s"  ${a}hh"
      val e = s"  ${a}hh"
    """)
    
    shouldnt("""
      val a = "hh"
      val b = a
      val c = b
      val d = c
      val e = d
    """)
  }
  
  @Test
  @Ignore
  def string__alreadyDefined() {
    implicit val msg = "You have defined that string as a val already"
    
    should("""
      val a = "hh"
      val b = "hh"
    """)
    //"hh" + "hh" is auto-joined by the compiler, so you can't detect
    should("""
      val a = "hh"
      val b = "hh" + a
    """)
    should("""
      val a = "hh"
      println("hh")
    """)
    
    shouldnt("""
      val a = "hh"
      val b = a + "cde"
    """)
    shouldnt("""
      var a = "hh"
      a += "aa"
      val b = a + "cde"
    """)
    shouldnt("""
      println("hh")
      val a = "hh"
    """)
  }
  
  @Test
  def abs_interpretation__assert() {
    implicit var msg = "will never hold"
    
    should("""
      val a = 5
      assert(a == 6)
    """)
    should("""
      val a,b = 5
      assert(a == b+1)
    """)
    shouldnt("""
      val a = 5
      assert(a == 5)
    """)
    shouldnt("""
      val a,b = 5
      assert(a == b)
    """)

    msg = "will always hold"
    
    shouldnt("""
      val a = 5
      assert(a == 6)
    """)
    shouldnt("""
      val a,b = 5
      assert(a == b+1)
    """)
    should("""
      val a = 5
      assert(a == 5)
    """)
    should("""
      val a,b = 5
      assert(a == b)
    """)
  }

  @Test
  def abs_interpretation__listAndCondition() {
    //isUsed is the suspect, if this fails inside the if
    should(""" val a = List(1,2,3); if(a.size == 3) for(i <- a) 1/i """)("This condition will always hold.")
    should(""" val a = List(1,2,3); if(a.size > 3) for(i <- a) 1/i """)("This condition will never hold.")
    should(""" val a = List(1,2,3); for(i <- a) 1/(i-1) """)("You will likely divide by zero here.")
    should(""" val k = 3; val a = List(1,2,3); if(a.size == k) for(i <- a) 1/i """)("This condition will always hold.")
    should(""" val k = 4; val a = List(1,2,3); if(a.size == k) for(i <- a) 1/i """)("This condition will never hold.")
    should(""" val k = 1; val a = List(1,2,3); for(i <- a) 1/(i-k) """)("You will likely divide by zero here.")
    
    should(""" var k = 3; val a = List(1,2,3); if(a.size == k) for(i <- a) 1/i """)("This condition will always hold.")
    should(""" var k = 4; val a = List(1,2,3); if(a.size == k) for(i <- a) 1/i """)("This condition will never hold.")
    should(""" var k = 1; val a = List(k,2,3); for(i <- a) 1/(i-k) """)("You will likely divide by zero here.")
    
    shouldnt(""" val b = if(util.Random.nextBoolean) { if(util.Random.nextBoolean) 0 else 2 } else { 1 }; 1/(b-3) """)("You will likely divide by zero here.")
    should(""" val b = if(util.Random.nextBoolean) { if(util.Random.nextBoolean) 0 else 2 } else { 1 }; 1/(b-2) """)("You will likely divide by zero here.")
    should(""" val b = if(util.Random.nextBoolean) { if(util.Random.nextBoolean) 0 else 2 } else { 1 }; 1/(b-1) """)("You will likely divide by zero here.")
    should(""" val b = if(util.Random.nextBoolean) { if(util.Random.nextBoolean) 0 else 2 } else { 1 }; 1/(b-0) """)("You will likely divide by zero here.")
    shouldnt(""" val b = if(util.Random.nextBoolean) { if(util.Random.nextBoolean) 0 else 2 } else { 1 }; 1/(b+1) """)("You will likely divide by zero here.")
  }
  
  @Test
  def abs_interpretation__String() {
    implicit var msg = ""
    
    //TODO:
    
    msg = "string will never be empty"
    
    should(""" val a = "a "; if(a.isEmpty) "foo" """)
    should(""" val r = "a    b".distinct.tail; if(r.nonEmpty) "foo" """)
    should(""" var b = " "; val a = (b + (if(b == " ") "a" else "b"+b)).trim.toLowerCase; if(a.nonEmpty) "foo" """)
    should(""" val a = util.Random.nextLong + util.Random.nextString(6); if(a.nonEmpty) "fdd" """)
    shouldnt(""" var b = " "; val a = (b + (if(b == " ") " " else " "+b)).trim.toLowerCase; if(a.nonEmpty) "foo" """)

    msg = "string will always be empty"
    
    should(""" val r = " "; if(r.trim.nonEmpty) "foo" """)
    should(""" val r = " ".tail; if(r.nonEmpty) "foo" """)
    should(""" val r = " ".init; if(r.nonEmpty) "foo" """)
    should(""" val r = "a a".init.tail.trim; if(r.nonEmpty) "foo" """)
    should(""" val r = "a a".capitalize.reverse.init.tail.trim; if(r.nonEmpty) "foo" """)
    should(""" val r = "    ".distinct.tail; if(r.nonEmpty) "foo" """)
    shouldnt(""" val r = "a    b".distinct.tail; if(r.nonEmpty) "foo" """)
    should(""" val a = " ".trim; if(a.isEmpty) "foo" """)
    shouldnt(""" val a = " "; if(a.isEmpty) "foo" """)
    should(""" val a = "   ".substring(2,2); if(a.isEmpty) "foo" """)
    
    msg = "return true"
    should(""""fszd".startsWith("f")""")
    shouldnt(""""fszd".startsWith("a")""")
    should(""""fszd".endsWith("zd")""")
    shouldnt(""""fszd".endsWith("a")""")
    should(""""fszd".reverse.endsWith("sf")""")
    should(""""abcd".substring(2,4).endsWith("cd")""")

    msg = "return false"
    shouldnt(""""fszd".startsWith("f")""")
    should(""""fszd".startsWith("a")""")
    shouldnt(""""fszd".endsWith("zd")""")
    should(""""fszd".endsWith("a")""")
    shouldnt(""""fszd".reverse.endsWith("sf")""")

    msg = "IndexOutOfBoundsException"
    should(""""abcd".substring(2,77).endsWith("cd")""")
    should(""""abcd".substring(2,1).endsWith("cd")""")
    should(""" val a = "abcd"; val b = a.substring(2,2).tail """)("Taking the tail of an empty string.")
    should(""""abcd".substring(0,2).charAt(6)""")
    should(""""abcd".substring(-1,2).endsWith("cd")""")
    should(""""abcd".substring(78,89).endsWith("cd")""")
    shouldnt(""""abcd".substring(2,4).endsWith("cd")""")

    should(""""abcd".charAt(22)""")
    shouldnt(""""abcd".charAt(2)""")

    //should(""""abcd"(22)""")
    //shouldnt(""""abcd"(2)""")
    
    msg = "Multiplying a string with a value <= 0 will always result in an empty string."
    //should(""" "dfd"*(-5) """)
    //should(""" val a = 3; "dfd"*(-a) """)
    //shouldnt(""" val a = 3; "dfd"*(a) """)
    
  }
  
  def abs_interpretation__Option() {
    
    should("""List(2).headOption.size < 0""")("will never hold")
    should("""List(2).headOption.size > 1""")("will never hold")
    should("""val a = 55; List(2).headOption.size < a""")("will always hold")
    
    shouldnt("""List(2).headOption.size == 1""")("will never hold")
    shouldnt("""List(2).headOption.size == 1""")("will always hold")
  
    implicit var msg = "Did you mean to take the size of the collection inside the Option?"
    should("""Option(List(2)).size""")
    should("""Option("fdsfd").size""")
    should("""List(List(2)).headOption.size""")
    shouldnt("""List(2).headOption.size""")
    
    msg = "Option of an Option"
    should("""val a = Option(Option("fdsfs"))""")
    shouldnt("""val a = Option("fdsfs")""")
    shouldnt("""val a = Option(List(2))""")

    msg = "Use .isdefined instead of comparing to None"
    should("""val a = Option(5); if(a == None) "foo"""")
    should("""val a = Option(5); if(a != None) "bar"""")
    should("""val a = Option(5); if(a.isDefined) "foo"""")
  }
  
  def abs_interpretation__StringAndInt() {
    implicit var msg = ""
    msg = "string will never be empty"
    should(""" var a = 3; val b = ""+a; if(b.isEmpty) "foo" """)
    shouldnt(""" var a = 3; val b = " "+a; if(b.isEmpty) "foo" """)
    
    msg = "never hold"
    should(""" val a = 3; val b = ""+a; if(b.size != 1) "foo" """)
    should(""" var a = 3; val b = ""+a; if(b.size > 15) "foo" """)
    
    msg = "by zero" //div by zero
    
    should(""" val a = 1/"0".toInt """)
    should(""" val a = 5; if(a == 1/"0".toInt) "foo" """)
    shouldnt(""" val a = 1/"1".toInt """)

    should(""" val a = "abc"; val b = 1/(a.size-3) """)
    should(""" val a = "abc"; val b = a*a.size; 1/(b.size-(a*a.size).size) """)
    shouldnt(""" val a = "abc"; val b = 1/(a.size-1) """)
    shouldnt(""" val a = "abc"; val b = a*a.size; 1/(b.size-(a*a.size).size+1) """)
    
    should("""1/"fszdf".take(0).size""")
    shouldnt("""1/"fszdf".take(1).size""")

    should("""1/"fszdf".drop(10).size""")
    shouldnt("""1/"fszdf".drop(1).size""")
    
    should(""" var a = "5"; val b = a.take(2).tail.tail.size; 1/b """)
    shouldnt(""" var a = "5"; val b = a.take(2).tail.size; 1/b """)
    
    should(""" var b = "" ; val a = b.take(5)+" "; 1/(a.size-1); b.size """)
    should(""" var b = "" ; val a = b.take(5)+" "; 1/(a.size-6); b.size """)
    shouldnt(""" var b = "" ; val a = b.take(5)+" "; 1/(a.size-0); b.size """)
    shouldnt(""" var b = "" ; val a = b.take(5)+" "; 1/(a.size-7); b.size """)

    should(""" val a = 1/0.toString.toInt """)
    shouldnt(""" val a = 1/1.toString.toInt """)

    msg = "String toInt"
    
    should(""" val a = 1/"d0d".toInt """)
    shouldnt(""" val a = 1/"1".toInt """)

    should(""" val a = 1/("0"+0).toInt """)
    shouldnt(""" val a = 1/("1"+0).toInt """)
  }

  @Test
  def abs_interpretation__vartests() {
    //TODO: this should read both as test, and an invitation to improve where the value is actually obvious
    
    should(""" var b = 3; 1/(b-3) """)("divide by zero")
    shouldnt(""" var b = 3; { b = 3; 1+1; }; 1/(b-3) """)("divide by zero") //but could
    shouldnt(""" var b = 3; { b = 4; 1+1; }; 1/(b-3) """)("divide by zero")

    should(""" var b = 3; for(i <- 1 to 10) { b = i; 1/(b-3) } """)("divide by zero")

    should(""" var b = "3"; 1/(b.toInt-3) """)("divide by zero")
    //TODO: in the for loop it doesn't warn even if there is a problem, because range to string is not covered
    shouldnt(""" var b = "0"; for(i <- 1 to 10) { b = i.toString; 1/(b.toInt-333) }; 1/b.toInt """)("divide by zero")
    shouldnt(""" var b = "0"; for(i <- -4 to 0) { b = i.toString; 1/(b.toInt-333) }; 1/b.toInt """)("divide by zero")

    should("""def a = { val a = 0; if (a == 0) 1.0 else 5 / a }""")("This condition will always hold.")
    should("""def a = { val a = 0; def precision = if (a == 0) 1.0 else 5 / a }""")("This condition will always hold.")
    should("""def a = { var a = 0; if (a == 0) 1.0 else 5 / a }""")("This condition will always hold.")
    should("""def a = { var a = 0; def precision = if (a == 0) 1.0 else 5 / a }""")("This condition will always hold.")
    should("""def a = { var a = 0; def precision = { val a = 5; if (a == 0) 1.0 else 5 / a } }""")("This condition will never hold.")
    
    shouldnt(""" def test(a: Int = 0) = if(a > 0) "x" else "y" """)("This condition will never hold.")
  }
  
  @Test
  def regex__syntaxErrors() {
    implicit val msg = "Regex pattern syntax warning"
    
    should("""
      "*+".r
    """)
    should("""
      "[".r
    """)
    should("""
      val a = "*+"
      a.r
    """)
    should("""
      "fsdfds".split("*+")
    """)
    should("""
      "fsdfds".replaceAll("*", "")
    """)
    should("""
      val a = "*"
      java.util.regex.Pattern.compile(a)
    """)
    
    shouldnt("""
      "3*[a]+".r
    """)
    shouldnt("""
      "[a-t]".r
    """)
    shouldnt("""
      val a = "4*a+"
      a.r
    """)
    shouldnt("""
      "3*[a]+".format("hello")
    """)
    shouldnt("""
      val a = "*"
      java.util.regex.Pattern.compile("(pattern)"+a)
    """)
  }
  
  @Test
  @Ignore
  def instanceOf__check() {
    implicit val msg = "Avoid using asInstanceOf"
    
    should("""
      val a = "aa"
      a.replace(a.asInstanceOf[CharSequence], "bb".asInstanceOf[CharSequence])
    """)
    
   shouldnt("""
      val a = "aa"
      a.replace(a: CharSequence, "bb": CharSequence)
    """)
   }
   
  @Test
  def numeric_isNan() {
    implicit val msg = "Use .isNan instead"

    should("""
      var a = Double.NaN
      if(a != a) "foo"
    """)
    should("""
      var a = 4f
      if(a != a) "foo"
    """)
    should("""
      var a = Double.NaN
      if(a == Double.NaN) "foo"
    """)
    should("""
      var a = Double.NaN
      if(a == a) "foo"
    """)

    shouldnt("""
      var a = 4
      if(a != a) "foo"
    """)
  }
  
  @Test
  def numeric_log1p() {
    implicit val msg = "Use math.log1p(x) instead of"// math.log(1 + x) for added accuracy"
    should("""
      val a = 4d
      math.log(1 + a)
    """)
    should("""
      val a = 4d
      math.log(1d + a)
    """)
    shouldnt("""
      val a = 4d
      math.log(2d + a)
    """)
    shouldnt("""
      val a = 4d
      math.log1p(1 + a)
    """)
    
    should("""
      val a = 4d
      math.log(1d + a)
    """)
    should("""
      val a,b = 4d
      math.log((a+b) + 1)
    """)
    shouldnt("""
      val a = 4d
      math.log(a + 2d)
    """)
    shouldnt("""
      val a = 4d
      math.log1p(a + 1)
    """)
  }

  def numeric_exp1m() {
    implicit val msg = "Use math.expm1(x) instead of"// math.exp(x) - 1 for added accuracy (if x is near 1).
    should("""
      val a = 4d
      math.exp(a) - 1
    """)
    should("""
      val a = 4d
      math.exp(a) - 1d
    """)
    shouldnt("""
      val a = 4d
      math.exp(a) - 2d
    """)
    shouldnt("""
      val a = 4d
      math.expm1(a) - 1d
    """)
    
    should("""
      val a = 4d
      -1 + math.exp(a)
    """)
    should("""
      val a,b = 4d
      -1 + math.exp(a + b)
    """)
    shouldnt("""
      val a = 4d
      -2 + math.exp(a)
    """)
    shouldnt("""
      val a = 4d
      -1 + math.exp1m(a)
    """)
  }

  @Test
  def probableBugs__selfAssign() {
    implicit val msg = "Assigning a variable to itself?"

    should("""
      var a = 4d
      println("hi")
      a = a
    """)
    should("""
      class t { 
        var a = 4d
        def k { a = a }
      }
    """)

    shouldnt("""
      var a = 4d
      a += 1
    """)
    shouldnt("""
      var a = 4d
      a = a * a
    """)
  }

  @Test
  def probableBugs__sameElseIfCondition() {
    implicit val msg = "else-if has the same condition"

    should("""
      var a = "b"+util.Random.nextInt
      if(a.size == 5) 
        println("hi")
      else if(a.size == 5) 
        println("hello")
    """)
    should("""
      var a = "b"+util.Random.nextInt
      if(a.size == 5) 
        println("hi")
      else if(a.size == 4) 
        println("hello")
      else if(a.size == 3) 
        println("hell")
      else if(a.size == 4) 
        println("helo")
    """)
    should("""
      var a = 5
      a = util.Random.nextInt
      val b = 
        if(a == 4) 
          println("hi")
        else if(a == 4) 
          println("hello")
    """)

    shouldnt("""
      var a = "b"
      if(a.size == 5) 
        println("hi")
      else if(a.size != 5) 
        println("hello")
    """)
    shouldnt("""
      var a = 5
      val b = 
        if(a == 4) 
          println("hi")
        else if(a == 5) 
          println("hello")
    """)
  }
  
  @Test
  def probableBugs__sameExpression() {
    implicit val msg = /*Same value/expression */"on both sides"

    should("""
      var a = "b"
      if(a.size == a.size) "foo"
    """)("will always hold")
    should("""
      var a = "b"
      val b = (a != a)
    """)
    should("""
      var a = 5
      val b = if(a > 5 && a > 5) "foo" else "bar"
    """)
    should("""
      val a = 5
      val b = 4 + (a-a)
    """)

    shouldnt("""
      var a = "b"
      if(a.size == a.size+1) "foo"
    """)
    shouldnt("""
      var a = "A5"
      val b = (a != a.toLowerCase)
    """)
    shouldnt("""
      var a = 5
      val b = if(a > 5 && a >= 5) "foo" else "bar"
    """)
    shouldnt("""
      var a = 5
      val b = 4 + (a-(a-1))
    """)
    shouldnt("""
      val a = List(1,2,3); a(a.size-3)
    """)
  }
  
  @Test
  def style__YodaConditions() {
    implicit val msg = "Yoda conditions"

    should("""
      var a = util.Random.nextInt
      if(5 == a) "foo"
    """)
    shouldnt("""
      var a = util.Random.nextInt
      if(a == 5) "foo"
    """)
  }
  
  
  @Test
  @Ignore
  def string__constantLength() {
    implicit val msg = "of a constant string"

    should("""
      "5".size
    """)
    should("""
      "5".length
    """)

    shouldnt(""" 
      var a = "5"
      a.size
    """)
    shouldnt(""" 
      var a = "5"
      a.length
    """)
  }

  @Test
  @Ignore
  def string__processingConstant() {
    implicit val msg = "Processing a constant string"
    
    should(""" 
      "aBc".toLowerCase
    """)
    
    shouldnt("""
      val a = "aBc"
      a.toLowerCase
    """)
  }

  @Test
  def if__mergeInner() {
    implicit val msg = "These two ifs can be merged"
    
    should("""
      val a,b = 4
      if(a > 3) {
        if(b == 4) {
          println("foo")
        }
      }
    """)

    shouldnt("""
      val a,b = 4
      if(a > 3) {
        println("bar")
        if(b == 4) {
          println("foo")
        }
      }
    """)
    shouldnt("""
      val a,b = 4
      if(a > 3) {
        if(b == 4) {
          println("foo")
        }
        println("bar")
      }
    """)
    shouldnt("""
      val a,b = 4
      if(a > 3) {
        if(b == 4) {
          println("foo")
        } else {
          println("bar")
        }
      }
    """)
    shouldnt("""
      val a,b = 4
      if(a > 3) {
        if(b == 4) {
          println("foo")
        }
      } else {
        println("bar")
      }
    """)
  }
  
  @Test
  def numeric__signum() {
    implicit val msg = "Did you mean to use the signum function"
    
    should("""
      val a = 4d
      a/math.abs(a)
    """)
    should("""
      val a = 4d
      (a+1)/math.abs(a+1)
    """)
    should("""
      val a = 4d
      a/a.abs
    """)
    should("""
      val a = 4d
      (a+1)/(a+1).abs
    """)
    should("""
      val a = 4d
      (a+1).abs/(a+1)
    """)
    should("""
      val a = 4d
      math.abs(a)/a
    """)

    shouldnt("""
      val a = 4d
      math.abs(a/a)
    """)
    shouldnt("""
      val a = 4d
      a/math.abs(a+1)
    """)
  }

  @Test
  def possibleBugs__assignment() {
    implicit val msg = "Assignment right after declaration"
    
    should("""
      var a = 6
      a = 3
    """)
    //TODO:
    /*should("""
      var a = 6
      println("foo")
      a = 3
    """)*/

    // Most of the real-world cases are like this - use var to compute new value
    shouldnt("""
      var a = "A6"
      a = a.toLowerCase
    """)
    shouldnt("""
      var a = 6
      a += 3
    """)
  }

  @Test
  def possibleBugs__assignment2() {
    implicit val msg = "Two subsequent assigns"
    
    should("""
      var a = 6
      println(a)
      a = 4
      a = 3
    """)

    // Most of the real-world cases are like this - use var to compute new value
    // If fails, look at isUsed first
    shouldnt("""
      var a = "A6"
      println(a)
      a = a.toLowerCase
      a = a + "d"
    """)
  }

  @Test
  def possibleBugs__swapVars() {
    implicit val msg = "Did you mean to swap these two variables"
    
    should("""
      var a = 6
      var b = 7
      println(a+b)
      a = b
      b = a
    """)

    shouldnt("""
      var a = 6
      var b = 7
      println(a+b)
      val c = a
      a = b
      b = c
    """)
  }

  @Test
  @Ignore
  def style__tempVariable() {
    implicit val msg = "You don't need that temp variable"
   
    should("""
      def a = {
        val out = 5 + 5
        out
      }
    """) 

    //TODO: some more cases to cover with val vs var

    shouldnt("""
      def a = {
        var out = 5
        out += 5
        out
      }
    """)
  }
  
  @Test
  def possibleBugs__sameThingTwice() {
    implicit val msg = "You're doing the exact same thing twice."
    
    should("""
      val a = 5
      println(a) 
      println(a) 
    """)
    
    shouldnt("""
      var a = 5
      println(a)
      print(a)
      print(a+1)
    """)
  }
  
  @Test
  def collections__negativeIndex() {
    implicit val msg = "negative index"
    
    should("""
      val a = List(1,2,3)
      a(-1)
    """)
    should("""
      val a = List(1,2,3)
      a(a.size-a.size-1)
    """)
    should("""
      val a = List(1,2,3,4)
      val b = -a.size /* -4 */
      a(a.size/2 + b + 1) /* 2 - 4 + 1 == -1 */
    """)
    should("""
      val a = List(1,2,3)
      a(a.size-a.length-1)
    """)
    should("""
      val a = List(1,2,3)
      val b = "a"
      a(b.size-b.length-1)
    """)
     
    shouldnt("""
      val a = List(1,2,3)
      a(0)
    """)
    shouldnt("""
      val a = List(1,2,3)
      a(a.size-a.size)
    """)
    shouldnt("""
      val a = List(1,2,3,4)
      val b = -a.size + 1 /* -3 */
      a(a.size/2 + b + 1) /* 2 - 3 + 1 == 0 */
    """)
    shouldnt("""
      val a = List(1,2,3)
      a(a.size-a.length)
    """)
    shouldnt("""
      val a = List(1,2,3)
      val b = "a"
      a(b.size-b.length)
    """)
  }
  @Test
  def collections__tooLargeIndex() {
    implicit val msg = "too large index"
    
    should("""
      val a = List(1,2,3)
      a(a.size)
    """)
    should("""
      val a = List(1,2,3)
      a(a.length)
    """)    
    should("""
      val a = List(1,2,3)
      val b = 5
      a(b-1)
    """)
     
    shouldnt("""
      val a = List(1,2,3)
      a(b.size-1)
    """)
    shouldnt("""
      val a = List(1,2,3)
      a(a.length-1)
    """)
    shouldnt("""
      val a = List(1,2,3)
      val b = 5
      a(b-3)
    """)
  }
  
  @Test
  def numeric__BigDecimalFromFloat() {
    implicit val msg = "use a string constant"
    
    should("""BigDecimal(0.55555555555555555555555555555)""")
    should("""new java.math.BigDecimal(0.1)""")
    should("""BigDecimal.valueOf(0.55555555555555555555555555555)""")
    should("""BigDecimal(1.333333333333333333333333333333333333333333333e223)""")
    should("""
      println("hello")
      val a = math.BigDecimal.valueOf(
         -0.12345678901234567890
      )
    """)
    
    shouldnt("""BigDecimal(0.1)""")
    shouldnt("""BigDecimal("0.1")""")
    shouldnt("""new java.math.BigDecimal("0.1")""")
    shouldnt("""BigDecimal.valueOf("0.1")""")
    shouldnt("""math.BigDecimal.valueOf("0.1")""")
  }

  @Test
  def numeric__divisionByZero() {
    implicit val msg = " by zero"
    
    should("""1/0""")
    should("""
      val a = 5
      println(a/(a-5))
    """)
    should(""" val a = 5; if(a == 1/(a-5)) "foo" """)
    should("""
      val a = List(1,2,3)
      println(a.size/(a.size-3))
    """)
    
    shouldnt("""1/2""")
    shouldnt("""
      val a = List(1,2,3)
      println(a.size/(a.size-4))
    """)
  }

  @Test
  def string__nonEmpty() {
    implicit val msg = "string will never be empty"
    
    should("""
      val a = " "
      if(a.nonEmpty) "foo"
    """)
    should("""
      var b = " "
      val a = (b + "," + (if(b == " ") "a" else "b")).trim
      if(a.nonEmpty) "foo"
    """)

    //TODO: add toString to tracked vals - 34.toString is easily known
    shouldnt("""
      val a = ""
      if(a.nonEmpty) "foo"
    """)
  }

  @Test
  def style__find_isDefined_to_exists() {
    implicit val msg = "Use exists(...) instead of find(...).isDefined"
    
    should("""List(1,2,3).find(_ == 2).isDefined""")
    should("""Set(1,2,3).find(_ == 2).isDefined""")
    should("""collection.mutable.HashSet(1,2,3).find(_ == 2).isDefined""")
    should("""Array(1,2,3).find(_ == 2).isDefined""")
    should("""def a(x:Int) = x == 2; List(1,2,3).find(a).isDefined""")

    shouldnt("""List(1,2,3).headOption.isDefined""")
    shouldnt("""List(1,2,3).exists(_ == 2)""")
  }

  @Test
  def style__flatMap_to_filter() {
    implicit val msg = /*filter*/"instead of this flatMap"
    
    should(""" List(1,2,3).flatMap(x => if(x == 2) Some(x) else None) """)
    should(""" List(1,2,3).flatMap(x => if(x == 2) Nil else List(x)) """)

    shouldnt(""" List(1,2,3).flatMap(x => if(x == 2) Some(x+1) else None) """)
    shouldnt(""" List(1,2,3).flatMap(x => if(x == 2) Nil else List(x+1)) """)
  }

  @Test
  def numeric__badAbs() {
    implicit val msg = "Use abs instead of"
    
    should("""math.sqrt(math.pow(15, 2))""")
    should("""math.sqrt(math.pow(15d, 2d))""")
    should("""val a = 14d; math.sqrt(math.pow(a, 2))""")
    should("""val a = 14d; math.sqrt(a*a)""")

    shouldnt("""math.sqrt(math.pow(15, 3))""")
    shouldnt("""val a = 14d; math.sqrt(math.pow(a, 3))""")
    shouldnt("""val a = 14d; math.sqrt(a*(a-1))""")
  }

  @Test
  def style__useUntil() {
    implicit val msg = "Use (low until high) instead of (low to high-1)"
    
    should(""" val a = 5; val b = (1 to a-1) """)
    should(""" val a = 5; for(i <- 1 to a-1) 5 """)
    should(""" val a = List(1,2,3); val b = (1 to a.size-1) """)
    should(""" val a = "fdsfd"; val b = (1 to a.size-1) """)

    shouldnt(""" val a = 5; val b = (a-5 to a-1) """)
    shouldnt(""" val a = 5; val b = (1 to a) """)
    shouldnt(""" val a = 5; for(i <- 1 to a) 5 """)
    shouldnt(""" val a = 5; for(i <- 1 until a-1) 5 """)
    shouldnt(""" val a = 5; for(i <- 1 until a) 5 """)
  }
  
  @Test
  @Ignore
  def def__recursion() {
    implicit val msg = "infinite recursive call"
    
    should("""def k(a:Int):Int = 1 + k(a)""")
    
    shouldnt("""def k(a:Int):Int = 1 + k(a+1)""")
  }
  
  @Test
  def def__constant() {
    implicit val msg = "This method always returns the same value"
    
    should("""def k: Int = { val a = 0; val b = a+2; a+1 }""")
    should("""def l: Int = { val x = 5; if(util.Random.nextBoolean) 4; x }""")
    shouldnt("""def l: Int = { val x = 5; if(util.Random.nextBoolean) return 4; x }""")
    shouldnt("""def l: Int = { val x = 5; if(util.Random.nextBoolean) 4 else 5 }""")
    shouldnt("""def k(x:Int):Int = { val a = 0; val b = a+2; b+x }""")
  }
  
  @Test 
  def trait__unused_sealed() {
    implicit val msg = "This sealed trait is never"
    
    should("""sealed trait Hello""")
    should("""class a { sealed trait Hello; object a { val b = "fdsfds"; class s(); } }""")
    should("""class a { sealed trait Hello; sealed trait Hello2 extends Hello; object a { val b = "fdsfds"; class s(); } }""")
    should("""class a { sealed trait Hello; sealed trait Hello2 extends Hello; object a { val b = "fdsfds"; class s() extends Hello; } }""")
    shouldnt("""class a { sealed trait Hello; sealed trait Hello2 extends Hello; object a { val b = "fdsfds"; class s() extends Hello2; } }""")
  }

  @Test 
  def option__newbieChecks() {
    
    should("""var a: String = null; if(a+"" == null) None else Some(a+"")""")("""Use Option(...), which automatically wraps null to None""")
    shouldnt("""var a: String = null; if(a == null) None else Some(a+"")""")("""Use Option(...), which automatically wraps null to None""")
    should("""var a: String = null; if(a+"" != null) Some(a+"") else None""")("""Use Option(...), which automatically wraps null to None""")
    shouldnt("""var a: String = null; if(a != null) Some(a+"") else None""")("""Use Option(...), which automatically wraps null to None""")
    
    should("""def a: Option[Int] = null""")("""You probably meant None, not null.""")
    should("""val a: Option[Int] = null""")("""You probably meant None, not null.""")
    should("""var a: Option[Int] = Some(6); println("Foo"); a = null""")("""You probably meant None, not null.""")

  }

  @Test 
  def random__checks() {
    should("""util.Random.nextInt(-1)""")("""The parameter of this nextInt might be lower than 1 here.""")
    shouldnt("""util.Random.nextInt(1)""")("""The parameter of this nextInt might be lower than 1 here.""")
    should("""var a = new util.Random; a.nextInt(-1)""")("""The parameter of this nextInt might be lower than 1 here.""")
    shouldnt("""var a = new util.Random; a.nextInt(1)""")("""The parameter of this nextInt might be lower than 1 here.""")
  }
    
  //stuff that doesn't work and I don't know why
  @Test 
  def broken() {

    ///this one works in the console, but doesn't if you put { } around it
    //should("""val a = "abcd"; a.substring(2,2).tail""")("Taking the tail of an empty string.")
    ///this one works in the console, but doesn't in tests
    //should(""" "dfd"*(-5) """)
    
  }

/*
src/main/scala/LinterPlugin.scala:        if(maybeVals.nonEmpty) unit.warning(tree.pos, "[experimental] These vars might secretly be vals: grep -rnP --include=*.scala 'var ([(][^)]*)?("+maybeVals.mkString("|")+")'")
src/main/scala/AbstractInterpretation.scala:      if(neverHold) unit.warning(condExpr.pos, "This condition will never hold.")
src/main/scala/AbstractInterpretation.scala:      if(alwaysHold) unit.warning(condExpr.pos, "This condition will always hold.")
src/main/scala/AbstractInterpretation.scala:        if(this.actualSize == 0) unit.warning(treePosHolder.pos, "Taking the "+head_last.toString+" of an empty collection.")
src/main/scala/AbstractInterpretation.scala:          unit.warning(treePosHolder.pos, "Taking the "+tail_init.toString+" of an empty collection.")
src/main/scala/AbstractInterpretation.scala:        unit.warning(treePosHolder.pos, "This condition will " + (if(this.actualSize == 0) "always" else "never") + " hold.")
src/main/scala/AbstractInterpretation.scala:        unit.warning(treePosHolder.pos, "This condition will " + (if(this.actualSize > 0) "always" else "never") + " hold.")
src/main/scala/AbstractInterpretation.scala:            unit.warning(treePosHolder.pos, "This contains will always return true")
src/main/scala/AbstractInterpretation.scala:            unit.warning(treePosHolder.pos, "This contains will never return true")
src/main/scala/AbstractInterpretation.scala:            unit.warning(treePosHolder.pos, "This max will always return the first value")
src/main/scala/AbstractInterpretation.scala:            unit.warning(treePosHolder.pos, "This max will always return the second value")
src/main/scala/AbstractInterpretation.scala:            unit.warning(treePosHolder.pos, "This max will always return the second value")
src/main/scala/AbstractInterpretation.scala:            unit.warning(treePosHolder.pos, "This max will always return the first value")
src/main/scala/AbstractInterpretation.scala:            unit.warning(treePosHolder.pos, "This min will always return the first value")
src/main/scala/AbstractInterpretation.scala:            unit.warning(treePosHolder.pos, "This min will always return the second value")
src/main/scala/AbstractInterpretation.scala:            unit.warning(treePosHolder.pos, "This min will always return the second value")
src/main/scala/AbstractInterpretation.scala:            unit.warning(treePosHolder.pos, "This min will always return the first value")
src/main/scala/AbstractInterpretation.scala:            unit.warning(treePosHolder.pos, "This take is always unnecessary.")
src/main/scala/AbstractInterpretation.scala:            if(right.getValueForce <= 0) unit.warning(treePosHolder.pos, "This collection will always be empty.")
src/main/scala/AbstractInterpretation.scala:            unit.warning(treePosHolder.pos, "This drop is always unnecessary.")
src/main/scala/AbstractInterpretation.scala:            if(left.actualSize-right.getValueForce <= 0) unit.warning(treePosHolder.pos, "This collection will always be empty.")
src/main/scala/AbstractInterpretation.scala:              unit.warning(treePosHolder.pos, "The parameter of this nextInt might be lower than 1 here.")
src/main/scala/AbstractInterpretation.scala:      if(!isUsed(body, param) && func != "foreach") unit.warning(tree.pos, "Iterator value is not used in the body.")
src/main/scala/AbstractInterpretation.scala:        unit.warning(s.pos, "You have defined that string as a val already, maybe use that?")
src/main/scala/AbstractInterpretation.scala:        if(stringVals contains str) unit.warning(s.pos, "You have defined that string as a val already, maybe use that?")
src/main/scala/AbstractInterpretation.scala:        unit.warning(pos.pos, "You will likely divide by zero here.")
src/main/scala/AbstractInterpretation.scala:        unit.warning(pos.pos, "You will likely use a too large index for a collection here.")
src/main/scala/AbstractInterpretation.scala:        unit.warning(pos.pos, "You will likely use a negative index for a collection here.")
src/main/scala/AbstractInterpretation.scala:            unit.warning(pos.pos, "This function always returns the same value.")

src/main/scala/LinterPlugin.scala:            val warnMsg = "You should close the file stream after use."
src/main/scala/LinterPlugin.scala:            //val warnMsg = "Exact comparison of floating point values is potentially unsafe."
src/main/scala/LinterPlugin.scala:            val warnMsg = "Comparing with == on instances of different types (%s, %s) will probably return false."
src/main/scala/LinterPlugin.scala:            val warnMsg = "%s.contains(%s) will probably return false."
src/main/scala/LinterPlugin.scala:            val warnMsg = "This condition will always be "+a+"."
src/main/scala/LinterPlugin.scala:            val warnMsg = "This part of boolean expression will always be false."
src/main/scala/LinterPlugin.scala:            val warnMsg = "This part of boolean expression will always be true."
src/main/scala/LinterPlugin.scala:            val warnMsg = "Did you mean to use the signum function here? (signum also avoids division by zero)"
*/

}

