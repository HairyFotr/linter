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
  def if__condition() {
    should("""if(1 > 5) 7 else 8""")("This condition will always be false.")
    should("""if(1 < 5) 7 else 8""")("This condition will always be true.")
  }

  @Test
  def case__neigbouringCases() {
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
  def case__constantValue() {
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
  def implicit__returnType() {
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
  def string__duplicatedLiterals() {
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
  def string__alreadyDefined() {
    implicit val msg = "You have defined that string as a val already"
    
    should("""
      |val a = "hh"
      |val b = "hh"
    """)
    //"hh" + "hh" is auto-joined by the compiler, so you can't detect
    should("""
      |val a = "hh"
      |val b = "hh" + a
    """)
    should("""
      |val a = "hh"
      |println("hh")
    """)
    
    shouldnt("""
      |val a = "hh"
      |val b = a + "cde"
    """)
    shouldnt("""
      |var a = "hh"
      |a += "aa"
      |val b = a + "cde"
    """)
    shouldnt("""
      |println("hh")
      |val a = "hh"
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


  @Test
  def probableBugs__selfAssign() {
    implicit val msg = "Assigning a variable to itself?"

    should("""
      |var a = 4d
      |println("hi")
      |a = a
    """)
    should("""
      |class t { 
      |  var a = 4d
      |  def k { a = a }
      |}
    """)

    shouldnt("""
      |var a = 4d
      |a += 1
    """)
    shouldnt("""
      |var a = 4d
      |a = a * a
    """)
  }

  @Test
  def probableBugs__sameElseIfCondition() {
    implicit val msg = "The else-if has the same condition."

    should("""
      |var a = "b"
      |if(a.size == 5) 
      |  println("hi")
      |else if(a.size == 5) 
      |  println("hello")
    """)
    should("""
      |var a = 5
      |val b = 
      |  if(a == 4) 
      |    println("hi")
      |  else if(a == 4) 
      |    println("hello")
    """)

    shouldnt("""
      |var a = "b"
      |if(a.size == 5) 
      |  println("hi")
      |else if(a.size != 5) 
      |  println("hello")
    """)
    shouldnt("""
      |var a = 5
      |val b = 
      |  if(a == 4) 
      |    println("hi")
      |  else if(a == 5) 
      |    println("hello")
    """)
  }
  
  @Test
  def probableBugs__sameExpression() {
    implicit val msg = "Same expression on both sides"

    should("""
      |var a = "b"
      |if(a.size == a.size) "foo"
    """)
    should("""
      |var a = "b"
      |val b = (a != a)
    """)
    should("""
      |var a = 5
      |val b = if(a > 5 && a > 5) "foo" else "bar"
    """)
    should("""{
      |val a = 5
      |val b = 4 + (a-a)
    }""")

    shouldnt("""
      |var a = "b"
      |if(a.size == a.size+1) "foo"
    """)
    shouldnt("""
      |var a = "A5"
      |val b = (a != a.toLowerCase)
    """)
    shouldnt("""
      |var a = 5
      |val b = if(a > 5 && a >= 5) "foo" else "bar"
    """)
    shouldnt("""{
      |var a = 5
      |val b = 4 + (a-(a-1))
    }""")
  }
  
  @Test
  def style__YodaConditions() {
    implicit val msg = "Yoda conditions"

    should("""
      |var a = 5
      |if(5 == a) "foo"
    """)
    shouldnt("""
      |var a = 5
      |if(a == 5) "foo"
    """)
  }
  
  
  @Test
  def string__constantLength() {
    implicit val msg = "of a constant string"

    should("""
      |"5".size
    """)
    should("""
      |"5".length
    """)

    shouldnt(""" 
      |var a = "5"
      |a.size
    """)
    shouldnt(""" 
      |var a = "5"
      |a.length
    """)
  }

  @Test
  def string__processingConstant() {
    implicit val msg = "Processing a constant string"
    
    should(""" 
      |"aBc".toLowerCase
    """)
    
    shouldnt("""
      |val a = "aBc"
      |a.toLowerCase
    """)
  }

  @Test
  def if__mergeInner() {
    implicit val msg = "These two ifs can be merged"
    
    should("""
      |val a,b = 4
      |if(a > 3) {
      |  if(b == 4) {
      |    println("foo")
      |  }
      |}
    """)

    shouldnt("""
      |val a,b = 4
      |if(a > 3) {
      |  println("bar")
      |  if(b == 4) {
      |    println("foo")
      |  }
      |}
    """)
    shouldnt("""
      |val a,b = 4
      |if(a > 3) {
      |  if(b == 4) {
      |    println("foo")
      |  }
      |  println("bar")
      |}
    """)
    shouldnt("""
      |val a,b = 4
      |if(a > 3) {
      |  if(b == 4) {
      |    println("foo")
      |  } else {
      |    println("bar")
      |  }
      |}
    """)
    shouldnt("""
      |val a,b = 4
      |if(a > 3) {
      |  if(b == 4) {
      |    println("foo")
      |  }
      |} else {
      |  println("bar")
      |}
    """)
  }
  
  @Test
  def numeric__signum() {
    implicit val msg = "Did you mean to use the signum function"
    
    should("""
      |val a = 4d
      |a/math.abs(a)
    """)
    should("""
      |val a = 4d
      |(a+1)/math.abs(a+1)
    """)
    should("""
      |val a = 4d
      |a/a.abs
    """)
    should("""
      |val a = 4d
      |(a+1)/(a+1).abs
    """)
    should("""
      |val a = 4d
      |(a+1).abs/(a+1)
    """)
    should("""
      |val a = 4d
      |math.abs(a)/a
    """)

    shouldnt("""
      |val a = 4d
      |math.abs(a/a)
    """)
    shouldnt("""
      |val a = 4d
      |a/math.abs(a+1)
    """)
  }

  @Test
  def possibleBugs__assignment() {
    implicit val msg = "Assignment right after declaration"
    
    should("""
      |var a = 6
      |a = 3
    """)
    //TODO:
    /*should("""
      |var a = 6
      |println("foo")
      |a = 3
    """)*/

    // Most of the real-world cases are like this - use var to compute new value
    shouldnt("""
      |var a = "A6"
      |a = a.toLowerCase
    """)
    shouldnt("""
      |var a = 6
      |a += 3
    """)
  }

  @Test
  def possibleBugs__assignment2() {
    implicit val msg = "Two subsequent assigns"
    
    should("""
      |var a = 6
      |println(a)
      |a = 4
      |a = 3
    """)

    // Most of the real-world cases are like this - use var to compute new value
    shouldnt("""
      |var a = "A6"
      |println(a)
      |a = a.toLowerCase
      |a = a + "d"
    """)
  }

  @Test
  def possibleBugs__swapVars() {
    implicit val msg = "Did you mean to swap these two variables"
    
    should("""
      |var a = 6
      |var b = 7
      |println(a+b)
      |a = b
      |b = a
    """)

    shouldnt("""
      |var a = 6
      |var b = 7
      |println(a+b)
      |val c = a
      |a = b
      |b = c
    """)
  }

  @Test
  def style__tempVariable() {
    implicit val msg = "You don't need that temp variable"
   
    should("""
      |def a = {
      |  val out = 5 + 5
      |  out
      |}
    """) 

    //TODO: some more cases to cover with val vs var

    shouldnt("""
      |def a = {
      |  var out = 5
      |  out += 5
      |  out
      |}
    """)
  }
  
  @Test
  def possibleBugs__sameThingTwice() {
    implicit val msg = "You're doing the exact same thing twice."
    
    should("""
      |val a = 5
      |println(a) 
      |println(a) 
    """)
    
    shouldnt("""
      |var a = 5
      |println(a)
      |print(a)
      |print(a+1)
    """)
  }
  
  @Test
  def collections__negativeIndex() {
    implicit val msg = "negative index"
    
    should("""
      |val a = List(1,2,3)
      |a(-1)
     """)
    should("""
      |val a = List(1,2,3)
      |a(a.size-a.size-1)
     """)
    should("""
      |val a = List(1,2,3,4)
      |val b = -a.size /* -4 */
      |a(a.size/2 + b + 1) /* 2 - 4 + 1 == -1 */
     """)
    should("""{
      |val a = List(1,2,3)
      |val b = "a"
      |a(b.size-b.length-1)
     }""")
     
    shouldnt("""
      |val a = List(1,2,3)
      |a(0)
     """)
    shouldnt("""
      |val a = List(1,2,3)
      |a(a.size-a.size)
     """)
    shouldnt("""
      |val a = List(1,2,3,4)
      |val b = -a.size + 1 /* -3 */
      |a(a.size/2 + b + 1) /* 2 - 3 + 1 == 0 */
     """)
  }
  
  @Test
  def numeric__BigDecimalFromFloat() {
    implicit val msg = "use a string constant"
    
    //TODO: Scala's BigDecimal isn't as bad at accuracy as Java's, but still fails at BigDecimal(0.5555555555555555555555555555555555)
    should("""BigDecimal(0.1)""")
    shouldnt("""BigDecimal("0.1")""")
  }

  @Test
  def numeric__divisionByZero() {
    implicit val msg = " by zero"
    
    should("""1/0""")
    should("""
      |val a = 5
      |println(a/(a-5))
    """)
    should("""
      |val a = List(1,2,3)
      |println(a.size/(a.size-3))
    """)
    
    shouldnt("""1/2""")
    shouldnt("""
      |val a = List(1,2,3)
      |println(a.size/(a.size-4))
    """)
  }

  @Test
  def string__nonEmpty() {
    implicit val msg = "string will never be empty"
    
    should("""
      |{
      |  val a = " "
      |  if(a.nonEmpty) "foo"
      |}
    """)
    should("""
      |{
      |  var b = " "
      |  val a = (b + "," + (if(b == " ") "a" else "b")).trim
      |  if(a.nonEmpty) "foo"
      |}
    """)

    //TODO: add toString to tracked vals - 34.toString is easily known
    shouldnt("""
      |{
      |   val a = ""
      |   if(a.nonEmpty) "foo"
      |}
    """)
  }

  @Test
  def style__find_isDefined() {
    implicit val msg = "Use exists() instead of find().isDefined"
    
    should("""List(1,2,3).find(_ == 2).isDefined""")
    should("""Set(1,2,3).find(_ == 2).isDefined""")
    should("""collection.mutable.HashSet(1,2,3).find(_ == 2).isDefined""")
    should("""Array(1,2,3).find(_ == 2).isDefined""")
    should("""def a(x:Int) = x == 2; List(1,2,3).find(a).isDefined""")

    shouldnt("""List(1,2,3).headOption.isDefined""")
    shouldnt("""List(1,2,3).exists(_ == 2)""")
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

