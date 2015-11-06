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

package org.psywerx.hairyfotr

import java.io.{ PrintWriter, StringWriter }

import org.junit.{ Ignore, Test }
import org.specs2.matcher.{ JUnitMustMatchers, StandardMatchResults }

import scala.collection.mutable
import scala.io.Source
import scala.tools.nsc.interpreter.{ IMain, Results }
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.{ Properties, Settings }

//TODO:
// * each test should have a positive and a negative case
// * have longer tests, that maybe trigger several checks
// * if it's worth it, error msgs could come from outside
// * handle/test plugin settings (when settings are done)
// * detect the non-compiling tests (they currently pass)

final object Compiler {
  private val settings = new Settings
  val loader = manifest[LinterPlugin].runtimeClass.getClassLoader
  settings.classpath.value = Source.fromURL(loader.getResource("app.class.path")).mkString
  settings.bootclasspath.append(Source.fromURL(loader.getResource("boot.class.path")).mkString)
  //settings.deprecation.value = true // enable detailed deprecation warnings
  //settings.unchecked.value = true // enable detailed unchecked warnings 
  settings.Xwarnfatal.value = true // warnings cause compile failures too
  //settings.stopAfter.value = List("linter-refchecked")
  //if (Properties.versionString contains "2.10") settings.stopAfter.value = List("linter-refchecked") // fails in 2.11

  val stringWriter = new StringWriter()

  // This is deprecated in 2.9.x, but we need to use it for compatibility with 2.8.x
  private val interpreter = new IMain(settings, new PrintWriter(stringWriter)) {
    override protected def newCompiler(settings: Settings, reporter: Reporter) = {
      settings.outputDirs setSingleOutput virtualDirectory
      val compiler = super.newCompiler(settings, reporter)
      val linterPlugin = new LinterPlugin(compiler)
      import scala.language.reflectiveCalls
      for (phase <- linterPlugin.components)
        compiler.asInstanceOf[{def phasesSet: mutable.HashSet[tools.nsc.SubComponent]}].phasesSet += phase
      compiler
    }
  }

  def compileAndLint(code: String): String = {
    stringWriter.getBuffer.delete(0, stringWriter.getBuffer.length)
    val thunked = s"() => { \n$code\n }"
    interpreter.interpret(thunked) match {
      case Results.Success => ""
      case Results.Error => stringWriter.toString
      case Results.Incomplete => throw new Exception("Incomplete code snippet")
    }
  }
}

final class LinterPluginTest extends JUnitMustMatchers with StandardMatchResults {
  // A few hacks to scrap the boilerplate and better pinpoint the failing test
  def should(code: String, not: Boolean = false)(implicit expectedMsg: String): Unit = {
    val unitResult = (expectedMsg, Compiler.compileAndLint(code)) must beLike {
      case (expected, actual) if (not ^ actual.contains(expected)) => ok
      case _ => ko((if (not) "false positive" else "false negative")+":\n" + code + "\n ")
    }
  }
  
  def noLint(code: String): Unit = { val unitResult = Compiler.compileAndLint(code) must be ("") }
  def noWarn(code: String)(implicit expectedMsg: String): Unit = { should(code, not = true)(expectedMsg) }

  /*@Before
  def forceCompilerInit(): Unit = {
    val unitResult = compiler.compileAndLint("1 + 1")
  }*/
  
  @Test
  def readmeExamples(): Unit = {
    val defs = """
    
      val a,b,x,y = util.Random.nextInt
      val bool = util.Random.nextBoolean
      val str = util.Random.nextString(5)
      val strOption = Option(str)
      
    """
  
    should(defs+"""if (a == 10 || b == 10) 0 else if(a == 20 && b == 10) 1 else 2""")("""This condition has appeared earlier in the if-else chain and will never hold here.""")
    should(defs+"""if (b > 4) (2,a) else (2,a)""")("""If statement branches have the same structure.""")
    should(defs+"""if (a == b) true else false""")("""Remove the if expression and use the condition directly.""")
    should(defs+"""(x,y) match { case (a,5) if a > 5 => 0 case (c,5) if c > 5 => 1 }""")("""Identical case condition detected above. This case will never match.""")
    should(defs+"""a match { case 3 => "hello" case 4 => "hello" case 5 => "hello" case _ => "how low" }""")("""Bodies of 3 neighbouring cases are identical and could be merged.""")
    should(defs+"""bool match { case true => 0 case false => 1 }""")("""Pattern matching on Boolean is probably better written as an if statement.""")
    should(defs+"""for (i <- 10 to 20) { if(i > 20) "" }""")("""This condition will never hold.""")
    should(defs+"""for (i <- 1 to 10) { 1/(i-1)  }""")("""Possible division by zero.""")
    should(defs+"""{ val a = List(1,2,3); for (i <- 1 to 10) { println(a(i)) } }""")("""You will likely use a too large index.""")
    should(defs+"""for (i <- 10 to 20) { if (i.toString.length == 3) "" }""")("""This condition will never hold.""")
    should(defs+"""val s = "hello"+util.Random.nextString(10)+"world"+util.Random.nextString(10)+"!"; if(s contains "world") ""; """)("""This contains always returns the same value: true""")
    should(defs+"""val s = "hello"+util.Random.nextString(10)+"world"+util.Random.nextString(10)+"!"; if(s startsWith "hell") ""; """)("""This startsWith always returns the same value: true""")
    should(defs+"""val s = "hello"+util.Random.nextString(10)+"world"+util.Random.nextString(10)+"!"; if(s endsWith "!") ""; """)("""This endsWith always returns the same value: true""")
    should(defs+"""str.replaceAll("?", ".")""")("""Regex pattern syntax error: Dangling meta character '?'""")
    should(defs+"""math.log(1d + a)""")("""Use math.log1p(x), instead of math.log(1 + x) for added accuracy when x is near 0.""")
    should(defs+"""BigDecimal(0.555555555555555555555555555)""")("""Possible loss of precision.""")
    should(defs+"""{val a = Some(List(1,2,3)); if (a.size > 3) ""}""")("""Did you mean to take the size of the collection inside the Option?""")
    should(defs+"""if (strOption.isDefined) strOption.get else "" """)("""Use strOption.getOrElse(...) instead of if (strOption.isDefined) strOption.get else ...""")
    should(defs+"""List(1,2,3,4).find(x => x % 2 == 0).isDefined""")("""Use col.exists(...) instead of col.find(...).isDefined""")
    should(defs+"""List(1,2,3,4).flatMap(x => if (x % 2 == 0) List(x) else Nil)""")("""Use col.filter(x => condition) instead of col.flatMap(x => if (condition) ... else ...).""")
    should(defs+"""def func(b: Int, c: String, d: String) = { println(b); b+c }""")("""Parameter d is not used in method func""")
    //should(defs+"""List(1, 2, 3).contains("4")""")("""List[Int].contains(String) will probably return false because the collection and target element are of different types.""")
    //should(defs+"""Nil == None""")("""Comparing with == on instances of different types (scala.collection.immutable.Nil.type, None.type) will probably return false.""")
    should(defs+"""List(1, 2, 3).contains("4")""")("""will probably return false, since the collection and target element are of unrelated types.""")
    should(defs+"""Nil == None""")("""Comparing with == on instances of unrelated types""")
  }

  @Test
  def UseIfExpression(): Unit = {
    implicit val msg = "Assign the result of the if expression"
    
    should("""var a = 5; if(util.Random.nextBoolean) a = 4 else a = 2""")
    should("""var a = "a"; if(util.Random.nextBoolean) a = "b" else a = "c" """)
    noLint("""var a = 5; if(util.Random.nextBoolean) a = 4 else println("foo")""")
    noLint("""var a = 5; if(util.Random.nextBoolean) println("foo") else a = 4""")
  }
  
  @Test
  def UnnecessaryElseBranch(): Unit = {
    implicit val msg = "This else branch is unnecessary, as the then branch always returns"
    
    should("""
      def test(): Any = { 
        if(util.Random.nextBoolean) { 
          println("foo"); return 5; println("foo2"); 
        } else {
          println("foo3"); println("foo4"); 
        } 
     }""")
    noLint("""
      def test(): Any = { 
        if(util.Random.nextBoolean) { 
          println("foo"); println("foo2"); 
        } else {
          println("foo3"); return 3; println("foo4"); 
        } 
     }""")
    noLint("""
      def test(): Any = { 
        if(util.Random.nextBoolean) { 
          println("foo"); 
          if(util.Random.nextBoolean) return 5 else println("fds")
          println("foo2"); 
        } else {
          println("foo3"); println("foo4"); 
        }
     }""")
  }
  
  @Test
  def NumberInstanceOf(): Unit = {
    implicit val msg = "asInstanceOf"
    
    should("""4.asInstanceOf[Double]""")
    should("""3.4.asInstanceOf[Byte]""")
    should("""val a = 4; a.asInstanceOf[Double]""")
  }
  
  @Test
  def incompleteTest_InvariantCondition(): Unit = {
    implicit val msg = "This condition will"
    
    should("""val (a,b) = (1,2); if(a != b) a else b""")
    should("""val (a,b) = (1,2); if(a == b) a else b""")
    should("""val (a,b) = (1,2); if(a == b) b else a""")
    should("""val (a,b) = (1,2); if(a == b && util.Random.nextInt == 5) a else b""")
    should("""val (a,b) = (1,2); if(a == b || util.Random.nextInt == 5) a else b""")
    should("""val (a,b) = (1,2); if(util.Random.nextInt == 3) 4 else if(a == b) a else b""")
  }
  
  @Test
  def UseHypot(): Unit = {
    implicit val msg = "Use math.hypot"
    
    should("""val x,y = util.Random.nextDouble; math.sqrt(x*x + y*y)""")
    should("""val x,y = util.Random.nextDouble; math.sqrt(x*x + math.pow(y, 2))""")
    should("""val x,y = util.Random.nextDouble; math.sqrt(x*x + math.pow(5, 2))""")
    noLint("""val x,y = util.Random.nextDouble; math.sqrt(x*x + y)""")
    noLint("""val x,y = util.Random.nextDouble; math.sqrt(x + y*y)""")
    
    should("""val x,y = 5f; math.sqrt(x*x + y*y)""")

    should("""val x,y = util.Random.nextDouble; math.sqrt(25 + x*x)""")
    should("""val x,y = util.Random.nextDouble; math.sqrt(x*x + 25)""")
    should("""val x,y = util.Random.nextDouble; math.sqrt(2147395600 + x*x)""")
    noLint("""val x,y = util.Random.nextDouble; math.sqrt(x*x + 24)""")
    noLint("""val x,y = util.Random.nextDouble; math.sqrt(x*x + 26)""")
    noLint("""val x,y = util.Random.nextDouble; math.sqrt(26 + x*x)""")
    noLint("""val x,y = util.Random.nextDouble; math.sqrt(x*x + 2147395601)""")
    noLint("""val x,y = util.Random.nextDouble; math.sqrt(x*x + 2147395599)""") 
  }

  @Test
  def UseCbrt(): Unit = {
    implicit val msg = "Use math.cbrt"
    
    should("""val x = util.Random.nextDouble; math.pow(x, 1/3d)""")
    should("""val x = util.Random.nextDouble; math.pow(x, 1/3f)""")
    should("""val x = util.Random.nextDouble; math.pow(20*x+1, 1/3f)""")
    noLint("""val x = util.Random.nextDouble; math.pow(20*x+1, 0.75)""")
    noLint("""val x = util.Random.nextDouble; math.pow(20*x+1, 0.25)""")
  }
  @Test
  def UseSqrt(): Unit = {
    implicit val msg = "Use math.sqrt"
    
    should("""val x = util.Random.nextDouble; math.pow(x, 1/2d)""")
    should("""val x = util.Random.nextDouble; math.pow(x, 1/2f)""")
    should("""val x = util.Random.nextDouble; math.pow(20*x+1, 0.5)""")
    noLint("""val x = util.Random.nextDouble; math.pow(20*x+1, 0.75)""")
    noLint("""val x = util.Random.nextDouble; math.pow(20*x+1, 0.25)""")
  }
  @Test
  def UseExp(): Unit = {
    implicit val msg = "Use math.exp"
    
    should("""{ val x = util.Random.nextDouble; math.pow(2.718281828459045, 1/x) }""")
    should("""val x = util.Random.nextDouble; math.pow(math.E, x/2f)""")
    should("""val x = util.Random.nextDouble; math.pow(math.E, 20*x+1)""")
    noLint("""val x = util.Random.nextDouble; math.pow(2, 20*x+1)""")
    noLint("""val x = util.Random.nextDouble; math.pow(3, 20*x+1)""")
  }

  @Test
  def UseLog10(): Unit = {
    implicit val msg = "Use math.log10"
    
    should("""val x = util.Random.nextDouble; math.log(x)/math.log(10)""")
    should("""val x = util.Random.nextDouble; math.log(x+2)/math.log(10)""")
    noLint("""val x = util.Random.nextDouble; math.log(x)/math.log(11)""")
    noLint("""val x = util.Random.nextDouble; math.log(x)/math.log(9)""")
  }
  
  @Test
  def PossibleLossOfPrecision(): Unit = {
    {
      implicit val msg = "Literal cannot be represented exactly"
     
      should("""val x = 0.5555555555555555555555555555""")
      should("""val x = 0.5555555555555555555555555555+0.5""")
      noLint("""val x = 0.5""")
      noLint("""val x = 0.5+0.5""")

      should("""val x = 0.555555555f""")
      noLint("""val x = 0.555555555d""")
    }
  }

  @Test
  def UnitImplicitOrdering(): Unit = {
    implicit val msg = "Unit is returned here"
   
    should("""List(1,2,3) maxBy { x => val res = x }""")
    should("""List(1,2,3) minBy { x => println("hello"); val res = x }""")
    should("""(1 to 3) maxBy { x => println("hello"); () }""")
    should("""Array(1,2,3) maxBy { x => println("hello"); () }""")

    noLint("""List(1,2,3) maxBy { x => println("hello"); x }""")
  }
    
  @Test
  def UnsafeAbs(): Unit = {
    implicit val msg = "unsafe use of abs"
   
    should("""math.abs(util.Random.nextInt)""")
    should("""val a = new util.Random; math.abs(a.nextInt)""")
    
    should("""util.Random.nextInt.abs""")
    should("""val a = new util.Random; a.nextInt.abs""")

    should("""util.Random.nextLong.abs""")

    noLint("""math.abs(-10)""")
    noLint("""-10.abs""")
  }
    
  @Test
  def TypeToType(): Unit = {
    implicit val msg = "that is already of type"
   
    should(""" "".toString """)
    noLint(""" "5".toInt """)
    
    should(""" val a = ""; a.toString """)
    noLint(""" val a = 5; a.toString """)

    should("""5.toInt""")
    noLint("""5.toShort""")
    noLint("""5.toLong""")
    noLint("""5.toDouble""")

    should("""5.0.toDouble""")
    noLint("""5.0.toFloat""")

    should("""val a = 5.0; a.toDouble""")

    should("""val a = List(1,2,3); a.toList""")
    should("""val a = Seq(1,2,3); a.toSeq""")
    should("""val a = Set(1,2,3); a.toSet""")
    should("""val a = Vector(1,2,3); a.toVector""")
    noLint("""val a = List(1,2,3); a.toSeq""")
    noLint("""val a = Seq(1,2,3); a.toList""")
    noLint("""val a = Seq(1,2,3); a.toVector""")
    noLint("""val a = collection.mutable.Seq(1,2,3); a.toSeq""")
    noLint("""val a = collection.mutable.Set(1,2,3); a.toSet""")
  }
  
  @Test
  def InvalidStringFormat(): Unit = {
    implicit val msg = "string format will throw"
   
    should(""" { val a = 5; "%s %i".format("a", 1) } """) // wrong formatter %i
    should(""" { val a = "%s %d".format("a") } """)       // not enough params
    should(""" { val a = "%s %  d".format("a", 3) } """)  // two spaces
    noLint(""" { val a = 5; "%s %d".format("a", 1) } """)
    
    should(""" String.format("%s %s %s", "a", "1") """)
    should(""" printf("%s %s %s", "a", "1") """)
    should(""" Console.printf("%s %s %s", "a", "1") """)
    
    should(""" "dafdsfdsa".format(1) """)("percent sign")
    should(""" "dafdsf%dsa".format(1,2) """)("percent sign")
  }
  
  @Test
  def EmptyStringInterpolator(): Unit = {
    implicit val msg = "string interpolation"
    
    should(""" val a = s"wat" """)
    should(""" println(s"wat") """)
    should(""" val a = 5; println(s"wat" + a) """)
    should(""" val a = "5"; val b = s"wat" + a """)
    noLint(""" { val a = 5; s" $a " } """)
  }

  @Test
  def UnlikelyToString(): Unit = {
    implicit val msg = "Using toString on type"
    
    should(""" Array("1").toString """)
    should(""" val a = Array(1,2,3); println(a.toString) """)
    should(""" def x(a: Array[Long]): String = a.toString """)
    noLint(""" val a = Seq(1,2,3); println(a.toString) """)
  } 
  
  @Test
  def UnthrownException(): Unit = {
    implicit val msg = "This exception was likely meant to be thrown here."
    
    should(""" def a: Int = { println(""); new Exception(); 5 } """)
    noLint(""" def a: Int = { println(""); throw new Exception(); 5 } """)
    noLint(""" def a: Int = { println(""); return 4; 5 } """)
  }
  
  @Test
  def SuspiciousMatches(): Unit = {
    implicit val msg = "matches the entire string"
    
    should(""" val a = util.Random.nextString(5) matches "^abc$" """)
    noLint(""" val a = util.Random.nextString(5) matches "abc" """)
    
    should(""" val a = util.Random.nextString(5); val b = a matches (a+"$") """)
    should(""" val a = util.Random.nextString(5); val b = a matches ("fdsf$") """)
    noLint(""" val a = util.Random.nextString(5); val b = a matches ("fdsf\\$") """)
    
    should(""" val a = util.Random.nextString(5); val b = a matches ("^"+a) """)
    noLint(""" val a = util.Random.nextString(5); a matches a """)
    should(""" if(List("") forall { _ matches "^[A-Za-z0-9_]{1,20}$" }) println""")
  }
    
  @Test
  def UseFindNotFilterHead(): Unit = {
    implicit val msg = ".headOption can be replaced by "
    
    should(""" val a = List(1,2,3); a.filter( _ >= 2).headOption """)
    noLint(""" val a = List(1,2,3); a.filter( _ >= 2) """)
  }


  @Test
  def RegexWarning(): Unit = {
    implicit val msg = "Regex pattern"
    
    should(""" "ffasd".replaceFirst("/$", "") """)
    should(""" "ffasd".replaceAll("^/", "") """)
    should(""" "ffasd".replaceFirst("\\.git$", "") """)
    should(""" "ffasd".replaceFirst("^refs/heads/", "") """)

    should(""" "*+".r """)
    should(""" "[".r """)
    should("""
      val a = "*+"
      a.r
    """)
    should(""" "fsdfds".split("*+") """)
    should(""" "fsdfds".replaceAll("*", "") """)
    should("""
      val a = "*"
      java.util.regex.Pattern.compile(a)
    """)
    should(""" "|(|)".r """)
    
    noLint(""" "3*[a]+".r """)
    noLint(""" "[a-t]".r """)
    noLint("""
      val a = "4*a+"
      a.r
    """)
    noLint(""" "3*[a]+%s".format("hello") """)
    noLint("""
      val a = "*"
      java.util.regex.Pattern.compile("(pattern)"+a)
    """)
    
  }
  
  @Test
  def InvariantCondition(): Unit = {
    implicit val msg = "This condition will"
    
    should(""" val a = util.Random.nextInt; if(a > 5 && a < 4) "wat" else "" """)
    should(""" case class A(b: Int); val c = A(util.Random.nextInt); if(c.b > 5 && c.b < 4) "wat" else "" """)
    should(""" val a = util.Random.nextInt; if(a > 5 || a < 6) "wat" else "" """)
    noLint(""" val a = util.Random.nextInt; if(a > 5 && a < 6) "k" else "" """)
    noLint(""" val a = util.Random.nextInt; if(a < 5 && a < 4) "k, wat" else "" """)
    noLint(""" val a = util.Random.nextInt; if(a > 5 && a > 6) "k" else "" """)
    noLint(""" val a = util.Random.nextInt; if(a > 6 && a > 5) "k" else "" """)
    noLint(""" val a = util.Random.nextInt; if(a < 5 || a > 6) "k" else "" """)
    noLint(""" val a = util.Random.nextInt; if(a > 5 || a > 4) "k, wat" else "" """)
    noLint(""" val a = util.Random.nextInt; if(a < 5 || a < 4) "k, wat" else "" """)
    noLint(""" val a = util.Random.nextInt; if(a < 4 || a < 5) "k, wat" else "" """)
    noLint(""" if(util.Random.nextInt > 5 && util.Random.nextInt < 4) "k" else "" """)
  }
  
  @Test
  def IfDoWhile(): Unit = {
    implicit val msg = "do-while"
    
    should(""" 
      val a = util.Random.nextInt;
      if(a > 5) do { 
        println("hello");
        println("world")
      } while(a > 5)
    """)
    noLint(""" 
      val a = util.Random.nextInt;
      if(a > 6) do { 
        println("hello");
        println("world")
      } while(a > 5)
    """)
    // This one's actually about the same, but meh
    noLint(""" 
      val a = util.Random.nextInt;
      if(a > 5) while(a > 5) {
        println("hello");
        println("world")
      }
    """)
  }
  
  @Test
  def TransformNotMap(): Unit = {
    implicit val msg = ".transform"
    
    should(""" 
      var a = collection.mutable.ListBuffer(1,2,3);
      a = a.map{_ + 1}
    """)
    should(""" 
      var a = Array("1","2","3");
      a = a.map{_ + 1}
    """)

    noLint(""" 
      var a = collection.mutable.ListBuffer(1,2,3);
      a = a.map{_ + 1}.filter{_ > 1}
    """)
    //although this one could be flagged too... anything of the form a = a.(...).map
    noLint(""" 
      var a = collection.mutable.ListBuffer(1,2,3);
      a = a.filter{_ > 1}.map{_ + 1}
    """)
    noLint(""" 
      var a = collection.mutable.Set(1,2,3);
      a = a.map{_ + 1}
    """)
    noLint(""" 
      var a = Seq(1,2,3);
      a = a.map{_ + 1}
    """)
    noLint(""" 
      var a = List(1,2,3);
      a = a.map{_ + 1}
    """)
    noLint(""" 
      var a = Vector(1,2,3);
      a = a.map{_ + 1}
    """)
  }
  
  @Test
  def UseExistsNotFilterEmpty(): Unit = {
    implicit val msg = ".exists(...) instead of "
    
    should(""" 
      var a = Seq(1,2,3);
      val b = a.filter{ _ > 1 }.nonEmpty
    """)
    should(""" 
      var a = Seq(1,2,3);
      val b = !a.filter{ _ > 1 }.isEmpty
    """)
    should(""" 
      var a = Set(1,2,3);
      val b = !a.filter{ _ > 1 }.isEmpty
    """)
    should(""" 
      var a = Array(1,2,3);
      val b = !a.filter{ _ > 1 }.isEmpty
    """)
    should(""" 
      var a = Option(1);
      val b = !a.filter{ _ > 1 }.isEmpty
    """)
  }

  @Test
  def UseCountNotFilterLength(): Unit = {
    implicit val msg = "Use a.count(...) instead of a.filter(...)."

    should(""" 
      var a = Seq(1,2,3);
      val b = a.filter{ _ > 1 }.length
    """)
    should(""" 
      var a = Seq(1,2,3);
      val b = a.filter{ _ > 1 }.size
    """)
    should(""" 
      var a = Set(1,2,3);
      val b = a.filter{ _ > 1 }.size
    """)
    should(""" 
      var atte = Array(1,2,3);
      val b = atte.filter{ _ > 1 }.size
    """)("Use col.count(...) instead of col.filter(...).")
    should(""" 
      var aaaa = Array(1,2,3);
      val b = aaaa.filter{ _ > 1 }.size
    """)("Use col.count(...) instead of col.filter(...).")
    should(""" 
      var a = Array(1,2,3);
      val b = a.filter{ _ > 1 }.size
    """)("Use col.count(...) instead of col.filter(...).")
  }
  
  @Test
  def UseExistsNotCountCompare(): Unit = {
    implicit val msg = ".exists(...) instead of "
    
    should(""" 
      var a = Seq(1,2,3);
      val b = a.count{ _ > 1 } > 0
    """)
    should(""" 
      var a = Seq(1,2,3);
      val b = a.count{ _ > 1 } >= 1
    """)
    should(""" 
      var a = Seq(1,2,3);
      val b = a.count{ _ > 1 } != 0
    """)
    should(""" 
      var a = Set(1,2,3);
      val b = a.count{ _ > 1 } != 0
    """)
    should(""" 
      var a = collection.mutable.ListBuffer(1,2,3);
      val b = a.count{ _ > 1 } != 0
    """)
    should(""" 
      var a = Array(1,2,3);
      val b = a.count{ _ > 1 } != 0
    """)
    should(""" 
      var a = Option(1);
      val b = a.count{ _ > 1 } != 0
    """)

    noLint(""" 
      var a = Seq(1,2,3);
      val b = a.count{ _ > 1 } == 0
    """)
    noLint(""" 
      var a = Seq(1,2,3);
      val b = a.count{ _ > 1 } >= 0
    """)
  }
  
  @Test
  def UseContainsNotExistsEquals(): Unit = {
    implicit val msg = " instead of "
    
    should("val b = 5; Set(1,2,3).exists(_ == b) ")
    should("val b = 5; List(1,2,3).exists(a => a == b) ")
    should("val b = 5; Set(1,2,3).exists(a => b == a) ")
    should("""val b = "5"; Set("1","2","3").exists(a => b eq a) """)
    should("Set(1,2,3).exists(a => a == 2) ")
    should("Vector(1,2,3).exists(a => a == 2) ")
    should("Array(1,2,3).exists(a => a == 2) ")
    should("collection.mutable.ListBuffer(1,2,3).exists(a => a == 2) ")
    
    if (Properties.versionString.contains("2.10")) {
      noLint("val b = 5; Option(2).exists(_ == b)")
      noLint("Option(2).exists(_ == 2)")
    } else {
      should("val b = 5; Option(2).exists(_ == b)")
      should("Option(2).exists(_ == 2)")
    }
  }
  
  @Test
  def UseQuantifierFuncNotFold(): Unit = {
    implicit val msg = " can be replaced by "
    
    should(""" val a = List(true, true, false); a.fold(true)((acc, n) => acc && !n) """)
    should(""" val a = List(true, true, false); a.fold(true)((acc, n) => !n && acc) """)
    should(""" val a = List(true, true, false); a./:(true)((acc, n) => acc && !n) """)
    should(""" val a = Array.fill(10)(scala.util.Random.nextInt(20)); a.foldLeft(false)((acc, n) => acc || n > 5) """)
    should(""" val a = Array.fill(10)(scala.util.Random.nextInt(20)); a.foldLeft(false)((acc, n) => n > 5 || acc) """)
    should(""" val a = List(true, true, false); a.reduce((acc, n) => acc && !n) """)
    should(""" val a = List(true, true, false); a.reduce((acc, n) => !n || acc) """)
    should(""" val a = List(true, true, false); a.reduceLeft((acc, n) => !n || acc) """)
    should(""" val a = Set(true, true, false); a.reduceLeft((acc, n) => !n || acc) """)
 
  }
  
  @Test
  def UseFuncNotFold(): Unit = {
    {
      should(""" val a = List(1, 2, 3); a.fold(0)((a, b) => a + b) """)(".sum instead of ")
      should(""" val a = List(1, 2, 3); a./:(0)((a, b) => a + b) """)(".sum instead of ")
      should(""" val a = List(1.0, 2.0, 3.0); a.foldLeft(2.0)((a, b) => b + a) """)(".sum + 2.0 instead of ")
      should(""" val a = Array(1.0, 2.0, 3.0); a.foldLeft(2.0)((a, b) => b + a) """)(".sum + 2.0 instead of ")
      should(""" val a = Set(1.0, 2.0, 3.0); a.foldLeft(2.0)((a, b) => b + a) """)(".sum + 2.0 instead of ")
      noLint(""" val a = List(1.0, 2.0, 3.0); a.foldLeft(2.0)((a, b) => b + (a * 4)) """)
    }
    {
      should(""" val a = List(1, 2, 3); a.fold(1)((a, b) => a * b) """)(".product instead of ")
      should(""" val a = List(1, 2, 3); a./:(1)((a, b) => a * b) """)(".product instead of ")
      should(""" val a = Array(1, 2, 3); a./:(1)((a, b) => a * b) """)(".product instead of ")
      should(""" val a = Set(1, 2, 3); a./:(1)((a, b) => a * b) """)(".product instead of ")
      should(""" val a = List(1.0, 2.0, 3.0); a.foldLeft(2d)((a, b) => b * a) """)(".product * 2.0 instead of ")
    }
  }

  @Test
  def UseFuncNotReduce(): Unit = {
    {
      implicit val msg = ".sum instead of "
      
      should(""" val a = List(1, 2, 3); a.reduce((a, b) => a + b) """)
      should(""" val a = List(1.0, 2.0, 3.0); a.reduceLeft((a, b) => b + a) """)
      should(""" val a = Array(1.0, 2.0, 3.0); a.reduceLeft((a, b) => b + a) """)
      should(""" val a = Set(1.0, 2.0, 3.0); a.reduceLeft((a, b) => b + a) """)
      noLint(""" val a = List(1.0, 2.0, 3.0); a.reduceLeft((a, b) => b + (a * 2)) """)
    }
    {
      implicit val msg = ".product instead of "
      
      should(""" val a = List(1, 2, 3); a.reduce((a, b) => a * b) """)
      should(""" val a = List(1.0, 2.0, 3.0); a.reduceLeft((a, b) => b * a) """)
      should(""" val a = Array(1.0, 2.0, 3.0); a.reduceLeft((a, b) => b * a) """)
      should(""" val a = Set(1.0, 2.0, 3.0); a.reduceLeft((a, b) => b * a) """)
      noLint(""" val a = List(1.0, 2.0, 3.0); a.reduceLeft((a, b) => b * (a * 2)) """)
    }
    {
      implicit val msg = ".min instead of "
      
      should(""" val a = List(1, 2, 3); a.reduce((a, b) => a min b) """)
      should(""" val a = List(1.0, 2.0, 3.0); a.reduceLeft(_ min _) """)
      should(""" val a = Array(1.0, 2.0, 3.0); a.reduceLeft(_ min _) """)
      should(""" val a = Set(1.0, 2.0, 3.0); a.reduceLeft(_ min _) """)
      noLint(""" val a = List(1, 2, 3); a.reduce((a, b) => a min (b + 2)) """)
    }
    {
      implicit val msg = ".max instead of "
      
      should(""" val a = List(1, 2, 3); a.reduce((a, b) => a max b) """)
      should(""" val a = List(1.0, 2.0, 3.0); a.reduceLeft(_ max _) """)
      should(""" val a = Array(1.0, 2.0, 3.0); a.reduceLeft(_ max _) """)
      should(""" val a = Set(1.0, 2.0, 3.0); a.reduceLeft(_ max _) """)
      noLint(""" val a = List(1, 2, 3); a.reduce((a, b) => a max (b / 2)) """)
    }
  }
  
  @Test 
  def CloseSourceFile(): Unit = {
    implicit val msg = "You should close the file stream after use."
     
    should("""scala.io.Source.fromFile("README.md").mkString""")
    //should("""scala.io.Source.fromFile("README.md")""")
    
    noLint("""val a = scala.io.Source.fromFile("README.md"); a.mkString(""); a.close()""")
    noLint("""def fromFile(s: String) = ""; fromFile("aaa").mkString("")""")
  }

  @Test
  def JavaConverters(): Unit = {
    implicit val msg = "Consider using the explicit collection.JavaConverters"
    should("import scala.collection.JavaConversions._;")
  }

  @Test
  def ContainsTypeMismatch(): Unit = {
    implicit val msg = "will probably return false"

    should("""val x = List(4); x.contains("foo")""")

    // Set and Map have type-safe contains methods so we don't want to warn on those. 
    noLint("""val x = Set(scala.util.Random.nextInt); x.contains(3)""")
    noLint("""val x = Map(4 -> 5); x.contains(3)""")
  }

  @Test
  def UseConditionDirectly(): Unit = {
    implicit val msg = "Remove the if expression and use the "
    
    should("""
      val a,b = util.Random.nextInt
      if(a == b && b > 5) 
        true 
      else 
        false""")
    should("""
      val a,b = util.Random.nextInt
      if(a != b && b > 5)
        false
      else
        true""")

    noLint("""
      val a,b = util.Random.nextInt
      if(a != b && b > 5)
        1+1
      else
        true""")
  }

  @Test
  def DuplicateIfBranches(): Unit = {
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
      
    noLint("""
      val a,b = scala.util.Random.nextInt
      if(b > 4)
        (3,a) 
      else if(b > 7)
        (2,a)""") 
  }

  @Test
  def IdenticalCaseBodies(): Unit = {
    implicit val msg = "neighbouring cases are identical"
    should("""
      val a = 7
      a match { 
        case 3 => println("hello") 
        case 4 => println("hello") 
        case 5 => println("hello") 
        case _ => println("how low") 
      }""")
    should("""
      val a = 7
      import scala.annotation.switch
      (a: @switch) match { 
        case 3 => println("hello") 
        case 4 => println("hello") 
        case 5 => println("hello") 
        case _ => println("how low") 
      }""")
    noLint("""
      val a = 7
      a match { 
        case 3 => println("hello1")
        case 4 => println("hello2")
        case 5 => println("hello3")
        case _ => println("how low")
      }""")
    noLint("""
      val a = 7
      a match { 
        case 3 => println("hello") 
        case 4 if util.Random.nextBoolean => println("hello") 
        case 5 if util.Random.nextBoolean => println("hello") 
        case 6 if util.Random.nextBoolean => println("hello") 
        case 7 => println("hello") 
        case _ => println("how low") 
      }""")
    noLint("""
      import scala.concurrent._
      import ExecutionContext.Implicits.global
      import scala.util.{Failure,Success}
      future { 1+1 } andThen { case Success(a) => println("win") case Failure(s) => println("fail") }
    """)
  }

  @Test
  def PatternMatchConstant(): Unit = {
    implicit val msg = "Pattern matching on a constant value"
    
    should("""
      5 match {
        case 3 => "hello"
        case _ => "hi"
      }""")
    
    //TODO: should("""val a = 5; a match { case 3 => "hello"; case _ => "hi" } """)
    
    noLint("""
      val a = 5
      a match {
        case 3 => "hello";
        case _ => "hi"
      }""")
  }

  @Test
  def UnusedParameter(): Unit = {
    implicit val msg = "not used in method"
    
    should("""def func(a:Int, b:Int) = { val c = a+1; c } """)
    should("""def func(a:Int)(implicit b:Int) = { val c = b+1; b }""")

    noLint("""def func(a:Int)(implicit b:Int) = a""")
    noLint("""def func(a:Int) = a""")
    noLint("""def func(a:Int) = {}""")
    noLint("""def func(a:Int) {}""")
    noLint("""def func(a:Int) = ???""")

    should("""
      trait A { def a(b:Int): Traversable[Int] }
      trait C { def a(b: Int): List[Int] = { println; Nil } }
    """)
    noLint("""
      trait A { def a(b:Int): Traversable[Int] }
      trait C extends A { def a(b: Int): List[Int] = { println; Nil } }
    """)
  }

  @Test
  def UseLog1p(): Unit = {
    implicit val msg = "Use math.log1p(x), instead of "// math.log(1 + x) for added accuracy"
    should("""
      val a = 4d
      math.log(1 + a)
    """)
    should("""
      val a = 4d
      math.log(1d + a)
    """)
    noLint("""
      val a = 4d
      math.log(2d + a)
    """)
    noLint("""
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
    noLint("""
      val a = 4d
      math.log(a + 2d)
    """)
    noLint("""
      val a = 4d
      math.log1p(a + 1)
    """)

    should("""
      val a = 4d
      Math.log(1 + a)
    """)
    should("""
      val a = 4d
      StrictMath.log(1 + a)
    """)
  }

  def UseExpm1(): Unit = {
    implicit val msg = "Use math.expm1(x) instead of"// math.exp(x) - 1 for added accuracy (if x is near 1).
    should("""
      val a = 4d
      math.exp(a) - 1
    """)
    should("""
      val a = 4d
      math.exp(a) - 1d
    """)
    noLint("""
      val a = 4d
      math.exp(a) - 2d
    """)
    noLint("""
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
    noLint("""
      val a = 4d
      -2 + math.exp(a)
    """)
    noLint("""
      val a = 4d
      -1 + math.expm1(a)
    """)
  }
  
  @Test
  def DuplicateKeyInMap(): Unit = {
    implicit val msg = "This key has already been defined"
    
    should(""" val a = 5; Map(a -> 5, 2 -> 4, a -> 2) """)
    should(""" val a = 5; collection.mutable.HashMap(a -> 5, 2 -> 4, a -> 2) """)
    should(""" val a = 5; Map((a,5), 2 -> 4, a → 2) """)

    noLint(""" Map(1 -> 2, 2 -> 3, 3 -> 4) """)
    noLint(""" val a = 5; Map(a -> 5, 2 -> 4, (a+1) -> 2) """)
    noLint(""" val a = 5; collection.mutable.HashMap(a -> 5, 2 -> 4, (a-1) -> 2) """)
  }

  @Test
  def UseFlattenNotFilterOption(): Unit = {
    implicit val msg = ".flatten instead of "//.filter(_.isDefined).map(_.get)"
    
    should("""val a = List[Option[String]](Some("a"), None, Some("b")); a.filter(_.isDefined).map(_.get)""")
  }
  
  @Test
  def InefficientUseOfListSize(): Unit = {
    implicit val msg = "Empty instead of comparing to"//slow for lists, etc
    
    should(""" val a = List(1,2,3); println(a.size > 0) """)
    should(""" val a = List(1,2,3); println(a.size == 0) """)
    should(""" val a = List(1,2,3); println(a.size != 0) """)
    should(""" val a = List(1,2,3); if(a.size > 0) "" """)
    should(""" val a = List(1,2,3); if(a.size < 1) "" """)
    should(""" val a = List(1,2,3); if(a.size <= 0) "" """)
    should(""" val a = List(1,2,3); if(a.size >= 1) "" """)

  }
  
  @Test
  def UnnecessaryMethodCall(): Unit = { //TODO: there's more
    implicit val msg = "unnecessary."
  
    should(""" "ffasd".replace(" ", " ") """)
    should(""" "ffasd".replace("ff", "ff") """)
    noLint(""" "ffasd".replace(" ", "") """)
    noLint(""" "ffasd".replace("  ", " ") """)
  }

  @Test
  def UseAbsNotSqrtSquare(): Unit = {
    implicit val msg = "Use math.abs(x) instead of math.sqrt(x^2)"
    
    should("""math.sqrt(math.pow(15, 2))""")
    should("""math.sqrt(math.pow(15d, 2d))""")
    should("""val a = 14d; math.sqrt(math.pow(a, 2))""")
    should("""val a = 14d; math.sqrt(a*a)""")

    noLint("""math.sqrt(math.pow(15, 3))""")
    noLint("""val a = 14d; math.sqrt(math.pow(a, 3))""")
    noLint("""val a = 14d; math.sqrt(a*(a-1))""")
  }
  
  @Test
  def MergeMaps(): Unit = {
    implicit val msg = "Merge these two map operations."
    
    should(""" List(1,2,3).map(_+1).map(_/2) """)
    should(""" List("1","2","3").map(x => x + "hello").map( _ + "hi") """)
    should(""" val a = List("1234","2234","3344"); a.filter(_ contains '2').map(x => x + "hello").map( _ + "hi") """)
  }
  
  @Test
  def FuncFirstThenMap(): Unit = {
    implicit val msg = "first, then map."
    
    should(""" List(1,2,3).map(_+1).take(2) """)
    should(""" List(1,2,3).map(_+1).takeRight(2) """)
    should(""" List(1,2,3).map(x => x-1).drop(2) """)
    should(""" List(1,2,3).map(x => x-1).dropRight(2) """)
    should(""" List(1,2,3).map(_+1).headOption """)
    should(""" List(1,2,3).map(_+1).lastOption """)
    should(""" List(1,2,3).map(_+1).init """)
    should(""" List(1,2,3).map(_+1).tail """)
    should(""" List(1,2,3).map(_+1).slice(0,1) """)
  }
  
  @Test
  def FilterFirstThenSort(): Unit = {
    implicit val msg = "Filter collection first, then sort it."
    
    should(""" List(1,2,3).sortWith((x, y) => x > y).filter(x => x > 1) """)
    should(""" List(1.0,2.0,3.0).sortWith((x, y) => x > y).filterNot(x => x > 1) """)

    should(""" List(1L,2L,3L).sortBy(x => x).filter(x => x > 1) """)
    should(""" List(1,2,3).sortBy(x => x).filterNot(x => x > 1) """)

    should(""" List("1","2","3").sorted.filter(x => x.contains("x")) """)
    should(""" List(1,2,3).sorted.filterNot(x => x > 1) """)
  }
  
  @Test
  def UseMinOrMaxNotSort(): Unit = {
    implicit val msg = " instead of "

    should(""" List(1,2,3).sorted.head """)
    should(""" List(1,2,3).sorted.last """)
    should(""" Array(1,2,3).sorted.last """)

    should(""" List(1,2,3).sortBy(x => x).head """)
    should(""" List(1,2,3).sortBy(x => x).last """)
    should(""" Array(1,2,3).sortBy(x => -x).last """)

  }

  @Test
  def UseMapNotFlatMap(): Unit = {
    implicit val msg = "Use col.map(x => if (...) y else z) instead of col.flatMap(x => if (...) Collection(y) else Collection(z))"
    
    should(""" List(1,2,3).flatMap(x => if(x == 2) List(x) else List(x+1)) """)
    should(""" val col = List(1,2,3); col.flatMap(x => if(x == 2) List(x+1) else List(x)) """)

    noLint(""" List(1,2,3).flatMap(x => if(x == 2) List(x, x) else List(x+1)) """)
    noLint(""" val col = List(1,2,3); col.flatMap(x => if(x == 2) List(x+1, x) else List(x)) """)
  }

  @Test
  def UseFilterNotFlatMap(): Unit = {
    implicit val msg = "Use col.filter(x => condition) instead of col.flatMap(x => if (condition) ... else ...)"
    
    should(""" List(1,2,3).flatMap(x => if(x == 2) Some(x) else None) """)
    should(""" List(1,2,3).flatMap(x => if(x == 2) Nil else List(x)) """)
    should(""" val col = List(1,2,3); col.flatMap(x => if(x == 2) Nil else List(x)) """)

    noLint(""" List(1,2,3).flatMap(x => if(x == 2) Some(x+1) else None) """)
    noLint(""" List(1,2,3).flatMap(x => if(x == 2) Nil else List(x+1)) """)
  }

  @Test
  def UseExistsNotFindIsDefined(): Unit = {
    implicit var msg = ".exists(...) instead of "
    should("""List(1,2,3).find(_ == 2).isDefined""")
    should("""Set(1,2,3).find(_ == 2).isDefined""")
    should("""collection.mutable.HashSet(1,2,3).find(_ == 2).isDefined""")
    should("""Array(1,2,3).find(_ == 2).isDefined""")
    should("""def a(x:Int) = x == 2; List(1,2,3).find(a).isDefined""")
    should("""Seq(1,2,3).find(_ > 1).isDefined""")

    should("""List(1,2,3).find(_ == 2).isEmpty""")
    should("""Set(1,2,3).find(_ == 2).isEmpty""")
    should("""Seq(1,2,3).find(_ == 2).isEmpty""")
    should("""Seq(1,2,3).find(_ == 2).nonEmpty""")

    noLint("""List(1,2,3).headOption.isDefined""")
    noLint("""List(1,2,3).headOption.isEmpty""")
  }
  
  @Test
  def UseExistsNotFilterIsEmpty(): Unit = {
    implicit var msg = ".exists(...) instead of "
    should("""List(1,2,3).filter(_ == 2).isEmpty""")
    should("""Set(1,2,3).filter(_ == 2).isEmpty""")
    should("""Seq(1,2,3).filter(_ == 2).isEmpty""")
    should("""collection.mutable.HashSet(1,2,3).filter(_ == 2).isEmpty""")
    //should("""Array(1,2,3).filter(_ == 2).isEmpty""") // TODO: gets wrapped probably...
    should("""def a(x:Int) = x == 2; List(1,2,3).filter(a).isEmpty""")

    should("""Seq(1,2,3).filter(_ == 2).nonEmpty""")
    should("""Seq(1,2,3).filterNot(_ == 2).isEmpty""")
    should("""Some(1).filter(_ > 1).isDefined""")
  }

  @Test
  def ReflexiveAssignment(): Unit = {
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

    noLint("""
      var a = 4d
      a += 1
    """)
    noLint("""
      var a = 4d
      a = a * a
    """)
  }

  @Test
  def IdenticalIfElseCondition(): Unit = {
    implicit val msg = "condition has appeared earlier"

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

    noLint("""
      var a = "b" * util.Random.nextInt
      if(a.size == 5) 
        println("hi")
      else if(a.size != 5) 
        println("hello")
    """)
    noLint("""
      var a = util.Random.nextInt
      val b = 
        if(a == 4) 
          println("hi")
        else if(a == 5) 
          println("hello")
    """)
    noLint("""
      import util.Random._
      
      if(nextBoolean) 
        println("hi")
      else if(nextBoolean) 
        println("hello")
    """)
  }

  @Test
  def YodaConditions(): Unit = {
    implicit val msg = "Yoda conditions"

    should("""
      var a = util.Random.nextInt
      if(5 == a) "foo"
    """)
    noLint("""
      var a = util.Random.nextInt
      if(a == 5) "foo"
    """)
    noLint("""
      var a = util.Random.nextInt
      if(5 < a && a <= 10) "foo"
    """)

    //TODO: _ is marked as x, so it's ignored...  should(""" "abc".filter(x => 'b' == x) """)
    should(""" "abc".filter(c => 'b' == c) """)
    noLint(""" "abc".filter('b' == _) """)
    noLint(""" import scala.language.postfixOps; "abc".filter('b'==) """)
  }
  
  @Test
  def MergeNestedIfs(): Unit = {
    implicit val msg = "These two nested ifs can be merged"
    
    should("""
      val a,b = 4
      if(a > 3) {
        if(b == 4) {
          println("foo")
        }
      }
    """)

    noLint("""
      val a,b = scala.util.Random.nextInt
      if(a > 3) {
        println("bar")
        if(b == 4) {
          println("foo")
        }
      }
    """)
    noLint("""
      val a,b = scala.util.Random.nextInt
      if(a > 3) {
        if(b == 4) {
          println("foo")
        }
        println("bar")
      }
    """)
    noLint("""
      val a,b = scala.util.Random.nextInt
      if(a > 3) {
        if(b == 4) {
          println("foo")
        } else {
          println("bar")
        }
      }
    """)
    noLint("""
      val a,b = scala.util.Random.nextInt
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
  def UseSignum(): Unit = {
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
    should("""
      val a = 4d
      Math.abs(a)/a
    """)

    noLint("""
      val a = 4d
      math.abs(a/a)
    """)
    noLint("""
      val a = 4d
      a/math.abs(a+1)
    """)
  }

  @Test
  def MalformedSwap(): Unit = {
    implicit val msg = "Did you mean to swap these two variables"
    
    should("""
      var a = 6
      var b = 7
      println(a+b)
      a = b
      b = a
    """)

    noLint("""
      var a = 6
      var b = 7
      println(a+b)
      val c = a
      a = b
      b = c
    """)
  }

  @Test
  def IdenticalCaseConditions(): Unit = {
    implicit val msg = "Identical case condition detected above. This case will never match."
    
    should("""val a = 5; a match { case a if a == 5 => "f" case a if a == 5 => "d" }""")
    noLint("""val a = 5; a match { case a if a == 6 => "f" case a if a == 5 => "d" }""")
    
    should("""val x = 5; (x,8) match { case (a,8) if a == 5 => "f" case (b,8) if b == 5 => "d" }""")
    noLint("""val x = 5; (x,8) match { case (a,8) if a == 5 => "f" case (b,8) if b == 6 => "d" }""")
    noLint("""val x = 5; (x,8) match { case (a,8) if a == 5 => "f" case (b,7) if b == 6 => "d" }""")
  }

  @Test
  def UndesirableTypeInference(): Unit = {
    implicit val msg = "Inferred type"//Any/Nothing. This might not be what you intended."
    
    should("""{ var a = if(3 == 3) 5 else ""; println(a) }""")
    noLint("""{ var a:Any = if(3 == 3) 5 else ""; println(a) }""")
    
    should("""{ var a = List(if(3 == 3) 5 else ""); println(a) }""")
    noLint("""{ var a:List[Any] = List(if(3 == 3) 5 else ""); println(a) }""")

    should("""{ var a = if(3 == 3) throw new Exception() else throw new Error(); println(a) }""")
    noLint("""{ var a:Nothing = if(3 == 3) throw new Exception() else throw new Error(); println(a) }""")
    
    should("""{ var a = List(if(3 == 3) throw new Exception() else throw new Error()); println(a) }""")
    noLint("""{ var a:List[Nothing] = List(if(3 == 3) throw new Exception() else throw new Error()); println(a) }""")

    should("""{ var a = List(1, "2") }""")
    noLint("""{ var a = List[Any](1, "2") }""")
    
    // Issue #24
    should("""{ val base = scala.collection.mutable.Map("review_id" -> "abcd", "review_id2" -> 1) }""")
    noLint("""{ val base = scala.collection.mutable.Map[String, Any]("review_id" -> "abcd") }""")
  }
  
  @Test
  def VariableAssignedUnusedValue(): Unit = {
    implicit val msg = "unused value before"
    
    should("""
      var a = BigDecimal(6)
      a = BigDecimal(16)
    """)
    should("""
      var a = BigDecimal(6)
      println("foo")
      a = BigDecimal(16)
    """)

    noLint("""
      var a = 6L
      a = 3
    """)
    noLint("""
      var a = 6L
      println("foo")
      a = 3
    """)

    noLint("""
      var a = "A6"
      a = a.toLowerCase
    """)
    noLint("""
      var a = 6L
      a += 3
    """)

    should("""
      var a = 6L
      println(a)
      a = 4
      a = 3
    """)

    // Issue #21
    noLint("""
      var a = 0
      
      val x1 = new Runnable() {
        override def run(): Unit = a = 1
      }
      val x2 = new Runnable() {
        override def run(): Unit = a = 2
      }
    """)
    noLint("""
      var a = 0
      
      def x1() {
        a = 1
      }
      def x2() {
        a = 2
      }
    """)


    // TODO: If fails, look at isUsed first
    noLint("""
      var a = "A6"
      println(a)
      a = a.toLowerCase
      a = a + "d"
    """)
  }
  
  @Test
  def IdenticalStatements(): Unit = {
    implicit val msg = "You're doing the exact same thing twice"
    
    should("""
      val a = 5
      println(a) 
      println(a) 
    """)
    
    noLint("""
      var a = 5
      println(a)
      print(a)
      print(a+1)
    """)
  }

  @Test
  def UseIsNanNotSelfComparison(): Unit = {
    implicit val msg = "Use x.isNan instead"

    should("""
      var a = Double.NaN
      if(a != a) "foo"
    """)
    should("""
      var a = 4f
      if(a != a) "foo"
    """)

    noLint("""
      var a = util.Random.nextInt
      if(a != a) "foo" //linter:disable:ReflexiveComparison+InvariantCondition
    """)
  }
  
  @Test
  def UseIsNanNotNanComparison(): Unit = {
    implicit val msg = "Use x.isNan instead"

    should("""
      var a = Double.NaN
      if(a == Double.NaN) "foo"
    """)
    should("""
      var a = Double.NaN
      if(a == a) "foo"
    """)
  }

  @Test
  def UseUntilNotToMinusOne(): Unit = {
    implicit val msg = "Use (low until high) instead of (low to high-1)"
    
    should(""" val a = 5; val b = (1 to a-1) """)
    should(""" val a = 5; for(i <- 1 to a-1) 5 """)
    should(""" val a = List(1,2,3); val b = (1 to a.size-1) """)
    should(""" val a = "fdsfd"; val b = (1 to a.size-1) """)

    noLint(""" val a = 6; val b = (a-5 to a-1) """)
    noLint(""" val a = 5; val b = (1 to a) """)
    noLint(""" val a = 5; for(i <- 1 to a) 5 """)
    noLint(""" val a = 5; for(i <- 1 until a-1) 5 """)
    noLint(""" val a = 5; for(i <- 1 until a) 5 """)
  }

  @Test 
  def UnextendedSealedTrait(): Unit = {
    implicit val msg = "This sealed trait is never"
    
    should("""sealed trait Hello""")
    should("""sealed trait Hello { def a = println("hello") }""")
    should("""class a { sealed trait Hello; object a { val b = "fdsfds"; class s(); } }""")
    should("""class a { sealed trait Hello; sealed trait Hello2 extends Hello; object a { val b = "fdsfds"; class s(); } }""")
    should("""class a { sealed trait Hello; sealed trait Hello2 extends Hello; object a { val b = "fdsfds"; class s() extends Hello; } }""")
    noLint("""class a { sealed trait Hello; sealed trait Hello2 extends Hello; object a { val b = "fdsfds"; class s() extends Hello2; } }""")

    noLint("""sealed trait Hello { def a = println("hello") }; val b = new Hello {}""")
    noLint("""sealed trait Hello[A, B <: List[_]]""")
  }

  @Test
  def PassPartialFunctionDirectly(): Unit = {
    implicit val msg = "You can pass the partial function in directly"
    should("""List(Some(1), None) map { _ match { case Some(x) => x; case None => 10 }}""")
    should("""List(Some(1), None) map { item => item match { case Some(x) => x; case None => 10 }}""")
    should("""List(1,2,3) map { a => a match { case _ => 5 }}""")
    noLint("""List(1,2,3) map { a: AnyVal => a match { case _ => 5 }}""")
    noLint("""List(Some(1), None) map { case Some(x) => x; case None => 10 }""")
    noLint("""for(a <- List(Some(1), None)) a match { case Some(x) => x; case None => 10 }""")
  }

  @Test // see http://scalapuzzlers.com/#pzzlr-001
  def OnceEvaluatedStatementsInBlockReturningFunction(): Unit = {
    implicit val msg = "You're passing a block that returns a function"
    
    should("""List(1, 2).map { println("Hi"); _ + 1 }""")
    noLint("""List(1, 2).map { i => println("Hi"); i + 1 }""")
  }

  @Test
  def IntDivisionAssignedToFloat(): Unit = {
    implicit val msg = "Integer division detected in an expression assigned to a floating point variable."
    
    should("""var a = 5; var b = 5f; println("Ignoring some other warning here... "+b); b = 1/a""")
    noLint("""var a = 5; var b = 5f; println("Ignoring some other warning here... "+b); b = 1/a.toFloat""")
    should("""var a = 5; var b: Float = 1/a""")
    noLint("""var a = 5; var b = 1/a""")
    noLint("""var a = 5; var b = 1/a.toFloat""")
    should("""var a = 5; var b = 1f + 1/a + 1f""")
    noLint("""var a = 5; var b = 1f + 1/a.toDouble + 1f""")
  }

  @Test 
  def InvalidParamToRandomNextInt(): Unit = {
    implicit val msg = """The parameter of this .nextInt might be lower than 1 here."""
    
    should("""util.Random.nextInt(-1)""")
    should("""var a = new util.Random; a.nextInt(-1)""")

    noLint("""util.Random.nextInt(1)""")
    noLint("""var a = new util.Random; a.nextInt(1)""")
  }
  
  @Test 
  def ModuloByOne(): Unit = {
    implicit val msg = """Taking the modulo by one will return zero."""
    
    should("""def f(x: Int) = x % 1""")
    should("""val a = 34; val b = a % 1""")
    should("""val a = 34L; val b = a % 1""")

    noLint("""def f(x: Double) = x % 1""") // Issue #27
    noLint("""val a = 34d; val b = a % 1""") 
  }

  // ^ New tests named after their Warning.scala name ^
  // --------------------------------------------------

  // --------------------------------------------------
  // --------------------------------------------------
  // ----------------- OLD TESTS ----------------------
  // --------------------------------------------------
  // --------------------------------------------------

  /* //commented because they crash if stopAfter is set.
  @Test
  def caseClass__NoWarn() {
    noWarnings("""case class A()""")
    noWarnings("""case class A(a: Float)""")
    noWarnings("""case class A(a: Float*)""")
    noWarnings("""class A(a: Float, b: String)""")
  }*/
  
  @Test
  def UnlikelyEquality(): Unit = {
    implicit val msg = "Comparing with == on instances of unrelated types"//(%s, %s) will probably return false.

    should("Nil == None")
    should("""
      val x: List[Int] = Nil
      val y: List[String] = Nil
      x == y""")//TODO: returns true, not false

    noLint(""" BigInt(2) == 2 """)
    noLint(""" 2 == BigInt(2) //linter:disable:YodaConditions """)
    noLint("""
      val x = BigInt(3)
      val y = 3
      x == y
      y == x""")
    noLint(""" "foo" == "bar" //linter:disable:InvariantReturn """)
    noLint("""
      val x: List[Int] = Nil
      val y: List[Int] = Nil
      x == y""")
    noLint("""
      val x: String = "foo"
      val y: String = "bar"
      x == y //linter:disable:InvariantReturn""")
    noLint("""
      val x: String = "foo"
      x == "bar" //linter:disable:InvariantReturn """)
  }
  
  @Test
  def warningSuppression(): Unit = {
    implicit val msg = "Regex pattern"
    
    should(""" "ffasd".replaceFirst("/$", "") """)
    noLint(""" "ffasd".replaceFirst("/$", "") // linter:ignore RegexWarning """)
    noLint(""" "ffasd".replaceFirst("/$", "") // linter:ignore """)
    noLint(""" "ffasd".replaceFirst("/$", "") // linter:disable:RegexWarning """) // legacy
    noLint(""" "ffasd".replaceFirst("/$", "") // linter:disable """)

    should(""" "ffasd".replaceFirst("/$", ""); val a = 1/0 // linter:ignore RegexWarning """)("division")
    noLint(""" "ffasd".replaceFirst("/$", ""); val a = 1/0 // linter:ignore """)
  }

  @Test //UseOptionGetOrElse, UseOptionOrNull
  def style__if_to_optstuff(): Unit = {
    implicit val msg = " instead of if" //use getOrElse, orNull, ... instead of if
    
    should("""val a = Option("str"); if(a.isDefined) a.get else null""")
    should("""val a = Option("str"); if(!a.isDefined) null else a.get""")
    should("""val a = Option("str"); if(a.isEmpty) null else a.get""")
    should("""val a = Option("str"); if(!a.isEmpty) a.get else null""")
    should("""val a = Option("str"); if(a != None) a.get else null""")
    should("""val a = Option("str"); if(a == None) null else a.get""")
    
    // different value
    noLint("""val a = Option("str"); if(a.isDefined) a.get+1 else null""")
    noLint("""val a = Option("str"); if(!a.isDefined) null else a.get+1""")
    noLint("""val a = Option("str"); if(a.isEmpty) null else a.get+1""")
    noLint("""val a = Option("str"); if(!a.isEmpty) a.get+1 else null""")
    noLint("""val a = Option("str"); if(a != None) a.get+1 else null""")
    noLint("""val a = Option("str"); if(a == None) null else a.get+1""")
    
    // switcheroo
    noLint("""val a = Option("str"); if(a.isDefined) null else a.get""")
    noLint("""val a = Option("str"); if(!a.isDefined) a.get else null""")
    noLint("""val a = Option("str"); if(a.isEmpty) a.get else null""")
    noLint("""val a = Option("str"); if(!a.isEmpty) null else a.get""")
    noLint("""val a = Option("str"); if(a != None) null else a.get""")
    noLint("""val a = Option("str"); if(a == None) a.get else null""") 
  }

  @Test
  @Ignore //disabled check
  def import__wildcard(): Unit = {
    implicit val msg = "Wildcard imports should be avoided. Favor import selector clauses."

    should("import collection._;")
    noLint("import collection.mutable.HashSet;")
    noLint("import collection.mutable.{HashSet, ListBuffer};")
    noLint("import util.Random;")
  }

  @Test
  @Ignore //disabled check
  def null__check(): Unit = {
    implicit val msg = "Using null is considered dangerous."
    
    should("""val a = null""") 
    noLint("""val a = 5""")
  }
  
  @Test
  @Ignore //disabled check
  def if__condition(): Unit = {
    should("""if(1 > 5) 7 else 8""")("This condition will always be false.")
    should("""if(1 < 5) 7 else 8""")("This condition will always be true.")

    noLint("""while(1 < 5) { 7; 8 """)
  }
  
  @Test
  @Ignore //disabled check
  def case__ifStatement(): Unit = {
    implicit val msg = "This is probably better written as an if statement."
    
    should("""
      val a = true;
      a match {
        case true => 0
        case false => 1
      }""")
  }

  @Test
  @Ignore //disabled check
  def case__useMonadic(): Unit = {
    implicit val msg = "There are probably better ways of handling an Option"
    
    should("""
      val a = Option("")
      a match {
        case Some(x) => x
        case _ => null
      }""")
  }

  @Test
  @Ignore //disabled check
  def option__get(): Unit = {
    implicit val msg = "Calling .get on Option will throw an exception if the Option is None."

    should("""Option(10).get""")
    should("""val x: Option[Int] = None ; x.get""")
    should("""val x: Option[Int] = Some(3); x.get""")
    should("""val x = None ; x.get""")
    should("""val x = Some(3) ; x.get""")

    noLint("""Map(1 -> "1", 2 -> "2").get(1)""")
  }

  @Test
  @Ignore //disabled check
  def implicit__returnType(): Unit = {
    implicit val msg = "needs explicit return type"
    
    should("""
      import scala.language.implicitConversions
      implicit def int2string(a: Int) = a.toString
    """)
    
    noLint("""
      import scala.language.implicitConversions
      implicit def int2string(a: Int): String = a.toString
    """)
  }

  @Test
  @Ignore //disabled check
  def string__duplicatedLiterals(): Unit = {
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
    
    noLint("""
      val a = "hh"
      val b = a
      val c = b
      val d = c
      val e = d
    """)
  }
  
  @Test
  @Ignore //disabled check
  def string__alreadyDefined(): Unit = {
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
    
    noLint("""
      val a = "hh"
      val b = a + "cde"
    """)
    noLint("""
      var a = "hh"
      a += "aa"
      val b = a + "cde"
    """)
    noLint("""
      println("hh")
      val a = "hh"
    """)
  }
  
  @Test
  def abs_interpretation__assert(): Unit = {
    implicit var msg = "will never hold"
    
    should("""
      val a = 5
      assert(a == 6)
    """)
    should("""
      val a,b = 5
      assert(a == b+1)
    """)
    noLint("""
      val a = scala.util.Random.nextInt
      assert(a == 5)
    """)
    noLint("""
      val a,b = scala.util.Random.nextInt
      assert(a == b)
    """)

    msg = "will always hold"
    
    noLint("""
      val a = scala.util.Random.nextInt
      assert(a == 6)
    """)
    noLint("""
      val a,b = scala.util.Random.nextInt
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
  def abs_interpretation__listAndCondition(): Unit = {
    //isUsed is the suspect, if this fails inside the if
    should(""" val a = List(1,2,3); if(a.size == 3) for(i <- a) 1/i """)("This condition will always hold.")
    should(""" val a = List(1,2,3); if(a.size > 3) for(i <- a) 1/i """)("This condition will never hold.")
    should(""" val a = List(1,2,3); for(i <- a) 1/(i-1) """)("Possible division by zero.")
    should(""" val k = 3; val a = List(1,2,3); if(a.size == k) for(i <- a) 1/i """)("This condition will always hold.")
    should(""" val k = 4; val a = List(1,2,3); if(a.size == k) for(i <- a) 1/i """)("This condition will never hold.")
    should(""" val k = 1; val a = List(1,2,3); for(i <- a) 1/(i-k) """)("Possible division by zero.")
    
    should(""" var k = 3; val a = List(1,2,3); if(a.size == k) for(i <- a) 1/i """)("This condition will always hold.")
    should(""" var k = 4; val a = List(1,2,3); if(a.size == k) for(i <- a) 1/i """)("This condition will never hold.")
    should(""" var k = 1; val a = List(k,2,3); for(i <- a) 1/(i-k) """)("Possible division by zero.")
    
    noLint(""" val b = if(util.Random.nextBoolean) { if(util.Random.nextBoolean) 0 else 2 } else { 1 }; 1/(b-3) """)
    should(""" val b = if(util.Random.nextBoolean) { if(util.Random.nextBoolean) 0 else 2 } else { 1 }; 1/(b-2) """)("Possible division by zero.")
    should(""" val b = if(util.Random.nextBoolean) { if(util.Random.nextBoolean) 0 else 2 } else { 1 }; 1/(b-1) """)("Possible division by zero.")
    should(""" val b = if(util.Random.nextBoolean) { if(util.Random.nextBoolean) 0 else 2 } else { 1 }; 1/(b-0) """)("Possible division by zero.")
    noLint(""" val b = if(util.Random.nextBoolean) { if(util.Random.nextBoolean) 0 else 2 } else { 1 }; 1/(b+1) """)
    
    should("""for(i <- (1 to 10).map(a => a+1)) 1/(i-2)""")("Possible division by zero.")
    noLint("""for(i <- (1 to 10).map(a => a+2)) 1/(i-2)""")
  }
  
  @Test
  def abs_interpretation__String(): Unit = {
    implicit var msg = ""
    
    //TODO:
    
    msg = "string will never be empty"
    
    should(""" val a = "a "; if(a.isEmpty) "foo" """)
    should(""" val r = "a    b".distinct.tail; if(r.nonEmpty) "foo" """)
    should(""" var b = " "; val a = (b + (if(b == " ") "a" else "b"+b)).trim.toLowerCase; if(a.nonEmpty) "foo" """)
    should(""" val a = util.Random.nextLong + util.Random.nextString(6); if(a.nonEmpty) "fdd" """)
    should(""" val a = " "; if(a.isEmpty) "foo" """)
    noLint(""" var b = " "; val a = (b + (if(util.Random.nextBoolean) " " else " "+b)).trim.toLowerCase; if(a.nonEmpty) "foo" """)

    msg = "string will always be empty"
    
    should(""" val r = " "; if(r.trim.nonEmpty) "foo" """)
    should(""" val r = " ".tail; if(r.nonEmpty) "foo" """)
    should(""" val r = " ".init; if(r.nonEmpty) "foo" """)
    should(""" val r = "a a".init.tail.trim; if(r.nonEmpty) "foo" """)
    should(""" val r = "a a".capitalize.reverse.init.tail.trim; if(r.nonEmpty) "foo" """)
    should(""" val r = "    ".distinct.tail; if(r.nonEmpty) "foo" """)
    should(""" val a = " ".trim; if(a.isEmpty) "foo" """)
    should(""" val a = "   ".substring(2,2); if(a.isEmpty) "foo" """)
    
    msg = "always returns the same value: true"
    should(""" "fszd".startsWith("f")""")
    should(""" "fszd".endsWith("zd")""")
    should(""" "fszd".reverse.endsWith("sf")""")
    should(""" "abcd".substring(2,4).endsWith("cd")""")

    msg = "always returns the same value: false"
    should(""" "fszd".startsWith("a")""")
    should(""" "fszd".endsWith("a")""")
    should(""" "fszd".reverse.endsWith("dd")""")
    should(""" "abcd".substring(2,4).endsWith("ee")""")

    msg = "You will likely use a out of bounds index."
    should(""" "abcd".substring(2,77).endsWith("cd")""")
    should(""" "abcd".substring(2,1).endsWith("cd")""")
    should(""" val a = "abcd"; val b = a.substring(2,2).tail """)("Taking the tail of an empty string.")
    should(""" "abcd".substring(0,2).charAt(6)""")
    should(""" "abcd".substring(-1,2).endsWith("cd")""")
    should(""" "abcd".substring(78,89).endsWith("cd")""")
    noLint(""" "abcd".substring(2,4)""")

    should(""" "abcd".charAt(22)""")
    noLint(""" "abcd".charAt(2)""")

    //should(""" "abcd"(22)""")
    //shouldnt(""" "abcd"(2)""")
    
    msg = "Multiplying a string with a value <= 0 will always result in an empty string."
    //should(""" "dfd"*(-5) """)
    //should(""" val a = 3; "dfd"*(-a) """)
    //shouldnt(""" val a = 3; "dfd"*(a) """)
    
    // prefix/suffix tests
    should("""val b = "bcd"+util.Random.nextString(6)+"cde"; if(b contains "bcd") "" """)("This contains always returns the same value: true")
    should("""val b = "bcd"+util.Random.nextString(6)+"cde"; if(b contains "cde") "" """)("This contains always returns the same value: true")
    should("""val b = "abc"; if(b contains "abcd") "" """)("This contains always returns the same value: false")
    
    should("""val b = "bcd"+util.Random.nextString(6)+"cde"; if(b startsWith "bc") "" """)("This startsWith always returns the same value: true")
    should("""val b = "bcd"+util.Random.nextString(6)+"cde"; if(b startsWith "bcd") "" """)("This startsWith always returns the same value: true")

    should("""val b = "bcd"+util.Random.nextString(6)+"cde"; if(b endsWith "de") "" """)("This endsWith always returns the same value: true")
    should("""val b = "bcd"+util.Random.nextString(6)+"cde"; if(b endsWith "cde") "" """)("This endsWith always returns the same value: true")

    should("""val b = "bcd"+util.Random.nextString(6)+"cde"; if(b.reverse endsWith "cb") "" """)("This endsWith always returns the same value: true")
    should("""val b = "bcd"+util.Random.nextString(6)+"cde"; if(b.toUpperCase endsWith "DE") "" """)("This endsWith always returns the same value: true")
    should("""val b = "bcd"+util.Random.nextString(6)+"CDE"; if(b.toLowerCase endsWith "de") "" """)("This endsWith always returns the same value: true")

    should("""val b = "bcd"+util.Random.nextString(6)+"cde"+util.Random.nextString(6)+"fgh"; if(b startsWith "bcd") "" """)("This startsWith always returns the same value: true")
    should("""val b = "bcd"+util.Random.nextString(6)+"cde"+util.Random.nextString(6)+"fgh"; if(b contains "cde") "" """)("This contains always returns the same value: true")
    should("""val b = "bcd"+util.Random.nextString(6)+"cde"+util.Random.nextString(6)+"fgh"; if(b endsWith "fgh") "" """)("This endsWith always returns the same value: true")

    noLint("""val b = "bcd"+util.Random.nextString(6)+"cde"+util.Random.nextString(6)+"fgh"; if(b startsWith "bcdcde") "" """)
    noLint("""val b = "bcd"+util.Random.nextString(6)+"cde"+util.Random.nextString(6)+"fgh"; if(b contains "dcde") "" """)
    noLint("""val b = "bcd"+util.Random.nextString(6)+"cde"+util.Random.nextString(6)+"fgh"; if(b contains "cdef") "" """)
    noLint("""val b = "bcd"+util.Random.nextString(6)+"cde"+util.Random.nextString(6)+"fgh"; if(b endsWith "cdefgh") "" """)

    should("""val b = "bcd"; if(b == "bcd") "" """)("This equals always returns the same value: true")
    should("""val b = "bcd"+util.Random.nextString(6)+"cde"+util.Random.nextString(6)+"fgh"; if(b == "bcdcdefg") "" """)("This equals always returns the same value: false")
    should("""val b = "bcd"+util.Random.nextString(6)+"cde"+util.Random.nextString(6)+"fgh"; if(b == "cdcdefgh") "" """)("This equals always returns the same value: false")
    should("""val b = "bcd"+util.Random.nextInt; if(b == "bcd") "" """)("This equals always returns the same value: false")
    noLint("""val b = "bcd"+util.Random.nextString(6)+"cde"+util.Random.nextString(6)+"fgh"; if(b == "bcdcdefgh") "" """)

    should("""val b = "bcd"; if(b != "bcd") "" """)("This not equals always returns the same value: false")
    should("""val b = "bcd"; if(b != "bcde") "" """)("This not equals always returns the same value: true")
    should("""val b = "bcd"+util.Random.nextInt; if(b != "bcd") "" """)("This not equals always returns the same value: true")
  }
  
  def abs_interpretation__Option(): Unit = {
    
    should("""List(2).headOption.size < 0""")("will never hold")
    should("""List(2).headOption.size > 1""")("will never hold")
    should("""val a = 55; List(2).headOption.size < a""")("will always hold")
    noLint("""List(2).headOption.size == 1""") //TODO
  
    implicit var msg = "Did you mean to take the size of the collection inside the Option?"
    should("""Option(List(2)).size""")
    should("""List(List(2)).headOption.size""")
    noLint("""List(2).headOption.size""")
    msg = "Did you mean to take the size of the string inside the Option?"
    should("""Option("fdsfd").size""")
    msg = "Using Option.size is not recommended, use Option.isDefined instead"
    should("""val a = Option(4).size""")
    should("""List(2).headOption.size""")
    
    msg = "Option of an Option"
    should("""val a = Option(Option("fdsfs"))""")
    noLint("""val a = Option("fdsfs")""")
    noLint("""val a = Option(List(2))""")

    msg = "Use .isdefined instead of comparing to None"
    should("""val a = Option(5); if(a == None) "foo" """)
    should("""val a = Option(5); if(a != None) "bar" """)
    should("""val a = Option(5); if(a.isDefined) "foo" """)
  }
  
  def abs_interpretation__StringAndInt(): Unit = {
    implicit var msg = ""
    msg = "string will never be empty"
    should(""" var a = 3; val b = ""+a; if(b.isEmpty) "foo" """)
    noLint(""" var a = 3; val b = " "+a; if(b.isEmpty) "foo" """)
    
    msg = "never hold"
    should(""" val a = 3; val b = ""+a; if(b.size != 1) "foo" """)
    should(""" var a = 3; val b = ""+a; if(b.size > 15) "foo" """)
    
    msg = "by zero" //div by zero
    
    should(""" val a = 1/"0".toInt """)
    should(""" val a = 5; if(a == 1/"0".toInt) "foo" """)
    noLint(""" val a = 1/"1".toInt """)

    should(""" val a = "abc"; val b = 1/(a.size-3) """)
    should(""" val a = "abc"; val b = a*a.size; 1/(b.size-(a*a.size).size) """)
    noLint(""" val a = "abc"; val b = 1/(a.size-1) """)
    noLint(""" val a = "abc"; val b = a*a.size; 1/(b.size-(a*a.size).size+1) """)
    
    should("""1/"fszdf".take(0).size""")
    noLint("""1/"fszdf".take(1).size""")

    should("""1/"fszdf".drop(10).size""")
    noLint("""1/"fszdf".drop(1).size""")
    
    should(""" var a = "5"; val b = a.take(2).tail.tail.size; 1/b """)
    noLint(""" var a = "5"; val b = a.take(2).tail.size; 1/b """)
    
    should(""" var b = "" ; val a = b.take(5)+" "; 1/(a.size-1); b.size """)
    should(""" var b = "" ; val a = b.take(5)+" "; 1/(a.size-6); b.size """)
    noLint(""" var b = "" ; val a = b.take(5)+" "; 1/(a.size-0); b.size """)
    noLint(""" var b = "" ; val a = b.take(5)+" "; 1/(a.size-7); b.size """)

    should(""" val a = 1/0.toString.toInt """)
    noLint(""" val a = 1/1.toString.toInt """)

    msg = "String toInt"
    
    should(""" val a = 1/"d0d".toInt """)
    noLint(""" val a = 1/"1".toInt """)

    should(""" val a = 1/("0"+0).toInt """)
    noLint(""" val a = 1/("1"+0).toInt """)
  }

  @Test
  def abs_interpretation__vartests(): Unit = {
    //TODO: this should read both as test, and an invitation to improve where the value is actually obvious
    
    should(""" var b = 3; 1/(b-3) """)("division by zero")
    noLint(""" var b = 3; { b = 3; 1+1; }; 1/(b-3) """) //but could
    noLint(""" var b = 3; { b = 4; 1+1; }; 1/(b-3) """)

    should(""" var b = 3; for(i <- 1 to 10) { b = i; 1/(b-3) } """)("division by zero")

    //TODO: in the for loop it doesn't warn even if there is a problem, because range to string is not covered
    should(""" var b = "3"; 1/(b.toInt-3) """)("division by zero")
    noLint(""" var b = "0"; for(i <- 1 to 10) { b = i.toString; 1/(b.toInt-333) }; 1/b.toInt """)
    noLint(""" var b = "0"; for(i <- -4 to 0) { b = i.toString; 1/(b.toInt-333) }; 1/b.toInt """)

    should("""def a = { val a = 0; if (a == 0) 1.0 else 5 / a }""")("This condition will always hold.")
    should("""def a = { val a = 0; def precision = if (a == 0) 1.0 else 5 / a }""")("This condition will always hold.")
    should("""def a = { var a = 0; if (a == 0) 1.0 else 5 / a }""")("This condition will always hold.")
    should("""def a = { var a = 0; def precision = if (a == 0) 1.0 else 5 / a }""")("This condition will always hold.")
    should("""def a = { var a = 0; def precision = { val a = 5; if (a == 0) 1.0 else 5 / a } }""")("This condition will never hold.")
    
    noLint(""" def test(a: Int = 0) = if(a > 0) "x" else "y" """)
  }
    
  @Test
  @Ignore //disabled check
  def instanceOf__check(): Unit = {
    implicit val msg = "Avoid using asInstanceOf"
    
    should("""
      val a = "aa"
      a.replace(a.asInstanceOf[CharSequence], "bb".asInstanceOf[CharSequence])
    """)
    
    noLint("""
      val a = "aa"
      a.replace(a: CharSequence, "bb": CharSequence)
    """)
  }
    
  @Test
  @Ignore //disabled check
  def probableBugs__sameExpression(): Unit = {
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

    noLint("""
      var a = "b"
      if(a.size == a.size+1) "foo"
    """)
    noLint("""
      var a = "A5"
      val b = (a != a.toLowerCase)
    """)
    noLint("""
      var a = 5
      val b = if(a > 5 && a >= 5) "foo" else "bar"
    """)
    noLint("""
      var a = 5
      val b = 4 + (a-(a-1))
    """)
    noLint("""
      val a = List(1,2,3); a(a.size-3)
    """)
  }
      
  @Test
  @Ignore //disabled check
  def string__constantLength(): Unit = {
    implicit val msg = "of a constant string"

    should("""
      "5".size
    """)
    should("""
      "5".length
    """)

    noLint(""" 
      var a = "5"
      a.size
    """)
    noLint(""" 
      var a = "5"
      a.length
    """)
  }

  @Test
  @Ignore //disabled check
  def string__processingConstant(): Unit = {
    implicit val msg = "Processing a constant string"
    
    should(""" 
      "aBc".toLowerCase
    """)
    
    noLint("""
      val a = "aBc"
      a.toLowerCase
    """)
  }

  @Test
  @Ignore //disabled check
  def style__tempVariable(): Unit = {
    implicit val msg = "You don't need that temp variable"
   
    should("""
      def a = {
        val out = 5 + 5
        out
      }
    """) 

    //TODO: some more cases to cover with val vs var

    noLint("""
      def a = {
        var out = 5
        out += 5
        out
      }
    """)
  }
  
  @Test
  def collections__negativeIndex(): Unit = {
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
     
    noLint("""
      val a = List(1,2,3)
      a(0)
    """)
    noLint("""
      val a = List(1,2,3)
      a(a.size-a.size)
    """)
    noLint("""
      val a = List(1,2,3,4)
      val b = -a.size + 1 /* -3 */
      a(a.size/2 + b + 1) /* 2 - 3 + 1 == 0 */
    """)
    noLint("""
      val a = List(1,2,3)
      a(a.size-a.length)
    """)
    noLint("""
      val a = List(1,2,3)
      val b = "a"
      a(b.size-b.length)
    """)
  }
  
  @Test
  def collections__tooLargeIndex(): Unit = {
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
     
    noLint("""
      val a = List(1,2,3)
      a(a.size-1)
    """)
    noLint("""
      val a = List(1,2,3)
      a(a.length-1)
    """)
    noLint("""
      val a = List(1,2,3)
      val b = 5
      a(b-3)
    """)
  }
  
  @Test
  def numeric__BigDecimal(): Unit = {
    implicit val msg = "Possible loss of precision"
    
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
    should("""BigDecimal.valueOf(0.555555555555555555f)""")
    noLint("""BigDecimal("0.555555555555555555")""")

    should("""BigDecimal(0.5555555555555555555555555555555555555555555555555555555555555)""")
    should("""BigDecimal(-0.5555555555555555555555555555555555555555555555555555555555555)""")
    should("""BigDecimal("1.333333333333333333333333333333333333333333333e223")""")
    should("""BigDecimal("1.33333333333333333333333333", new java.math.MathContext(5))""")
    should("""BigDecimal("-1.33333333333333333333333333", new java.math.MathContext(5))""")
    noLint("""BigDecimal("1.33333333333333333333333333", new java.math.MathContext(50))""")
    
    noLint("""BigDecimal(0.1)""")
    noLint("""BigDecimal("0.1")""")
    noLint("""new java.math.BigDecimal("0.1")""")
    noLint("""BigDecimal("0.1")""")
    noLint("""BigDecimal.valueOf(0.1)""")
    noLint("""math.BigDecimal.valueOf(0.1)""")
    noLint("""math.BigDecimal("0.1")""")
    
    should("""BigDecimal("afdsfsdafd")""")("""NumberFormatException""")
  }

  @Test
  def numeric__divisionByZero(): Unit = {
    implicit val msg = " by zero"
    
    should("""1/0""")
    should("""1/0L""")
    //should("""1/0f""") //parser makes it Infinity
    //should("""1/0d""") 
    should("""val a = 1; a/0""")
    should("""val a = 1; a/0d""")
    should("""val a = 1; a/0f""")

    noLint("""class x { def /(d: Int): Int = d }; val b = new x; b/0""")
    
    noLint("""1/2""")

    should("""
      val a = 5
      println(a/(a-5))
    """)
    should(""" val a = 5; if(a == 1/(a-5)) "foo" """)
    should("""
      val a = List(1,2,3)
      println(a.size/(a.size-3))
    """)
    
    noLint("""
      val a = List(1,2,3)
      println(a.size/(a.size-4))
    """)
  }

  @Test
  def string__nonEmpty(): Unit = {
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
  }

  @Test
  @Ignore //disabled check
  def def__recursion(): Unit = {
    implicit val msg = "infinite recursive call"
    
    should("""def k(a:Int):Int = 1 + k(a)""")
    noLint("""def k(a:Int):Int = 1 + k(a+1)""")
  }
  
  @Test
  def def__constant(): Unit = {
    implicit val msg = "This method always returns the same value"
    
    should("""def k: Int = { val a = 0; val b = a+2; a+1 }""")
    should("""def l: Int = { val x = 5; if(util.Random.nextBoolean) 4; x }""")
    noLint("""def l: Int = { val x = 5; if(util.Random.nextBoolean) return 4; x }""")
    noLint("""def l: Int = { val x = 5; if(util.Random.nextBoolean) 4 else 5 }""")
    noLint("""def k(x:Int):Int = { val a = 0; val b = a+2; b+x }""")
  }
  
  @Test 
  def optionChecks(): Unit = {
    
    should("""var a: String = null; if(a+"" == null) None else Some(a+"")""")("""Use Option(...), which automatically wraps null to None""")
    should("""var a: String = null; if(a+"" != null) Some(a+"") else None""")("""Use Option(...), which automatically wraps null to None""")
    
    should("""def a: Option[Int] = null""")("""You probably meant None, not null.""")
    should("""val a: Option[Int] = null""")("""You probably meant None, not null.""")
    should("""var a: Option[Int] = Some(6); println("Foo"); a = null""")("""You probably meant None, not null.""")

  }

  @Test
  def absInterpreter(): Unit = {
    should("""{ val a = 5; { val a = 6; if(a == 6) "" } }""")("This condition will always hold.")
    should("""{ val a = 5; { val a = 6; if(a == 5) "" } }""")("This condition will never hold.")
    noLint("""{ val a = 5; { val a = scala.util.Random.nextInt; if(a == 5) "" } }""")
    
    should("""{ val a = 5; if(a.toString.size == 20) "" }""")("This condition will never hold.")
    should("""{ val a = "5"; if(a.toInt.toString.size <= 0) "" }""")("This condition will never hold.")

    //should("""object o1 { val a = 5 }; object o2 { def d = { if(a == 6) "" }; val a = 6 }""")("This condition will never hold.")
    should("""for(i <- 1 to 10) { for(j <- 1 to i-1) { }}""")("Use (low until high) instead of (low to high-1)")
  }

  //stuff that doesn't work and I don't know why
  @Test 
  def broken(): Unit = {

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
src/main/scala/AbstractInterpretation.scala:            unit.warning(treePosHolder.pos, "This contains always returns the same value: true")
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
src/main/scala/AbstractInterpretation.scala:        unit.warning(pos.pos, "Possible division by zero.")
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

