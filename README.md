# Linter Compiler Plugin

This is a compiler plugin that adds additional lint checks to protect against sharp corners
in the Scala compiler and standard libraries.

It's a work in progress. For an overview of writing compiler plugins, see http://www.scala-lang.org/node/140

## Usage

Add it as a compiler plugin in your project by editing your build.sbt file:

    resolvers += "linter" at "http://hairyfotr.github.io/linteRepo/releases"

    addCompilerPlugin("com.foursquare.lint" %% "linter" % "0.1-SNAPSHOT")

Or, if you're working with a local version:

    scalacOptions += "-Xplugin:<path-to-jar>.jar"

Optionally, run `sbt console` in this project to see it in action.

## Currently supported warnings

### Using `scala.io.Source.fromFile` without closing file
    scala> io.Source.fromFile("README.md").mkString
    <console>:8: warning: You should close the file stream after use.
                  io.Source.fromFile("README.md").mkString
                                                   ^
                                                   
### Implicit methods without an explicit return type
    scala> implicit def int2string(a:Int) = a.toString
    <console>:8: warning: Implicit method int2string needs explicit return type
           implicit def int2string(a:Int) = a.toString
                        ^

### Unused method argument warnings
    scala> def func(b: Int, c: String, d: String) = b+c
    <console>:7: warning: Parameter d is not used in method func
           def func(b: Int, c: String, d: String) = b+c
               ^

### Unsafe `==`
    scala> Nil == None
    <console>:29: warning: Comparing with == on instances of different types (object Nil, object None) will probably return false.
                  Nil == None
                      ^
    scala> val a = 0.1; a == 0.4
    <console>:9: warning: Exact comparison of floating point values is potentially unsafe.
                  a == 0.4
                    ^

### Unsafe `contains`
    scala> List(1, 2, 3).contains("4")
    <console>:29: warning: List[Int].contains(String) will probably return false.
                  List(1, 2, 3).contains("4")
                                ^

### Wildcard import from `scala.collection.JavaConversions`
    scala> import scala.collection.JavaConversions._
    <console>:29: warning: Conversions in scala.collection.JavaConversions._ are dangerous.
           import scala.collection.JavaConversions._
                                   ^

### Any and all wildcard imports
    scala> import scala.collection.mutable._
    <console>:7: warning: Wildcard imports should be avoided. Favor import selector clauses.
           import scala.collection.mutable._
                                   ^

### Calling `Option#get`
    scala> Option(1).get
    <console>:29: warning: Calling .get on Option will throw an exception if the Option is None.
                  Option(1).get
                            ^
                            
### Using `x.asInstanceOf[T]`
    scala> val x = "a".asInstanceOf[CharSequence]
    <console>:7: warning: Avoid using asInstanceOf[T] (use pattern matching, type ascription, etc).
           val x = "a".asInstanceOf[CharSequence]
                       ^

### Repeated string literal
    scala> val e = List("hh","hh","hh","hh","hh","hh")
    <console>:7: warning: String literal "hh" appears multiple times.
           val e = List("hh","hh","hh","hh","hh","hh")
                                       ^

### Literal division by zero
    scala> 100 / (1+1 - 2)
    <console>:8: warning: Literal division by zero.
                  100 / (1+1 - 2)
                      ^
                      
### Using `log(1 + a)` instead of `log1p(1 + a)`
scala> val a = 4d; math.log(1 + a)
<console>:10: warning: Use math.log1p instead of math.log for added accuracy.
        math.log(1 + a)
                ^

### Pattern matching checks
    scala> a match { case 3 => println("hello") case 4 => println("hello") case 5 => println("hello") case _ => println("how low") }
    <console>:10: warning: 3 neighbouring cases will return scala.this.Predef.println("hello"), and should be merged.
            a match { case 3 => println("hello") case 4 => println("hello") case 5 => println("hello") case _ => println("how low") }
                                                                                             ^
    scala> 5 match { case 3 => "hello"; case _ => "hi" }
    <console>:8: warning: Pattern matching on a constant value 5.
                  5 match { case 3 => "hello"; case _ => "hi" }
                    ^
    scala> val a = Option(""); a match { case Some(x) => x case _ => null }
    <console>:10: warning: There are probably better ways of handling an Option (see: http://blog.tmorris.net/posts/scalaoption-cheat-sheet/)
            a match { case Some(x) => x case _ => null }
              ^
    scala> val a = true; a match { case true => 0 case false => 1 }
    <console>:9: warning: This is probably better written as an if statement.
            a match { case true => null  case false => null }
              ^

### If statement checks
    scala> val a,b = 5; if(b > 4) (1+1,a) else (2,a)
    <console>:9: warning: Result will always be scala.Tuple2.apply[Int, Int](2, this.a) regardless of condition.
                  val a,b = 5; if(b > 4) (1+1,a) else (2,a)
                                    ^

    scala> if(a == b && b > 5) true else false
    <console>:9: warning: Remove the if and just use the condition: this.a.==(this.b).&&(this.b.>(5))
            if(a == b && b > 5) true else false
            ^

    scala> if(1 > 5) 7 else 8
    <console>:8: warning: This condition will always be false.
                  if(1 > 5) 7 else 8
                       ^

## Future Work

* Add more warnings
* Add more tests, report false positives
* Pick and choose which warnings you want
* Choose whether they should be warnings or errors
* Improve testing by adding larger samples

### Ideas for new warnings

Feel free to implement these, or add your own ideas. Pull requests welcome!

* Modularize the wildcard import warnings like the "Avoid Star Import" configuration of checkstyle
 (http://checkstyle.sourceforge.net/config_imports.html)
* Require explicit `override` whenever a method is being overwritten
* Expressions spanning multiple lines should be enclosed in parentheses
* Warn on unrestricted catch clauses (`case e => ...`)
* Traversable#head, Traversable#last, Traversable#maxBy
* Warn on shadowing variables, especially those of the same type
* Warn on inexhaustive pattern matching
* Boolean function parameters should be named (`func("arg1", force = true)`)
* Detect vars, that could easily be vals (no assignments)
* Warn about using log(1+x),exp(x)-1 instead of log1p(x),expm(x)
