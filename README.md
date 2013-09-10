# Linter Compiler Plugin [![Build Status](https://travis-ci.org/HairyFotr/linter.png)](https://travis-ci.org/HairyFotr/linter)

Linter is a Scala compiler plugin that adds compile-time checks to help protect against various possible bugs and style problems.

It's currently a work in progress - some parts will need to be rewritten.

But it is usable (and useful), and all forms of feedback are very welcome.

To see it in action, try it out on your code or run `sbt console` in its folder.

## Usage

__Note:__ If you have instructions for another build tool or IDE, or better instructions for current ones, please make a pull request.

### From sbt
Add it as a compiler plugin to your project by editing your build.sbt file:

    resolvers += "linter" at "http://hairyfotr.github.io/linteRepo/releases"

    addCompilerPlugin("com.foursquare.lint" %% "linter" % "0.1-SNAPSHOT")

### Manually
You can download the latest jars here:
[Scala 2.10.2](https://github.com/HairyFotr/linteRepo/blob/gh-pages/releases/com/foursquare/lint/linter_2.10/0.1-SNAPSHOT/linter_2.10-0.1-SNAPSHOT.jar?raw=true), 
[Scala 2.9.3](https://github.com/HairyFotr/linteRepo/blob/gh-pages/releases/com/foursquare/lint/linter_2.9.3/0.1-SNAPSHOT/linter_2.9.3-0.1-SNAPSHOT.jar?raw=true),
[Scala 2.11.0-M4](https://github.com/HairyFotr/linteRepo/blob/gh-pages/releases/com/foursquare/lint/linter_2.11/0.1-SNAPSHOT/linter_2.11-0.1-SNAPSHOT.jar?raw=true) (experimental), 

    terminal:
      scalac -Xplugin:<path-to-linter-jar>.jar ...

    sbt: (in build.sbt)
      scalacOptions += "-Xplugin:<path-to-linter-jar>.jar"
    
    maven: (in pom.xml inside scala-maven-plugin configuration)
      <configuration>
        <args>
          <arg>-Xplugin:<path-to-linter-jar>.jar</arg>
        </args>
      </configuration>

## Currently supported warnings

__Note:__ These are just some examples. Full documentation is in the making.

You can also check the [test code](https://github.com/HairyFotr/linter/blob/master/src/test/scala/LinterPluginTest.scala#L95) for more.

### If checks
#### Repeated condition in an else-if chain
    scala> if(a == 10 || b == 10) 0 else if(a == 20 && b == 10) 1 else 2
    <console>:10: warning: This condition has appeared earlier in the if-else chain, and will never hold here.
                  if(a == 10 || b == 10) 0 else if(a == 20 && b == 10) 1 else 2
                                                                ^

#### Identical branches
    scala> if(b > 4) (2,a) else (2,a)
    <console>:9: warning: If statement branches have the same structure.
                  if(b > 4) (2,a) else (2,a)
                       ^

#### Unnecessary if
    scala> if(a == b) true else false
    <console>:9: warning: Remove the if and just use the condition.
            if(a == b) true else false
            ^

### Pattern matching checks
#### Detect some unreachable cases
    scala> (x,y) match { case (a,5) if a > 5 => 0 case (c,5) if c > 5 => 1 }
    <console>:10: warning: Identical case detected above - this will never match.
                  (x,y) match { case (a,5) if a > 5 => 0 case (c,5) if c > 5 => 1 }
                                                              ^

#### Identical neighbouring cases
    scala> a match { case 3 => "hello" case 4 => "hello" case 5 => "hello" case _ => "how low" }
    <console>:9: warning: 3 neighbouring cases are identical, and could be merged.
                  a match { case 3 => "hello" case 4 => "hello" case 5 => "hello" case _ => "how low" }
                                                                          ^

#### Match better written as if
    scala> bool match { case true => 0 case false => 1 }
    <console>:9: warning: This is probably better written as an if statement.
                  a match { case true => 0 case false => 1 }
                    ^

### Integer checks (some abstract intepretation)
#### Check conditions
    scala> for(i <- 10 to 20) { if(i > 20) "" }
    <console>:8: warning: This condition will never hold.
                  for(i <- 10 to 20) { if(i > 20) "" }
                                            ^
#### Detect division by zero
    scala> for(i <- 1 to 10) { 1/(i-1)  }
    <console>:8: warning: You will likely divide by zero here.
                  for(i <- 1 to 10) { 1/(i-1)  }
                                       ^
#### Detect too large, or negative indices
    scala> { val a = List(1,2,3); for(i <- 1 to 10) { println(a(i)) } }
    <console>:8: warning: You will likely use a too large index for a collection here.
                  { val a = List(1,2,3); for(i <- 1 to 10) { println(a(i)) } }
                                                                      ^

### String checks (some abstract intepretation)
#### Attempt to verify string length conditions
    scala> for(i <- 10 to 20) { if(i.toString.length == 3) "" }
    <console>:8: warning: This condition will never hold.
                  for(i <- 10 to 20) { if(i.toString.length == 3) "" }
                                                            ^
#### Attempt to track the prefix, suffix, and pieces
    scala> { val a = "hello"+util.Random.nextString(10)+"world"+util.Random.nextString(10)+"!"; if(a contains "world") ""; if(a startsWith "hell") "" }
    <console>:8: warning: This contains will always return true.
                  { val a = "hello"+util.Random.nextString(10)+"world"+util.Random.nextString(10)+"!"; if(a contains "world") ""; if(a startsWith "hell") "" }
                                                                                                                     ^
    <console>:8: warning: This startsWith will always return true.
                  { val a = "hello"+util.Random.nextString(10)+"world"+util.Random.nextString(10)+"!"; if(a contains "world") ""; if(a startsWith "hell") "" }
                                                                                                                                                  ^

#### Regex syntax warnings
    scala> str.replaceAll("?", ".")
    <console>:9: warning: Regex pattern syntax error: Dangling meta character '?'
                  str.replaceAll("?", ".")
                                 ^
### Numeric checks
#### Using `log(1 + a)` instead of `log1p(a)`
    scala> math.log(1d + a)
    <console>:9: warning: Use math.log1p(x) instead of math.log(1 + x) for added accuracy when x is near 0
                  math.log(1 + a)
                          ^

#### Loss of precision on BigDecimal
    scala> BigDecimal(0.555555555555555555555555555)
    <console>:8: warning: Possible loss of precision - use a string constant
                  BigDecimal(0.555555555555555555555555555)
                            ^

### Option checks
#### Using Option.size
    scala> val a = Some(List(1,2,3)); if(a.size > 3) ""
    <console>:9: warning: Did you mean to take the size of the collection inside the Option?
            if(a.size > 3) ""
                 ^

#### Using if-else instead of getOrElse
    scala> if(strOption.isDefined) strOption.get else ""
    <console>:9: warning: Use opt.getOrElse(...) instead of if(opt.isDefined) opt.get else ...
                  if(strOption.isDefined) strOption.get else ""
                                          ^

### Collection checks
#### Use exists(...) instead of find(...).isDefined
    scala> List(1,2,3,4).find(x => x % 2 == 0).isDefined
    <console>:8: warning: Use exists(...) instead of find(...).isDefined
                  List(1,2,3,4).find(x => x % 2 == 0).isDefined
                                ^

#### Use filter(...) instead of flatMap(...)
    scala> List(1,2,3,4).flatMap(x => if(x % 2 == 0) List(x) else Nil)
    <console>:8: warning: Use filter(x => condition) instead of flatMap(x => if(condition) ... else ...)
                  List(1,2,3,4).flatMap(x => if(x % 2 == 0) List(x) else Nil)
                                       ^

### Various possible bugs
#### Unused method parameters
    scala> def func(b: Int, c: String, d: String) = { println(b); b+c }
    <console>:7: warning: Parameter d is not used in method func
           def func(b: Int, c: String, d: String) = { println(b); b+c }
               ^

#### Unsafe `contains`
    scala> List(1, 2, 3).contains("4")
    <console>:29: warning: List[Int].contains(String) will probably return false.
                  List(1, 2, 3).contains("4")
                                ^
#### Unsafe `==`
    scala> Nil == None
    <console>:29: warning: Comparing with == on instances of different types (object Nil, object None) will probably return false.
                  Nil == None
                      ^

## Future Work

* Add more checks
* Add more tests, report false positives
* Pick and choose which warnings you want (configuration)
* Choose whether they should be warnings or errors
* Improve testing (larger samples, generated tests, ...)
* Drop Scala 2.9 and check out new stuff such as quasiquotes

### Ideas for new warnings

Feel free to add your own ideas, or implement these. Pull requests welcome!

* Require explicit `override` whenever a method is being overridden
* Expressions spanning multiple lines should be enclosed in parentheses
* Traversable#head, Traversable#last, Traversable#maxBy
* Warn on shadowing variables, especially those of the same type (`var a = 4; { val a = 5 }`)
* Warn on inexhaustive pattern matching or unreachable cases
* Boolean function parameters should be named (`func("arg1", force = true)`)
* Detect vars, that could easily be vals (done in scala 2.11 -Xlint)

Rule lists from other static analysis tools for inspiration:
* ScalaStyle - https://github.com/scalastyle/scalastyle/wiki
* CheckStyle(Java) - http://checkstyle.sourceforge.net/availablechecks.html
* PMD(Java) - http://pmd.sourceforge.net/pmd-5.0.3/rules/index.html
* CodeNarc(Groovy) - http://codenarc.sourceforge.net/codenarc-rule-index.html
* PVS-Studio(C++) - http://www.viva64.com/en/d/
* Coverity(C++) - http://www.slideshare.net/Coverity/static-analysis-primer-22874326 (6,7)
 
### Some resources
* A quick overview of writing compiler plugins: http://www.scala-lang.org/node/140
* Notes and a similar compiler plugin from a while ago: https://github.com/ymasory/alacs/blob/master/dev/resources.md

