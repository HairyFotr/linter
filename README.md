# Linter Compiler Plugin [![Build Status](https://travis-ci.org/HairyFotr/linter.png)](https://travis-ci.org/HairyFotr/linter) [![Donate](https://hairyfotr.github.io/linteRepo/donate.png)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=XB9RLMJDYA7DJ)

Linter is a Scala static analysis compiler plugin which adds compile-time checks for various possible bugs, inefficiencies, and style problems.

## Usage from sbt
Add Linter to your project by appending this line to your `build.sbt`:

    addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.12")

If you would always like to have the latest changes, snapshots are also available:

    resolvers += Resolver.sonatypeRepo("snapshots")

    addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1-SNAPSHOT")


## Manual usage
Another possible way to use Linter is to manually download and use these jars:<br>
[Scala 2.11.x](https://oss.sonatype.org/content/repositories/snapshots/org/psywerx/hairyfotr/linter_2.11/0.1-SNAPSHOT/linter_2.11-0.1-SNAPSHOT.jar), <br>
[Scala 2.10.x](https://oss.sonatype.org/content/repositories/snapshots/org/psywerx/hairyfotr/linter_2.10/0.1-SNAPSHOT/linter_2.10-0.1-SNAPSHOT.jar), <br>
[Scala 2.9.3](https://github.com/HairyFotr/linteRepo/blob/gh-pages/releases/com/foursquare/lint/linter_2.9.3/0.1-SNAPSHOT/linter_2.9.3-0.1-SNAPSHOT.jar?raw=true) (outdated)


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

__Note:__ If you have instructions for another build tool or IDE, please make a pull request.

## Enabling/Disabling checks

Checks can be disabled using a plus-separated list of check names:

    scalacOptions += "-P:linter:disable:UseHypot+CloseSourceFile+OptionOfOption"

Or only specific checks can be enabled using:

    scalacOptions += "-P:linter:enable-only:UseHypot+CloseSourceFile+OptionOfOption"

## Suppressing false positives

If you believe some warnings are false positives, you can ignore them with a code comment:
```scala
scala> val x = math.pow(5, 1/3d) + 1/0 // linter:ignore UseCbrt,DivideByZero // ignores UseCbrt and DivideByZero
<console>:8: warning: Integer division detected in an expression assigned to a floating point variable.
              math.pow(5, 1/3d) + 1/0 // linter:ignore UseCbrt,DivideByZero // ignores UseCbrt and DivideByZero
                                ^
scala> val x = math.pow(5, 1/3d) + 1/0 // linter:ignore // ignores all warnings
```
    
__Note:__ Please consider reporting false positives so that they can be removed in future versions.

## List of implemented checks

[UnextendedSealedTrait](src/test/scala/LinterPluginTest.scala#L1533), [UnlikelyEquality](src/test/scala/LinterPluginTest.scala#L1621), [UseLog1p](src/test/scala/LinterPluginTest.scala#L887), [UseLog10](src/test/scala/LinterPluginTest.scala#L246), [UseExpm1](src/test/scala/LinterPluginTest.scala#L933), [UseHypot](src/test/scala/LinterPluginTest.scala#L193), [UseCbrt](src/test/scala/LinterPluginTest.scala#L215), [UseSqrt](src/test/scala/LinterPluginTest.scala#L225), [UseExp](src/test/scala/LinterPluginTest.scala#L235), [UseAbsNotSqrtSquare](src/test/scala/LinterPluginTest.scala#L1015), [UseIsNanNotSelfComparison](src/test/scala/LinterPluginTest.scala#L1484), [UseIsNanNotNanComparison](src/test/scala/LinterPluginTest.scala#L1503), [UseSignum](src/test/scala/LinterPluginTest.scala#L1297), BigDecimalNumberFormat, BigDecimalPrecisionLoss, [ReflexiveAssignment](src/test/scala/LinterPluginTest.scala#L1138), [CloseSourceFile](src/test/scala/LinterPluginTest.scala#L721), [JavaConverters](src/test/scala/LinterPluginTest.scala#L732), [ContainsTypeMismatch](src/test/scala/LinterPluginTest.scala#L738), [NumberInstanceOf](src/test/scala/LinterPluginTest.scala#L172), [PatternMatchConstant](src/test/scala/LinterPluginTest.scala#L844), PreferIfToBooleanMatch, [IdenticalCaseBodies](src/test/scala/LinterPluginTest.scala#L798), [IdenticalCaseConditions](src/test/scala/LinterPluginTest.scala#L1362), ReflexiveComparison, [YodaConditions](src/test/scala/LinterPluginTest.scala#L1221), [UseConditionDirectly](src/test/scala/LinterPluginTest.scala#L749), [UseIfExpression](src/test/scala/LinterPluginTest.scala#L130), [UnnecessaryElseBranch](src/test/scala/LinterPluginTest.scala#L140), [DuplicateIfBranches](src/test/scala/LinterPluginTest.scala#L774), [IdenticalIfElseCondition](src/test/scala/LinterPluginTest.scala#L1164), [MergeNestedIfs](src/test/scala/LinterPluginTest.scala#L1244), [VariableAssignedUnusedValue](src/test/scala/LinterPluginTest.scala#L1394), [MalformedSwap](src/test/scala/LinterPluginTest.scala#L1340), IdenticalIfCondition, [IdenticalStatements](src/test/scala/LinterPluginTest.scala#L1466), IndexingWithNegativeNumber, OptionOfOption, [UndesirableTypeInference](src/test/scala/LinterPluginTest.scala#L1374), AssigningOptionToNull, WrapNullWithOption, AvoidOptionStringSize, AvoidOptionCollectionSize, UseGetOrElseOnOption, UseOptionOrNull, UseOptionGetOrElse, [UseExistsNotFindIsDefined](src/test/scala/LinterPluginTest.scala#L1104), [UseExistsNotFilterIsEmpty](src/test/scala/LinterPluginTest.scala#L1123), [UseFindNotFilterHead](src/test/scala/LinterPluginTest.scala#L393), [UseContainsNotExistsEquals](src/test/scala/LinterPluginTest.scala#L624), [UseQuantifierFuncNotFold](src/test/scala/LinterPluginTest.scala#L646), [UseFuncNotReduce](src/test/scala/LinterPluginTest.scala#L681), [UseFuncNotFold](src/test/scala/LinterPluginTest.scala#L662), [MergeMaps](src/test/scala/LinterPluginTest.scala#L1029), [FuncFirstThenMap](src/test/scala/LinterPluginTest.scala#L1038), [FilterFirstThenSort](src/test/scala/LinterPluginTest.scala#L1053), [UseMinOrMaxNotSort](src/test/scala/LinterPluginTest.scala#L1067), [UseMapNotFlatMap](src/test/scala/LinterPluginTest.scala#L1081), [UseFilterNotFlatMap](src/test/scala/LinterPluginTest.scala#L1092), AvoidOptionMethod, [TransformNotMap](src/test/scala/LinterPluginTest.scala#L485), [DuplicateKeyInMap](src/test/scala/LinterPluginTest.scala#L971), [InefficientUseOfListSize](src/test/scala/LinterPluginTest.scala#L991), [OnceEvaluatedStatementsInBlockReturningFunction](src/test/scala/LinterPluginTest.scala#L1559), [IntDivisionAssignedToFloat](src/test/scala/LinterPluginTest.scala#L1567), [UseFlattenNotFilterOption](src/test/scala/LinterPluginTest.scala#L984), [UseExistsNotFilterEmpty](src/test/scala/LinterPluginTest.scala#L525), [UseCountNotFilterLength](src/test/scala/LinterPluginTest.scala#L551), [UseExistsNotCountCompare](src/test/scala/LinterPluginTest.scala#L581), [PassPartialFunctionDirectly](src/test/scala/LinterPluginTest.scala#L1548), [UnitImplicitOrdering](src/test/scala/LinterPluginTest.scala#L271), [RegexWarning](src/test/scala/LinterPluginTest.scala#L402), [InvariantCondition](src/test/scala/LinterPluginTest.scala#L439), DecomposingEmptyCollection, InvariantExtrema, [UnnecessaryMethodCall](src/test/scala/LinterPluginTest.scala#L1005), ProducesEmptyCollection, OperationAlwaysProducesZero, [ModuloByOne](src/test/scala/LinterPluginTest.scala#L1591), DivideByOne, DivideByZero, ZeroDivideBy, [UseUntilNotToMinusOne](src/test/scala/LinterPluginTest.scala#L1517), [InvalidParamToRandomNextInt](src/test/scala/LinterPluginTest.scala#L1580), UnusedForLoopIteratorValue, StringMultiplicationByNonPositive, LikelyIndexOutOfBounds, UnnecessaryReturn, InvariantReturn, [UnusedParameter](src/test/scala/LinterPluginTest.scala#L864), [InvalidStringFormat](src/test/scala/LinterPluginTest.scala#L330), InvalidStringConversion, UnnecessaryStringNonEmpty, UnnecessaryStringIsEmpty, [PossibleLossOfPrecision](src/test/scala/LinterPluginTest.scala#L256), [UnsafeAbs](src/test/scala/LinterPluginTest.scala#L283), [TypeToType](src/test/scala/LinterPluginTest.scala#L299), [EmptyStringInterpolator](src/test/scala/LinterPluginTest.scala#L347), [UnlikelyToString](src/test/scala/LinterPluginTest.scala#L358), [UnthrownException](src/test/scala/LinterPluginTest.scala#L368), [SuspiciousMatches](src/test/scala/LinterPluginTest.scala#L377), [IfDoWhile](src/test/scala/LinterPluginTest.scala#L457)

__Note:__ Links currently go to the test for that check.

## Examples of reported warnings

### If checks
#### Repeated condition in an else-if chain
```scala
scala> if (a == 10 || b == 10) 0 else if (a == 20 && b == 10) 1 else 2
<console>:10: warning: This condition has appeared earlier in the if-else chain and will never hold here.
              if (a == 10 || b == 10) 0 else if (a == 20 && b == 10) 1 else 2
                                                              ^
```

#### Identical branches
```scala
scala> if (b > 4) (2,a) else (2,a)
<console>:9: warning: If statement branches have the same structure.
              if (b > 4) (2,a) else (2,a)
                    ^
```

#### Unnecessary if
```scala
scala> if (a == b) true else false
<console>:9: warning: Remove the if expression and use the condition directly.
              if (a == b) true else false
              ^
```

### Pattern matching checks
#### Detect some unreachable cases
```scala
scala> (x,y) match { case (a,5) if a > 5 => 0 case (c,5) if c > 5 => 1 }
<console>:10: warning: Identical case condition detected above. This case will never match.
              (x,y) match { case (a,5) if a > 5 => 0 case (c,5) if c > 5 => 1 }
                                                          ^
```

#### Identical neighbouring cases
```scala
scala> a match { case 3 => "hello" case 4 => "hello" case 5 => "hello" case _ => "how low" }
<console>:9: warning: Bodies of 3 neighbouring cases are identical and could be merged.
              a match { case 3 => "hello" case 4 => "hello" case 5 => "hello" case _ => "how low" }
                                                                      ^
```

#### Match better written as if
```scala
scala> bool match { case true => 0 case false => 1 }
<console>:9: warning: Pattern matching on Boolean is probably better written as an if statement.
              a match { case true => 0 case false => 1 }
                ^
```

### Integer checks (some abstract intepretation)
#### Check conditions
```scala
scala> for (i <- 10 to 20) { if (i > 20) "" }
<console>:8: warning: This condition will never hold.
              for (i <- 10 to 20) { if (i > 20) "" }
                                          ^
```

#### Detect division by zero
```scala
scala> for (i <- 1 to 10) { 1/(i-1)  }
<console>:8: warning: You will likely divide by zero here.
              for (i <- 1 to 10) { 1/(i-1)  }
                                    ^
```

#### Detect too large, or negative indices
```scala
scala> { val a = List(1,2,3); for (i <- 1 to 10) { println(a(i)) } }
<console>:8: warning: You will likely use a too large index.
              { val a = List(1,2,3); for (i <- 1 to 10) { println(a(i)) } }
                                                                   ^
```

### String checks (some abstract intepretation)
#### Attempt to verify string length conditions
```scala
scala> for (i <- 10 to 20) { if (i.toString.length == 3) "" }
<console>:8: warning: This condition will never hold.
              for (i <- 10 to 20) { if (i.toString.length == 3) "" }
                                                          ^
```

#### Attempt to track the prefix, suffix, and pieces
```scala
scala> { val a = "hello"+util.Random.nextString(10)+"world"+util.Random.nextString(10)+"!"; if (a contains "world") ""; if (a startsWith "hell") "" }
<console>:8: warning: This contains will always returns the same value: true
              { val a = "hello"+util.Random.nextString(10)+"world"+util.Random.nextString(10)+"!"; if (a contains "world") ""; if (a startsWith "hell") "" }
                                                                                                                   ^
<console>:8: warning: This startsWith always returns the same value: true
              { val a = "hello"+util.Random.nextString(10)+"world"+util.Random.nextString(10)+"!"; if (a contains "world") ""; if (a startsWith "hell") "" }
                                                                                                                                                ^
```

#### Regex syntax warnings
```scala
scala> str.replaceAll("?", ".")
<console>:9: warning: Regex pattern syntax error: Dangling meta character '?'
              str.replaceAll("?", ".")
                             ^
```

### Numeric checks
#### Using `log(1 + a)` instead of `log1p(a)`
```scala
scala> math.log(1d + a)
<console>:9: warning: Use math.log1p(x), instead of math.log(1 + x) for added accuracy when x is near 0.
              math.log(1 + a)
                      ^
```

#### Loss of precision on BigDecimal
```scala
scala> BigDecimal(0.555555555555555555555555555)
<console>:8: warning: Possible loss of precision. Literal cannot be represented exactly by Double. (0.555555555555555555555555555 != 0.5555555555555556)
              BigDecimal(0.555555555555555555555555555)
                        ^
```

### Option checks
#### Using Option.size
```scala
scala> val a = Some(List(1,2,3)); if (a.size > 3) ""
<console>:9: warning: Did you mean to take the size of the collection inside the Option?
              if (a.size > 3) ""
                    ^
```

#### Using if-else instead of getOrElse
```scala
scala> if (strOption.isDefined) strOption.get else ""
<console>:9: warning: Use strOption.getOrElse(...) instead of if (strOption.isDefined) strOption.get else ...
              if (strOption.isDefined) strOption.get else ""
                                       ^
```

### Collection checks
#### Use exists(...) instead of find(...).isDefined
```scala
scala> List(1,2,3,4).find(x => x % 2 == 0).isDefined
<console>:8: warning: Use col.exists(...) instead of col.find(...).isDefined.
              List(1,2,3,4).find(x => x % 2 == 0).isDefined
                            ^
```

#### Use filter(...) instead of flatMap(...)
```scala
scala> List(1,2,3,4).flatMap(x => if (x % 2 == 0) List(x) else Nil)
<console>:8: warning: Use col.filter(x => condition) instead of col.flatMap(x => if (condition) ... else ...).
              List(1,2,3,4).flatMap(x => if (x % 2 == 0) List(x) else Nil)
                                   ^
```

### Various possible bugs
#### Unused method parameters
```scala
scala> def func(b: Int, c: String, d: String) = { println(b); b+c }
<console>:7: warning: Parameter d is not used in method func
              def func(b: Int, c: String, d: String) = { println(b); b+c }
                  ^
```

#### Unsafe `contains`
```scala
scala> List(1, 2, 3).contains("4")
<console>:29: warning: List[Int].contains(String) will probably return false, since the collection and target element are of unrelated types.
               List(1, 2, 3).contains("4")
                             ^
```

#### Unsafe `==`
```scala
scala> Nil == None
<console>:29: warning: Comparing with == on instances of unrelated types (scala.collection.immutable.Nil.type, None.type) will probably return false.
               Nil == None
                   ^
```

## Future Work

* Improve documentation (bug/style/optimization, most valuable checks, descriptions, ...)
* Improve testing (larger samples, generated tests, link test names to check names, ...)
* Choose whether specific checks should return warnings or errors
* Add more checks
* Reduce false positive rate
* Check out new stuff such as quasiquotes
* Figure out why some tests fail on Scala 2.11

### Ideas for new checks

Feel free to add your own ideas, or implement these. Pull requests welcome!

* Require explicit `override` whenever a method is being overridden
* Expressions spanning multiple lines should be enclosed in parentheses
* Traversable#head, Traversable#last, Traversable#maxBy
* Warn on shadowing variables, especially those of the same type (`var a = 4; { val a = 5 }`)
* Warn on inexhaustive pattern matching or unreachable cases
* Boolean function parameters should be named (`func("arg1", force = true)`)
* Detect vars, that could easily be vals (done in scala 2.11 -Xlint)

Rule lists from other static analysis tools:
* ScalaStyle(Scala) - https://github.com/scalastyle/scalastyle/wiki
* Scapegoat(Scala) - https://github.com/sksamuel/scalac-scapegoat-plugin#inspections
* WartRemover(Scala) - https://github.com/puffnfresh/wartremover#warts
* Scala Abide(Scala) - https://github.com/scala/scala-abide
* ($)SuperSafe™(Scala) - http://www.artima.com/supersafe_user_guide.html
* IntelliJ IDEA(Scala/...) - https://www.jetbrains.com/idea/features/scala.html
* Findbugs(JVM) - http://findbugs.sourceforge.net/bugDescriptions.html
* ($)Julia(JVM) - http://www.juliasoft.com/warnings
* CheckStyle(Java) - http://checkstyle.sourceforge.net/availablechecks.html
* PMD(Java) - http://pmd.sourceforge.net/snapshot/pmd-java/rules/index.html
* Error-prone(Java) - https://github.com/google/error-prone
* CodeNarc(Groovy) - http://codenarc.sourceforge.net/codenarc-rule-index.html
* Shellcheck(shell) - http://www.shellcheck.net/about.html
* ($)PVS-Studio(C++) - http://www.viva64.com/en/d/
* ($)Coverity(C++/Java/...) - http://www.slideshare.net/Coverity/static-analysis-primer-22874326 (6,7)
* CppCheck(C++) - http://sourceforge.net/p/cppcheck/wiki/ListOfChecks/
* Clang(C++/ObjC) - http://clang-analyzer.llvm.org/available_checks.html
* OCLint(C++/ObjC) - http://docs.oclint.org/en/dev/rules/index.html
* ($)Fortify(Java/C++/...) - http://www.hpenterprisesecurity.com/vulncat/en/vulncat/index.html
* Infer(Java/C/ObjC) - http://fbinfer.com/docs/infer-bug-types.html
 
### Some resources
* A repo with various resources about the scala compiler: https://github.com/illandan/scala.compiler.guides
* A quick overview of writing compiler plugins: http://www.scala-lang.org/old/node/140
* Basic tree example, and list of AST elements: http://stackoverflow.com/q/10419101/293115
* Notes on compiler plugins from ScalaCL: http://code.google.com/p/scalacl/wiki/WritingScalaCompilerPlugins
* Notes and a similar compiler plugin from a while ago: https://github.com/ymasory/alacs/blob/master/dev/resources.md
* Great article about practical static analysis from Coverity authors: http://cacm.acm.org/magazines/2010/2/69354-a-few-billion-lines-of-code-later/fulltext

