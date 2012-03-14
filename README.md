# Linter Compiler Plugin

This is a compiler plugin that adds additional lint checks to protect against sharp corners
in the Scala compiler and standard libraries.

It's a work in progress.

## Usage

Add it as a compiler plugin in your project, or run `sbt console` in this project to see it in action.

## Currently suported warnings

### Unsafe `==`

    scala> Nil == None
    <console>:29: warning: Comparing with == on instances of different types (object Nil, object None) will probably return false.
                  Nil == None
                      ^

### Unsafe `contains`

    scala> List(1, 2, 3).contains("4")
    <console>:29: warning: SeqLike[Int].contains(java.lang.String) will probably return false.
                  List(1, 2, 3).contains("4")
                                ^


### Wildcard import from `scala.collection.JavaConversions`

    scala> import scala.collection.JavaConversions._
    <console>:29: warning: Conversions in scala.collection.JavaConversions._ are dangerous.
           import scala.collection.JavaConversions._
                                   ^

### Calling `Option#get`

    scala> Option(1).get
    <console>:29: warning: Calling .get on Option will throw an exception if the Option is None.
                  Option(1).get
                            ^

## Future Work

* Add more warnings
* Pick and choose which warnings you want
* Choose whether they should be warnings or errors

### Ideas for new warnings

Feel free to implement these, or add your own ideas. Pull requests welcome!

* Warn on wildcard imports (either all with whitelist, or blacklist)
* Require explicit `override` whenever a method is being overwritten
* Implicit methods should always have explicit return types
* Expressions spanning multiple lines should be enclosed in parentheses
* Unused method argument warnings
* Warn on unrestricted catch clauses (`case e => ...`)
* Traversable#head, Traversable#last, Traversable#maxBy
* Warn on shadowing variables, especially those of the same type
* Warn on inexhaustive pattern matching
