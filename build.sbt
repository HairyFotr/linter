name := "linter"

organization := "com.foursquare.lint"

version := "0.1.0"

scalaVersion := "2.10.3"

libraryDependencies <++= (scalaVersion) { (scalaVersion) =>
  Seq(
    "org.scala-lang"           % "scala-compiler"  % scalaVersion,
    if (scalaVersion startsWith "2.9")
      "org.specs2"  % "specs2_2.9.3"     % "1.12.4.1"  % "test" else
      "org.specs2"  % "specs2_2.10"     % "2.2.3"  % "test",
    "junit"                    % "junit"           % "4.11"  % "test",
    "com.novocode"             % "junit-interface" % "0.10"    % "test"
  )
}

scalacOptions in console in Compile <+= (packageBin in Compile) map { pluginJar =>
  "-Xplugin:"+pluginJar
}

crossScalaVersions <<= scalaVersion { scalaVersion => Seq("2.9.2", "2.9.3", "2.10.3", "2.11.0-RC1") }

publishTo := Some(Resolver.file("file",  new File( "../linteRepo/releases" )) )

// Well, if we're gonna do static analysis, why not see what the compiler already does ;)

scalacOptions ++= Seq("-unchecked", "-Xlint")

scalacOptions ++= Seq("-Ywarn-dead-code", "-Ywarn-inaccessible", "-Ywarn-nullary-override", "-Ywarn-nullary-unit", "-Ywarn-numeric-widen", "-Ywarn-value-discard")

// Also, a self-test

//scalacOptions += "-Xplugin:../linteRepo/releases/com/foursquare/lint/linter_2.10/0.1-SNAPSHOT/linter_2.10-0.1-SNAPSHOT.jar"

// Also, what others are doing

org.scalastyle.sbt.ScalastylePlugin.Settings

//import de.johoop.findbugs4sbt.FindBugs._

seq(findbugsSettings : _*)
