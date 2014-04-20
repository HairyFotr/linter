import de.johoop.findbugs4sbt._

name := "linter"

organization := "com.foursquare.lint"

//version := "0.1.2"

scalaVersion := "2.10.4"

crossScalaVersions <<= scalaVersion { scalaVersion => Seq("2.10.4", "2.11.0") }

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

libraryDependencies <++= (scalaVersion) { (scalaVersion) =>
  Seq(
    "org.scala-lang" % "scala-compiler"  % scalaVersion,
    "org.specs2"     %% "specs2"          % "2.3.11" % "test",
    "junit"          % "junit"           % "4.11"   % "test",
    "com.novocode"   % "junit-interface" % "0.10"   % "test"
  )
}

libraryDependencies := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      libraryDependencies.value :+ "org.scala-lang.modules" %% "scala-xml" % "1.0.1"
    case _ =>
      libraryDependencies.value
  }
}

scalacOptions in console in Compile <+= (packageBin in Compile) map { pluginJar => "-Xplugin:"+pluginJar }

publishTo := Some(Resolver.file("file",  new File( "../linteRepo/releases" )) )

// Well, if we're gonna do static analysis, why not see what the compiler already does ;)

scalacOptions ++= Seq(
  "-Yrangepos",
  "-deprecation",
  "-unchecked",
  "-Xlint")

scalacOptions ++= Seq(
  "-Ywarn-dead-code",
  "-Ywarn-inaccessible",
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard")

// Also, a self-test

//scalacOptions += "-Xplugin:../linteRepo/releases/com/foursquare/lint/linter_2.10/0.1-SNAPSHOT/linter_2.10-0.1-SNAPSHOT.jar"

//scalacOptions += "-Xplugin:../linteRepo/releases/com/foursquare/lint/linter_2.11/0.1-SNAPSHOT/linter_2.11-0.1-SNAPSHOT.jar"

// Also, what others are doing

// ScalaStyle

org.scalastyle.sbt.ScalastylePlugin.Settings

// Findbugs

findbugsSettings

findbugsEffort := Effort.Maximum

findbugsReportPath <<= baseDirectory { base => Some(base / "findbugs.xml") }

// CPD

//import de.johoop.cpd4sbt.CopyPasteDetector._

cpdSettings
