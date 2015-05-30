name := "linter"

organization := "com.foursquare.lint"

//version := "0.1.10"

scalaVersion := "2.10.5"

crossScalaVersions <<= scalaVersion { scalaVersion => Seq("2.10.5", "2.11.6", "2.12.0-M1") }

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

libraryDependencies := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor <= 11 =>
      libraryDependencies.value :+ "org.specs2" %% "specs2" % "2.4" % "test"
    case _ =>
      libraryDependencies.value // Tests won't work in Scala 2.12
  }
}
libraryDependencies ++= Seq(
  "junit"          % "junit"           % "4.12" % "test",
  "com.novocode"   % "junit-interface" % "0.11" % "test")

libraryDependencies <+= scalaVersion { (scalaVersion) =>
  "org.scala-lang" % "scala-compiler"  % scalaVersion
}

libraryDependencies := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      libraryDependencies.value :+ "org.scala-lang.modules" %% "scala-xml" % "1.0.3"
    case _ =>
      libraryDependencies.value
  }
}

publishTo := Some(Resolver.file("file",  new File( "../linteRepo/releases" )) )
