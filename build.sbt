name := "linter"

organization := "com.foursquare.lint"

//version := "0.1.11"

scalaVersion := "2.10.5"

crossScalaVersions <<= scalaVersion { scalaVersion => Seq("2.10.5", "2.11.7", "2.12.0-M2") }

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

libraryDependencies ++= Seq(
  "junit"          % "junit"           % "4.12" % "test",
  "com.novocode"   % "junit-interface" % "0.11" % "test")
  
libraryDependencies := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor <= 11 =>
      libraryDependencies.value :+ "org.specs2" %% "specs2" % "2.4" % "test"
    case _ =>
      libraryDependencies.value // Tests won't work in Scala 2.12 for now
  }
}

libraryDependencies := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      libraryDependencies.value :+ "org.scala-lang.modules" %% "scala-xml" % "1.0.5"
    case _ =>
      libraryDependencies.value
  }
}

libraryDependencies <+= scalaVersion { (scalaVersion) =>
  "org.scala-lang" % "scala-compiler"  % scalaVersion
}

// Enable linter in console
scalacOptions in console in Compile <+= (packageBin in Compile) map { pluginJar => "-Xplugin:"+pluginJar }

publishTo := Some(Resolver.file("file",  new File( "../linteRepo/releases" )) )
