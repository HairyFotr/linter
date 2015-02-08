name := "linter"

organization := "com.foursquare.lint"

//version := "0.1.7"

scalaVersion := "2.10.4"

crossScalaVersions <<= scalaVersion { scalaVersion => Seq("2.10.4", "2.11.4") }

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

libraryDependencies ++= Seq(
  "org.specs2"     %% "specs2"         % "2.4" % "test",
  "junit"          % "junit"           % "4.12"   % "test",
  "com.novocode"   % "junit-interface" % "0.11"   % "test")

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

scalacOptions in console in Compile <+= (packageBin in Compile) map { pluginJar => "-Xplugin:"+pluginJar }

publishTo := Some(Resolver.file("file",  new File( "../linteRepo/releases" )) )
