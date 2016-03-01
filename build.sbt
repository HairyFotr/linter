name := "linter"

licenses := Seq("Apache License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

homepage := Some(url("https://github.com/HairyFotr/linter"))

organization := "org.psywerx.hairyfotr"

//version := "0.1.13"

scalaVersion := "2.10.6"

crossScalaVersions <<= scalaVersion { scalaVersion => Seq("2.10.6", "2.11.7", "2.12.0-M3") }

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

libraryDependencies ++= Seq(
  "junit"          % "junit"           % "4.12" % "test",
  "com.novocode"   % "junit-interface" % "0.11" % "test")
  
libraryDependencies := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor <= 11 =>
      libraryDependencies.value :+ "org.specs2" %% "specs2" % "2.4" % "test"
    case _ =>
      // Tests won't work in Scala 2.12 for now
      libraryDependencies.value
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

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <scm>
    <url>https://github.com/HairyFotr/linter.git</url>
  </scm>
  <developers>
    <developer>
      <id>HairyFotr</id>
      <name>Matic Potočnik</name>
      <email>hairyfotr@gmail.com</email>
      <url>https://github.com/HairyFotr</url>
    </developer>
  </developers>
)
