name := "linter"
organization := "org.psywerx.hairyfotr"
licenses := Seq("Apache License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))
homepage := Some(url("https://github.com/HairyFotr/linter"))
//version := "0.1.17"

scalaVersion := "2.10.6"
crossScalaVersions := Seq("2.10.6", "2.11.8", "2.12.1")
libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

// Testing
libraryDependencies ++= Seq(
  "com.novocode"  % "junit-interface" % "0.11"  % Test,
  "org.specs2"   %% "specs2-core"     % "3.8.6" % Test
)

// Enable linter in console
scalacOptions in (Compile, console) += "-Xplugin:" + (packageBin in Compile).value

publishTo := Some(Resolver.sonatypeRepo(if (isSnapshot.value) "snapshots" else "releases"))
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
