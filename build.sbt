scalaVersion := "2.10.1"

libraryDependencies <++= (scalaVersion) { (scalaVersion) =>
  Seq(
    "org.scala-lang"           % "scala-compiler"  % scalaVersion,
    if (scalaVersion startsWith "2.9")
      "org.specs2"  % "specs2_2.9.2"     % "1.12.3"  % "test" else
      "org.specs2"  % "specs2_2.10"     % "1.13"  % "test",
    "junit"                    % "junit"           % "4.8.2"  % "test",
    "com.novocode"             % "junit-interface" % "0.7"    % "test"
  )
}

scalacOptions in console in Compile <+= (packageBin in Compile) map { pluginJar =>
  "-Xplugin:"+pluginJar
}

name := "linter"

organization := "com.foursquare.lint"

crossScalaVersions <<= scalaVersion { scalaVersion => Seq("2.11.0-M2", "2.10.1", "2.9.2", "2.9.3") }

publishTo := Some(Resolver.file("file",  new File( "../linteRepo/releases" )) )

//Well, if we're gonna do static analysis, why not see what the compiler already does ;)

scalacOptions ++= Seq("-unchecked", "-Xlint")

//scalacOptions ++= Seq("-Ywarn-all", "-Ywarn-dead-code", "-Ywarn-inaccessible", "-Ywarn-nullary-override", "-Ywarn-nullary-unit", "-Ywarn-value-discard")

//scalacOptions += "-Xplugin:../linteRepo/releases/com/foursquare/lint/linter_2.10/0.1-SNAPSHOT/linter_2.10-0.1-SNAPSHOT.jar"
