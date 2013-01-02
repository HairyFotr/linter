libraryDependencies <++= (scalaVersion) { (scalaVersion) =>
  Seq(
    "org.scala-lang"           % "scala-compiler"  % scalaVersion,
    if (scalaVersion startsWith "2.9")
      "org.scala-tools.testing"  % "specs_2.9.1"     % "1.6.9"  % "test" else
      "org.specs2"  % "specs2_2.10"     % "1.13"  % "test",
    "junit"                    % "junit"           % "4.8.2"  % "test",
    "com.novocode"             % "junit-interface" % "0.7"    % "test"
  )
}

scalacOptions in console in Compile <+= (packageBin in Compile) map { pluginJar =>
  "-Xplugin:"+pluginJar
}

scalaVersion := "2.10.0"

name := "linter"

organization := "com.foursquare.lint"