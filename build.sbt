libraryDependencies <++= (scalaVersion) { (scalaVer) =>
  Seq(
    "org.scala-lang"           % "scala-compiler"  % scalaVer,
    "org.scala-tools.testing"  % "specs_2.9.1"     % "1.6.9"  % "test" withSources(),
    "junit"                    % "junit"           % "4.8.2"  % "test" withSources(),
    "com.novocode"             % "junit-interface" % "0.7"    % "test"
  )
}

scalacOptions in console in Compile <+= (packageBin in Compile) map { pluginJar =>
  "-Xplugin:"+pluginJar
}

scalaVersion := "2.9.2"

name := "linter"

organization := "com.foursquare.lint"