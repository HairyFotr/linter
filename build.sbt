libraryDependencies <++= (scalaVersion) { (scalaVer) =>
  Seq(
    "org.scala-lang"           % "scala-compiler"  % scalaVer,
    "org.scala-tools.testing" %% "specs"           % "1.6.9"  % "test" withSources(),
    "junit"                    % "junit"           % "4.8.2"  % "test" withSources(),
    "com.novocode"             % "junit-interface" % "0.7"    % "test"
  )
}

scalacOptions in console in Compile <+= (packageBin in Compile) map { pluginJar =>
  "-Xplugin:"+pluginJar
}