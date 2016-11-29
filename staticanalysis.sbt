//import de.johoop.cpd4sbt.CopyPasteDetector._
//import de.johoop.cpd4sbt.{OutputType => CPDOutputType, ReportType => CPDReportType}
//import de.johoop.findbugs4sbt._

scalacOptions ++= Seq(
   "-feature"
  ,"-deprecation"
  ,"-unchecked"
  ,"-Yrangepos"
  ,"-Xlint"
//,"-Xcheckinit" //Adds runtime checks
//,"-Xstrict-inference"
  ,"-Ywarn-adapted-args"
  ,"-Ywarn-dead-code"
  ,"-Ywarn-inaccessible"
  ,"-Ywarn-nullary-override"
  ,"-Ywarn-nullary-unit"
  ,"-Ywarn-numeric-widen"
  ,"-Ywarn-value-discard"
)

scalacOptions ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      Seq(
        //"-Xdev",
        "-Ywarn-infer-any")
    case _ =>
      Seq()
  }
}

// Linter
//resolvers += Resolver.sonatypeRepo("snapshots")
//addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1-SNAPSHOT")
//scalacOptions += "-P:linter:disable:UseHypot+CloseSourceFile"
//scalacOptions += "-P:linter:printWarningNames:true"

// or manually, e.g.

//scalacOptions += "-Xplugin:lib/linter_2.11-0.1-SNAPSHOT.jar"

// Abide
//libraryDependencies ++= Seq("abide-core"/*, "abide-extra", "abide-akka"*/).map("com.typesafe" %% _ % "0.1-SNAPSHOT" % "abide")

// Wartremover
//wartremoverWarnings ++= Warts.unsafe
//wartremoverWarnings ++= Warts.allBut(Wart.Any, Wart.Nothing, Wart.Equals, Wart.Null, Wart.While, Wart.Return, Wart.Throw, Wart.Overloading, Wart.Var, Wart.ToString, Wart.NonUnitStatements, Wart.DefaultArguments, Wart.MutableDataStructures, Wart.AsInstanceOf, Wart.IsInstanceOf, Wart.LeakingSealed, Wart.OptionPartial, Wart.FinalCaseClass/*Yes, but not in Warning*/, Wart.EitherProjectionPartial/*TODO*/, Wart.Option2Iterable/*TODO*/, Wart.ListOps/*TODO*/, Wart.NoNeedForMonad/*Crashes*/)

// Scapegoat
//scapegoatVersion := "1.3.0"

// Scalastyle
scalastyleConfig := baseDirectory.value / "sca" / "scalastyle-config.xml"
watchSources += baseDirectory.value / "sca" / "scalastyle-config.xml"

// Findbugs (optionally put findbugs plugins (such as fb-contrib and findsecbugs) jars into ~/.findbugs/plugin)
//findbugsSettings
//findbugsEffort := Effort.Maximum
//findbugsReportPath <<= baseDirectory { base => Some(base / "sca" / "findbugsoutput.xml") }

// CPD
//cpdSettings
//cpdTargetPath <<= baseDirectory { base => base / "sca" }
//cpdReportName := "cpdoutput.txt"
//cpdReportType := CPDReportType.Simple
//cpdOutputType := CPDOutputType.Console
