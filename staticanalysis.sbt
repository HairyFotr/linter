import de.johoop.cpd4sbt.CopyPasteDetector._
import de.johoop.cpd4sbt.{OutputType => CPDOutputType, ReportType => CPDReportType}
import de.johoop.findbugs4sbt._

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-Yrangepos",
  "-Xlint",
  //"-Xdev",//2.11
  //"-Xcheckinit",
  //"-Xstrict-inference",
  "-Ywarn-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-inaccessible",
  //"-Ywarn-infer-any",//2.11
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard")

// Linter
//resolvers += Resolver.sonatypeRepo("snapshots")
//addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1-SNAPSHOT")
//scalacOptions += "-P:linter:disable:UseHypot+CloseSourceFile"

// or manually, e.g.

//scalacOptions += "-Xplugin:lib/linter_2.11-0.1-SNAPSHOT.jar"

// Wartremover
//wartremoverWarnings ++= Warts.unsafe
//wartremoverWarnings ++= Seq(Wart.Any2StringAdd, Wart.Enumeration, Wart.Serializable, Wart.Product)

// Scapegoat
//scapegoatVersion := "1.2.1"

// Scalastyle
scalastyleConfig <<= baseDirectory { base => base / "sca" / "scalastyle-config.xml" }
watchSources += baseDirectory.value / "sca" / "scalastyle-config.xml"

// Findbugs (optionally put findbugs plugins (such as fb-contrib and findsecbugs) jars into ~/.findbugs/plugin)
findbugsSettings
findbugsEffort := Effort.Maximum
findbugsReportPath <<= baseDirectory { base => Some(base / "sca" / "findbugsoutput.xml") }

// CPD
cpdSettings
cpdTargetPath <<= baseDirectory { base => base / "sca" }
cpdReportName := "cpdoutput.txt"
cpdReportType := CPDReportType.Simple
cpdOutputType := CPDOutputType.Console
