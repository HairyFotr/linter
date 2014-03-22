resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.4.0")

addSbtPlugin("de.johoop" % "findbugs4sbt" % "1.3.0")

addSbtPlugin("de.johoop" % "cpd4sbt" % "1.1.4")

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.1.4")

