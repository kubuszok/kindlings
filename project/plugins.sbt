// kubuszok plugin (bundles: sbt-git, sbt-scalafmt, sbt-scoverage, sbt-commandmatrix, sbt-pgp, sbt-ide-settings, sbt-scalajs, sbt-scala-native, sbt-mima)
// On sbt 2.0 sbt-projectmatrix is built in and sbt-welcome has no sbt-2.0 build, so neither is bundled here.
// TODO: remove after sbt-kubuszok 0.2.5
resolvers += "Maven Central Snapshots" at "https://central.sonatype.com/repository/maven-snapshots"
addSbtPlugin("com.kubuszok" % "sbt-kubuszok" % "0.2.4-4-g52226ed-SNAPSHOT")
// benchmarks
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")

ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
