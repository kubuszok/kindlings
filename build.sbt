import com.jsuereth.sbtpgp.PgpKeys.publishSigned
//import com.typesafe.tools.mima.core.{Problem, ProblemFilters}
import sbtwelcome.UsefulTask
import commandmatrix.extra.*
import sbt.ThisBuild
import org.scalafmt.sbt.ScalafmtPlugin.autoImport.scalafmtOnCompile

// Used to configure the build so that it would format+compile during development but not on CI.
lazy val isCI = sys.env.get("CI").contains("true")
ThisBuild / scalafmtOnCompile := !isCI

// Used to publish snapshots to Maven Central.
val mavenCentralSnapshots = "Maven Central Snapshots" at "https://central.sonatype.com/repository/maven-snapshots"

// Versions:

val versions = new {
  // Versions we are publishing for.
  val scala213 = "2.13.18"
  val scala3 = "3.8.2"

  // Which versions should be cross-compiled for publishing.
  val scalas = List(scala213, scala3)
  val platforms = List(VirtualAxis.jvm, VirtualAxis.js, VirtualAxis.native)

  // Dependencies.
  val hearth = "0.2.0-234-gc1b57c4-SNAPSHOT"
  val kindProjector = "0.13.4"
  val avro = "1.12.1"
  val circe = "0.14.15"
  val jsoniterScala = "2.38.9"
  val scalaYaml = "0.3.1"

  // Explicitly handle Scala 2.13 and Scala 3 separately.
  def fold[A](scalaVersion: String)(for2_13: => Seq[A], for3: => Seq[A]): Seq[A] =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, 13)) => for2_13
      case Some((3, _))  => for3
      case _             => Seq.empty // for sbt, apparently
    }
}

val dev = new {

  val props = scala.util
    .Using(new java.io.FileInputStream("dev.properties")) { fis =>
      val props = new java.util.Properties()
      props.load(fis)
      props
    }
    .get

  // Which version should be used in IntelliJ
  val ideScala = props.getProperty("ide.scala") match {
    case "2.13" => versions.scala213
    case "3"    => versions.scala3
  }
  val idePlatform = props.getProperty("ide.platform") match {
    case "jvm"    => VirtualAxis.jvm
    case "js"     => VirtualAxis.js
    case "native" => VirtualAxis.native
  }

  val logCrossQuotes = props.getProperty("log.cross-quotes") match {
    case "true"                          => true
    case "false"                         => false
    case otherwise if otherwise.nonEmpty => otherwise
    case _                               => !isCI
  }

  def isIdeScala(scalaVersion: String): Boolean =
    CrossVersion.partialVersion(scalaVersion) == CrossVersion.partialVersion(ideScala)
  def isIdePlatform(platform: VirtualAxis): Boolean = platform == idePlatform
}

// Common settings:

Global / excludeLintKeys += git.useGitDescribe
Global / excludeLintKeys += ideSkipProject
val only1VersionInIDE =
  // For the platform we are working with, show only the project for the Scala version we are working with.
  MatrixAction
    .ForPlatform(dev.idePlatform)
    .Configure(
      _.settings(
        ideSkipProject := !dev.isIdeScala(scalaVersion.value),
        bspEnabled := dev.isIdeScala(scalaVersion.value),
        scalafmtOnCompile := !isCI
      )
    ) +:
    // Do not show in IDE and BSP projects for the platform we are not working with.
    versions.platforms.filterNot(dev.isIdePlatform).map { platform =>
      MatrixAction
        .ForPlatform(platform)
        .Configure(_.settings(ideSkipProject := true, bspEnabled := false, scalafmtOnCompile := false))
    }

// The hearth-cross-quotes:
//  - on Scala 2 are macros (defined for all platforms)
//  - and on Scala 3 are plugins (defined only for JVM).
val useCrossQuotes = versions.scalas.flatMap { scalaVersion =>
  versions.fold(scalaVersion)(
    for2_13 = List(
      // Enable logging from cross-quotes.
      MatrixAction
        .ForScala(_.isScala2)
        .Configure(_.settings(scalacOptions += s"-Xmacro-settings:hearth.cross-quotes.logging=${dev.logCrossQuotes}"))
    ),
    for3 = List(
      MatrixAction
        .ForScala(_.isScala3)
        .Configure(
          _.settings(
            libraryDependencies += compilerPlugin("com.kubuszok" %% "hearth-cross-quotes" % versions.hearth),
            scalacOptions ++=
              Seq(
                // Enable logging from cross-quotes.
                s"-P:hearth.cross-quotes:logging=${dev.logCrossQuotes}"
              )
          )
        )
    )
  )
}

val settings = Seq(
  git.useGitDescribe := true,
  git.uncommittedSignifier := None,
  scalacOptions ++= versions.fold(scalaVersion.value)(
    for3 = Seq(
      // format: off
      "-encoding", "UTF-8",
      "-release", "17",
      "-rewrite",
      "-source", "3.3-migration",
      // format: on
      "-unchecked",
      "-deprecation",
      "-explain",
      "-explain-cyclic",
      "-explain-types",
      "-feature",
      "-no-indent",
      "-language:postfixOps", // "for >>"
      "-Wconf:msg=Unreachable case:s", // suppress fake (?) errors in internal.compiletime
      "-Wconf:msg=Missing symbol position:s", // suppress warning https://github.com/scala/scala3/issues/21672
      "-Werror",
      "-Wnonunit-statement",
      // "-Wunused:imports", // import x.Underlying as X is marked as unused even though it is! probably one of https://github.com/scala/scala3/issues/: #18564, #19252, #19657, #19912
      "-Wunused:privates",
      "-Wunused:locals",
      "-Wunused:explicits",
      "-Wunused:implicits",
      "-Wunused:params",
      "-Wvalue-discard",
      "-Xcheck-macros",
      "-Xkind-projector:underscores"
    ),
    for2_13 = Seq(
      // format: off
      "-encoding", "UTF-8",
      "-release", "11",
      // format: on
      "-unchecked",
      "-deprecation",
      "-explaintypes",
      "-feature",
      "-language:higherKinds",
      "-Wconf:cat=scala3-migration:s", // silence mainly issues with -Xsource:3 and private case class constructors
      "-Wconf:cat=deprecation&origin=hearth.*:s", // we want to be able to deprecate APIs and test them while they're deprecated
      "-Wconf:msg=The outer reference in this type test cannot be checked at run time:s", // suppress fake(?) errors in internal.compiletime (adding origin breaks this suppression)
      "-Wconf:msg=discarding unmoored doc comment:s", // silence errors when scaladoc cannot comprehend nested vals
      "-Wconf:msg=Could not find any member to link for:s", // since errors when scaladoc cannot link to stdlib types or nested types
      "-Wconf:msg=Variable .+ undefined in comment for:s", // silence errors when there we're showing a buggy Expr in scaladoc comment
      "-Wconf:msg=a type was inferred to be kind-polymorphic `Nothing` to conform to:s", // silence warn that appeared after updating to Scala 2.13.17
      "-Wunused:patvars",
      "-Xfatal-warnings",
      "-Xlint:adapted-args",
      "-Xlint:delayedinit-select",
      "-Xlint:doc-detached",
      "-Xlint:inaccessible",
      "-Xlint:infer-any",
      "-Xlint:nullary-unit",
      "-Xlint:option-implicit",
      "-Xlint:package-object-classes",
      "-Xlint:poly-implicit-overload",
      "-Xlint:private-shadow",
      "-Xlint:stars-align",
      "-Xlint:type-parameter-shadow",
      "-Xsource:3",
      "-Yrangepos",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-unused:locals",
      "-Ywarn-unused:imports",
      "-Ywarn-macros:after",
      "-Xsource-features:eta-expand-always", // silence warn that appears since 2.13.17
      "-Ytasty-reader"
    )
  ),
  Compile / doc / scalacOptions ++= versions.fold(scalaVersion.value)(
    for3 = Seq("-Ygenerate-inkuire"), // type-based search for Scala 3, this option cannot go into compile
    for2_13 = Seq.empty
  ),
  Compile / console / scalacOptions --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings")
)

val dependencies = Seq(
  libraryDependencies ++= Seq(
    "com.kubuszok" %%% "hearth" % versions.hearth,
    "com.kubuszok" %%% "hearth-munit" % versions.hearth % Test
  ),
  libraryDependencies ++= versions.fold(scalaVersion.value)(
    for3 = Seq.empty,
    for2_13 = Seq(
      compilerPlugin("org.typelevel" % "kind-projector" % versions.kindProjector cross CrossVersion.full)
    )
  ),
  resolvers += mavenCentralSnapshots
)

val versionSchemeSettings = Seq(versionScheme := Some("early-semver"))

val publishSettings = Seq(
  organization := "com.kubuszok",
  homepage := Some(url("https://scala-hearth.readthedocs.io")),
  organizationHomepage := Some(url("https://kubuszok.com")),
  licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/MateuszKubuszok/hearth/"),
      "scm:git:git@github.com:MateuszKubuszok/hearth.git"
    )
  ),
  startYear := Some(2026),
  developers := List(
    Developer("MateuszKubuszok", "Mateusz Kubuszok", "", url("https://github.com/MateuszKubuszok"))
  ),
  pomExtra := (
    <issueManagement>
      <system>GitHub issues</system>
      <url>https://github.com/MateuszKubuszok/hearth/issues</url>
    </issueManagement>
  ),
  publishTo := {
    if (isSnapshot.value) Some(mavenCentralSnapshots)
    else localStaging.value
  },
  publishMavenStyle := true,
  Test / publishArtifact := false,
  pomIncludeRepository := { _ =>
    false
  },
  // Sonatype ignores isSnapshot setting and only looks at -SNAPSHOT suffix in version:
  //   https://central.sonatype.org/publish/publish-maven/#performing-a-snapshot-deployment
  // meanwhile sbt-git used to set up SNAPSHOT if there were uncommitted changes:
  //   https://github.com/sbt/sbt-git/issues/164
  // (now this suffix is empty by default) so we need to fix it manually.
  git.gitUncommittedChanges := git.gitCurrentTags.value.isEmpty,
  git.uncommittedSignifier := Some("SNAPSHOT")
)

val noPublishSettings =
  Seq(publish / skip := true, publishArtifact := false)

// Command generation

val al = new {

  private val prodProjects =
    Vector("fastShowPretty", "circeDerivation", "jsoniterDerivation", "jsoniterJson", "yamlDerivation")

  private val jvmOnlyProdProjects = Vector("avroDerivation")

  private def isJVM(platform: String): Boolean = platform == "JVM"

  private def projects(platform: String, scalaSuffix: String): Vector[String] = {
    val crossPlatformProjects = for {
      name <- prodProjects
    } yield s"$name${if (isJVM(platform)) "" else platform}$scalaSuffix"
    val jvmOnly = if (isJVM(platform)) jvmOnlyProdProjects.map(name => s"$name$scalaSuffix") else Vector.empty
    crossPlatformProjects ++ jvmOnly
  }

  def ci(platform: String, scalaSuffix: String): String = {
    def tasksOf(name: String): Vector[String] = projects(platform, scalaSuffix).flatMap { case project =>
      Vector(s"$project/$name")
    }

    val clean = Vector("clean")
    val compileAndTest = tasksOf("compile") ++ tasksOf("test")
    val coverageCompileAndTest =
      if (isJVM(platform)) "coverage" +: compileAndTest :+ "coverageAggregate" :+ "coverageOff" else compileAndTest
    // val mimaReport = tasksOf("mimaReportBinaryIssues")

    val tasks = clean ++ coverageCompileAndTest // ++ mimaReport
    tasks.mkString(" ; ")
  }

  def test(platform: String, scalaSuffix: String): String =
    projects(platform, scalaSuffix).map(project => s"$project/test").mkString(" ; ")

  def release(tag: Seq[String]): String =
    if (tag.nonEmpty) "publishSigned ; sonaRelease" else "publishSigned"

  def publishLocal(platform: String, scalaSuffix: String): Vector[String] = {
    val crossPlatform = for {
      name <- prodProjects
    } yield s"$name${if (isJVM(platform)) "" else platform}$scalaSuffix/publishLocal"
    val jvmOnly =
      if (isJVM(platform)) jvmOnlyProdProjects.map(name => s"$name$scalaSuffix/publishLocal") else Vector.empty
    crossPlatform ++ jvmOnly
  }

  val publishLocalForTests = (publishLocal("JVM", "") ++ publishLocal("JVM", "3")).mkString(" ; ")
}

// Modules

lazy val root = project
  .in(file("."))
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .settings(settings)
  .settings(publishSettings)
  .settings(noPublishSettings)
  .aggregate(fastShowPretty.projectRefs *)
  .aggregate(circeDerivation.projectRefs *)
  .aggregate(jsoniterDerivation.projectRefs *)
  .aggregate(jsoniterJson.projectRefs *)
  .aggregate(yamlDerivation.projectRefs *)
  .aggregate(avroDerivation.projectRefs *)
  .settings(
    moduleName := "kindlings",
    name := "kindlings",
    description := "Build setup for Kindlings modules",
    logo :=
      s"""Kindlings ${(version).value} build for (${versions.scala213}, ${versions.scala3}) x (Scala JVM, Scala.js $scalaJSVersion, Scala Native $nativeVersion)
         |
         |This build uses sbt-projectmatrix with sbt-commandmatrix helper:
         | - Scala JVM adds no suffix to a project name seen in build.sbt
         | - Scala.js adds the "JS" suffix to a project name seen in build.sbt
         | - Scala Native adds the "Native" suffix to a project name seen in build.sbt
         | - Scala 2.13 adds no suffix to a project name seen in build.sbt
         | - Scala 3 adds the suffix "3" to a project name seen in build.sbt
         |
         |When working with IntelliJ or Scala Metals, edit dev.properties to control which Scala version you're currently working with.
         |""".stripMargin,
    usefulTasks := Seq(
      UsefulTask("projects", "List all projects generated by the build matrix").noAlias,
      UsefulTask(
        "test",
        "Compile and test all projects in all Scala versions and platforms (beware! it uses a lot of memory and might OOM!)"
      ).noAlias,
      UsefulTask(al.release(git.gitCurrentTags.value), "Publish everything to release or snapshot repository")
        .alias("ci-release"),
      UsefulTask(al.ci("JVM", "3"), "CI pipeline for Scala 3+JVM").alias("ci-jvm-3"),
      UsefulTask(al.ci("JVM", ""), "CI pipeline for Scala 2.13+JVM").alias("ci-jvm-2_13"),
      UsefulTask(al.ci("JS", "3"), "CI pipeline for Scala 3+Scala JS").alias("ci-js-3"),
      UsefulTask(al.ci("JS", ""), "CI pipeline for Scala 2.13+Scala JS").alias("ci-js-2_13"),
      UsefulTask(al.ci("Native", "3"), "CI pipeline for Scala 3+Scala Native").alias("ci-native-3"),
      UsefulTask(al.ci("Native", ""), "CI pipeline for Scala 2.13+Scala Native").alias("ci-native-2_13"),
      UsefulTask(al.test("JVM", "3"), "Test all projects in Scala 3+JVM").alias("test-jvm-3"),
      UsefulTask(al.test("JVM", ""), "Test all projects in Scala 2.13+JVM").alias("test-jvm-2_13"),
      UsefulTask(al.test("JS", "3"), "Test all projects in Scala 3+Scala JS").alias("test-js-3"),
      UsefulTask(al.test("JS", ""), "Test all projects in Scala 2.13+Scala JS").alias("test-js-2_13"),
      UsefulTask(al.test("Native", "3"), "Test all projects in Scala 3+Scala Native").alias("test-native-3"),
      UsefulTask(al.test("Native", ""), "Test all projects in Scala 2.13+Scala Native").alias("test-native-2_13"),
      UsefulTask(
        al.publishLocalForTests,
        "Publishes all Scala 2.13 and Scala 3 JVM artifacts to test snippets in documentation"
      )
        .alias("publish-local-for-tests")
    )
  )

lazy val fastShowPretty = projectMatrix
  .in(file("fast-show-pretty"))
  .someVariations(versions.scalas, versions.platforms)((useCrossQuotes ++ only1VersionInIDE) *)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-fast-show-pretty",
    name := "kindlings-fast-show-pretty",
    description := "Fast Show Pretty type class demonstrating how to use Hearth to implement a type class derivation"
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(versionSchemeSettings *)
  .settings(publishSettings *)

lazy val circeDerivation = projectMatrix
  .in(file("circe-derivation"))
  .someVariations(versions.scalas, versions.platforms)((useCrossQuotes ++ only1VersionInIDE) *)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-circe-derivation",
    name := "kindlings-circe-derivation",
    description := "Circe Encoder/Decoder derivation using Hearth macros"
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(versionSchemeSettings *)
  .settings(publishSettings *)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core" % versions.circe,
      "io.circe" %%% "circe-parser" % versions.circe % Test
    )
  )

lazy val jsoniterDerivation = projectMatrix
  .in(file("jsoniter-derivation"))
  .someVariations(versions.scalas, versions.platforms)((useCrossQuotes ++ only1VersionInIDE) *)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-jsoniter-derivation",
    name := "kindlings-jsoniter-derivation",
    description := "Jsoniter Scala JsonValueCodec derivation using Hearth macros"
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(versionSchemeSettings *)
  .settings(publishSettings *)
  .settings(
    libraryDependencies ++= Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % versions.jsoniterScala
    )
  )

lazy val jsoniterJson = projectMatrix
  .in(file("jsoniter-json"))
  .someVariations(versions.scalas, versions.platforms)(only1VersionInIDE *)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-jsoniter-json",
    name := "kindlings-jsoniter-json",
    description := "Minimal JSON AST with optics and JsonValueCodec for jsoniter-scala"
  )
  .settings(settings *)
  .settings(versionSchemeSettings *)
  .settings(publishSettings *)
  .settings(
    libraryDependencies ++= Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % versions.jsoniterScala,
      "com.kubuszok" %%% "hearth-munit" % versions.hearth % Test
    ),
    libraryDependencies ++= versions.fold(scalaVersion.value)(
      for3 = Seq.empty,
      for2_13 = Seq(
        compilerPlugin("org.typelevel" % "kind-projector" % versions.kindProjector cross CrossVersion.full)
      )
    ),
    resolvers += mavenCentralSnapshots
  )

lazy val yamlDerivation = projectMatrix
  .in(file("yaml-derivation"))
  .someVariations(versions.scalas, versions.platforms)((useCrossQuotes ++ only1VersionInIDE) *)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-yaml-derivation",
    name := "kindlings-yaml-derivation",
    description := "Scala-YAML YamlEncoder/YamlDecoder derivation using Hearth macros"
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(versionSchemeSettings *)
  .settings(publishSettings *)
  .settings(
    libraryDependencies ++= Seq(
      "org.virtuslab" %%% "scala-yaml" % versions.scalaYaml
    )
  )

lazy val avroDerivation = projectMatrix
  .in(file("avro-derivation"))
  .someVariations(versions.scalas, List(VirtualAxis.jvm))((useCrossQuotes ++ only1VersionInIDE) *)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-avro-derivation",
    name := "kindlings-avro-derivation",
    description := "Apache Avro AvroSchemaFor/AvroEncoder/AvroDecoder derivation using Hearth macros"
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(versionSchemeSettings *)
  .settings(publishSettings *)
  .settings(
    libraryDependencies ++= Seq(
      "org.apache.avro" % "avro" % versions.avro
    )
  )
