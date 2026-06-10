import sbtwelcome.UsefulTask
import commandmatrix.extra.*
import kubuszok.sbt._
import kubuszok.sbt.KubuszokPlugin.autoImport._

// Versions:

val versions = new {
  // Versions we are publishing for.
  val scala213 = "2.13.18"
  val scala3 = "3.8.4"

  // Which versions should be cross-compiled for publishing.
  val scalas = List(scala213, scala3)
  val platforms = List(VirtualAxis.jvm, VirtualAxis.js, VirtualAxis.native)

  // Dependencies.
  val hearth = "0.3.0-53-g69f9069-SNAPSHOT"
  val kindProjector = "0.13.4"
  val avro = "1.12.1"
  val avro4s213 = "4.1.2"
  val avro4s3 = "5.0.15"
  val cats = "2.13.0"
  val circe = "0.14.15"
  val iron = "3.3.1"
  val jsoniterScala = "2.38.14"
  val kittens = "3.5.0"
  val pureconfig = "0.17.10"
  val tapir = "1.13.19"
  val refined = "0.11.3"
  val scalacheck = "1.19.0"
  val scalaJavaTime = "2.6.0"
  val scalaSaxParser = "0.1.0"
  val scalaYaml = "0.3.1"
  val scalaXml = "2.4.0"
  val sconfig = "1.12.4"
}

val dev = new DevProperties(
  scala213 = Some(versions.scala213),
  scala3 = Some(versions.scala3),
  platforms = versions.platforms
)

val logCrossQuotes = dev.props.getProperty("log.cross-quotes") match {
  case "true"                          => true
  case "false"                         => false
  case otherwise if otherwise.nonEmpty => otherwise
  case _                               => !isCI
}

// Common settings:

// The hearth-cross-quotes:
//  - on Scala 2 are macros (defined for all platforms)
//  - and on Scala 3 are plugins (defined only for JVM).
val useCrossQuotes = versions.scalas.flatMap { scalaVersion =>
  foldVersion(scalaVersion)(
    for2_13 = List(
      // Enable logging from cross-quotes.
      MatrixAction
        .ForScala(_.isScala2)
        .Configure(_.settings(scalacOptions += s"-Xmacro-settings:hearth.cross-quotes.logging=$logCrossQuotes"))
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
                s"-P:hearth.cross-quotes:logging=$logCrossQuotes"
              )
          )
        )
    )
  )
}

val settings = Seq(
  scalacOptions ++= foldVersion(scalaVersion.value)(
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
  )
)

val dependencies = Seq(
  libraryDependencies ++= Seq(
    "com.kubuszok" %%% "hearth" % versions.hearth,
    "com.kubuszok" %%% "hearth-munit" % versions.hearth % Test
  ),
  libraryDependencies ++= foldVersion(scalaVersion.value)(
    for3 = Seq.empty,
    for2_13 = Seq(
      compilerPlugin("org.typelevel" % "kind-projector" % versions.kindProjector cross CrossVersion.full)
    )
  ),
  resolvers += Resolver.mavenLocal
)

val publishSettings = Seq(
  organization := "com.kubuszok",
  homepage := Some(url("https://kindlings.readthedocs.io")),
  organizationHomepage := Some(url("https://kubuszok.com")),
  licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/kubuszok/kindlings/"),
      "scm:git:git@github.com:kubuszok/kindlings.git"
    )
  ),
  startYear := Some(2026),
  developers := List(
    Developer("MateuszKubuszok", "Mateusz Kubuszok", "", url("https://kubuszok.com"))
  ),
  pomExtra := (
    <issueManagement>
      <system>GitHub issues</system>
      <url>https://github.com/kubuszok/kindlings/issues</url>
    </issueManagement>
  ),
  projectType := ProjectType.ScalaLibrary
)

val noPublishSettings =
  Seq(projectType := ProjectType.NonPublished)

// Modules

// Command generation

lazy val aliases = new Aliases(
  published = Seq(
    derivationCommons,
    fastShowPretty,
    circeDerivation,
    jsoniterDerivation,
    jsoniterJson,
    ubjsonDerivation,
    yamlDerivation,
    jsonSchemaConfigMacroProviders,
    tapirSchemaDerivation,
    refinedIntegration,
    ironIntegration,
    xmlDerivation,
    catsDerivation,
    scalacheckDerivation,
    catsIntegration,
    sconfigDerivation,
    diffDerivation,
    avroDerivation,
    pureconfigDerivation
  ),
  testOnly = Seq(integrationTests),
  compileOnly = Seq(benchmarks)
)

lazy val root = project
  .in(file("."))
  .settings(settings)
  .settings(publishSettings)
  .settings(noPublishSettings)
  .aggregate(derivationCommons.projectRefs *)
  .aggregate(fastShowPretty.projectRefs *)
  .aggregate(circeDerivation.projectRefs *)
  .aggregate(jsoniterDerivation.projectRefs *)
  .aggregate(jsoniterJson.projectRefs *)
  .aggregate(ubjsonDerivation.projectRefs *)
  .aggregate(yamlDerivation.projectRefs *)
  .aggregate(avroDerivation.projectRefs *)
  .aggregate(pureconfigDerivation.projectRefs *)
  .aggregate(sconfigDerivation.projectRefs *)
  .aggregate(jsonSchemaConfigMacroProviders.projectRefs *)
  .aggregate(tapirSchemaDerivation.projectRefs *)
  .aggregate(refinedIntegration.projectRefs *)
  .aggregate(ironIntegration.projectRefs *)
  .aggregate(xmlDerivation.projectRefs *)
  .aggregate(catsDerivation.projectRefs *)
  .aggregate(scalacheckDerivation.projectRefs *)
  .aggregate(catsIntegration.projectRefs *)
  .aggregate(diffDerivation.projectRefs *)
  .aggregate(integrationTests.projectRefs *)
  .aggregate(benchmarks.projectRefs *)
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
    usefulTasks := aliases.usefulTasks(
      publishLocalForTestsFilter = Some((_, platform) => platform == "JVM"),
      publishLocalForTestsDescription =
        "Publishes all Scala 2.13 and Scala 3 JVM artifacts to test snippets in documentation"
    )
  )

lazy val diffDerivation = projectMatrix
  .in(file("diff-derivation"))
  .someVariations(versions.scalas, versions.platforms)((useCrossQuotes ++ dev.only1VersionInIDE) *)
  .dependsOn(derivationCommons)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-diff-derivation",
    name := "kindlings-diff-derivation",
    description := "Structural Diff type class derivation using Myers algorithm with Hearth macros"
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(publishSettings *)

lazy val fastShowPretty = projectMatrix
  .in(file("fast-show-pretty"))
  .someVariations(versions.scalas, versions.platforms)((useCrossQuotes ++ dev.only1VersionInIDE) *)
  .dependsOn(derivationCommons)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-fast-show-pretty",
    name := "kindlings-fast-show-pretty",
    description := "Fast Show Pretty type class demonstrating how to use Hearth to implement a type class derivation"
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(publishSettings *)

lazy val circeDerivation = projectMatrix
  .in(file("circe-derivation"))
  .someVariations(versions.scalas, versions.platforms)((useCrossQuotes ++ dev.only1VersionInIDE) *)
  .dependsOn(derivationCommons, jsonSchemaConfigMacroProviders)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-circe-derivation",
    name := "kindlings-circe-derivation",
    description := "Circe Encoder/Decoder derivation using Hearth macros",
    macroExtensionTraits := Seq("hearth.kindlings.jsonschemaconfigs.JsonSchemaConfigExtension")
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(publishSettings *)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core" % versions.circe,
      "io.circe" %%% "circe-parser" % versions.circe % Test
    )
  )

lazy val jsoniterDerivation = projectMatrix
  .in(file("jsoniter-derivation"))
  .someVariations(versions.scalas, versions.platforms)((useCrossQuotes ++ dev.only1VersionInIDE) *)
  .dependsOn(derivationCommons, jsonSchemaConfigMacroProviders)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-jsoniter-derivation",
    name := "kindlings-jsoniter-derivation",
    description := "Jsoniter Scala JsonValueCodec derivation using Hearth macros",
    macroExtensionTraits := Seq("hearth.kindlings.jsonschemaconfigs.JsonSchemaConfigExtension")
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(publishSettings *)
  .settings(
    libraryDependencies ++= Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % versions.jsoniterScala
    )
  )
  .settings(
    Test / scalacOptions ++= {
      if (scalaBinaryVersion.value == "3")
        Seq(
          "-Xmacro-settings:hearth.mioBenchmarkScopes=true",
          s"-Xmacro-settings:hearth.mioBenchmarkFlameGraphDir=${crossTarget.value / "flame-graphs"}"
        )
      else Seq.empty
    }
  )

lazy val jsoniterJson = projectMatrix
  .in(file("jsoniter-json"))
  .someVariations(versions.scalas, versions.platforms)(dev.only1VersionInIDE *)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-jsoniter-json",
    name := "kindlings-jsoniter-json",
    description := "Minimal JSON AST with optics and JsonValueCodec for jsoniter-scala"
  )
  .settings(settings *)
  .settings(publishSettings *)
  .settings(
    libraryDependencies ++= Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % versions.jsoniterScala,
      "com.kubuszok" %%% "hearth-munit" % versions.hearth % Test
    ),
    libraryDependencies ++= foldVersion(scalaVersion.value)(
      for3 = Seq.empty,
      for2_13 = Seq(
        compilerPlugin("org.typelevel" % "kind-projector" % versions.kindProjector cross CrossVersion.full)
      )
    ),
    resolvers += Resolver.mavenLocal
  )

lazy val ubjsonDerivation = projectMatrix
  .in(file("ubjson-derivation"))
  .someVariations(versions.scalas, versions.platforms)((useCrossQuotes ++ dev.only1VersionInIDE) *)
  .dependsOn(derivationCommons)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-ubjson-derivation",
    name := "kindlings-ubjson-derivation",
    description := "UBJson (Universal Binary JSON) ValueCodec derivation using Hearth macros"
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(publishSettings *)

lazy val yamlDerivation = projectMatrix
  .in(file("yaml-derivation"))
  .someVariations(versions.scalas, versions.platforms)((useCrossQuotes ++ dev.only1VersionInIDE) *)
  .dependsOn(derivationCommons)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-yaml-derivation",
    name := "kindlings-yaml-derivation",
    description := "Scala-YAML YamlEncoder/YamlDecoder derivation using Hearth macros"
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(publishSettings *)
  .settings(
    libraryDependencies ++= Seq(
      "org.virtuslab" %%% "scala-yaml" % versions.scalaYaml
    )
  )

lazy val xmlDerivation = projectMatrix
  .in(file("xml-derivation"))
  .someVariations(versions.scalas, versions.platforms)((useCrossQuotes ++ dev.only1VersionInIDE) *)
  .dependsOn(derivationCommons)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-xml-derivation",
    name := "kindlings-xml-derivation",
    description := "Scala XML Encoder/Decoder derivation using Hearth macros"
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(publishSettings *)
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-xml" % versions.scalaXml,
      "com.kubuszok" %%% "scala-sax-parser" % versions.scalaSaxParser
    )
  )

lazy val avroDerivation = projectMatrix
  .in(file("avro-derivation"))
  .someVariations(versions.scalas, List(VirtualAxis.jvm))((useCrossQuotes ++ dev.only1VersionInIDE) *)
  .dependsOn(derivationCommons)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-avro-derivation",
    name := "kindlings-avro-derivation",
    description := "Apache Avro AvroSchemaFor/AvroEncoder/AvroDecoder derivation using Hearth macros"
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(publishSettings *)
  .settings(
    libraryDependencies ++= Seq(
      "org.apache.avro" % "avro" % versions.avro
    )
  )

lazy val pureconfigDerivation = projectMatrix
  .in(file("pureconfig-derivation"))
  .someVariations(versions.scalas, List(VirtualAxis.jvm))((useCrossQuotes ++ dev.only1VersionInIDE) *)
  .dependsOn(derivationCommons)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-pureconfig-derivation",
    name := "kindlings-pureconfig-derivation",
    description := "PureConfig ConfigReader/ConfigWriter/ConfigConvert derivation using Hearth macros"
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(publishSettings *)
  .settings(
    libraryDependencies ++= Seq(
      "com.github.pureconfig" %% "pureconfig-core" % versions.pureconfig
    )
  )

// sconfig pulls in `java.time.Duration` references that need a polyfill on Scala.js /
// Scala Native. Add scala-java-time as a test-time dependency on those platforms so the
// linker can resolve them.
val sconfigJavaTimePolyfill = List(
  MatrixAction
    .ForPlatform(VirtualAxis.js)
    .Configure(
      _.settings(
        libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % versions.scalaJavaTime % Test
      )
    ),
  MatrixAction
    .ForPlatform(VirtualAxis.native)
    .Configure(
      _.settings(
        libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % versions.scalaJavaTime % Test
      )
    )
)

lazy val sconfigDerivation = projectMatrix
  .in(file("sconfig-derivation"))
  .someVariations(versions.scalas, versions.platforms)(
    (useCrossQuotes ++ dev.only1VersionInIDE ++ sconfigJavaTimePolyfill) *
  )
  .dependsOn(derivationCommons)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-sconfig-derivation",
    name := "kindlings-sconfig-derivation",
    description := "sconfig (HOCON) ConfigReader/ConfigWriter/ConfigCodec derivation using Hearth macros — cross-platform"
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(publishSettings *)
  .settings(
    libraryDependencies ++= Seq(
      "org.ekrich" %%% "sconfig" % versions.sconfig
    )
  )

lazy val derivationCommons = projectMatrix
  .in(file("derivation-commons"))
  .someVariations(versions.scalas, versions.platforms)((useCrossQuotes ++ dev.only1VersionInIDE) *)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-derivation-commons",
    name := "kindlings-derivation-commons",
    description := "Shared compile-time utilities for Kindlings derivation modules"
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(publishSettings *)

lazy val jsonSchemaConfigMacroProviders = projectMatrix
  .in(file("json-schema-config-macro-providers"))
  .someVariations(versions.scalas, versions.platforms)((useCrossQuotes ++ dev.only1VersionInIDE) *)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-json-schema-config-macro-providers",
    name := "kindlings-json-schema-config-macro-providers",
    description := "Shared macro extension interface for JSON schema configuration discovery"
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(publishSettings *)

lazy val tapirSchemaDerivation = projectMatrix
  .in(file("tapir-schema-derivation"))
  .someVariations(versions.scalas, versions.platforms)((useCrossQuotes ++ dev.only1VersionInIDE) *)
  .dependsOn(derivationCommons, jsonSchemaConfigMacroProviders)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-tapir-schema-derivation",
    name := "kindlings-tapir-schema-derivation",
    description := "Tapir Schema derivation using Hearth macros with JSON-consistent naming"
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(publishSettings *)
  .settings(
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.tapir" %%% "tapir-core" % versions.tapir,
      "io.circe" %%% "circe-core" % versions.circe % Test,
      "io.circe" %%% "circe-parser" % versions.circe % Test,
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % versions.jsoniterScala % Test
    )
  )
  .dependsOn(circeDerivation % Test)
  .dependsOn(jsoniterDerivation % Test)

lazy val refinedIntegration = projectMatrix
  .in(file("refined-integration"))
  .someVariations(versions.scalas, versions.platforms)((useCrossQuotes ++ dev.only1VersionInIDE) *)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-refined-integration",
    name := "kindlings-refined-integration",
    description := "Refined types integration — IsValueType provider for eu.timepit.refined.api.Refined",
    macroExtensionTraits := Seq("hearth.std.StandardMacroExtension")
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(publishSettings *)
  .settings(libraryDependencies += "eu.timepit" %%% "refined" % versions.refined)

lazy val ironIntegration = projectMatrix
  .in(file("iron-integration"))
  .someVariations(List(versions.scala3), versions.platforms)((useCrossQuotes ++ dev.only1VersionInIDE) *)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-iron-integration",
    name := "kindlings-iron-integration",
    description := "Iron types integration — IsValueType provider for io.github.iltotore.iron.IronType",
    macroExtensionTraits := Seq("hearth.std.StandardMacroExtension")
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(publishSettings *)
  .settings(libraryDependencies += "io.github.iltotore" %%% "iron" % versions.iron)

lazy val catsDerivation = projectMatrix
  .in(file("cats-derivation"))
  .someVariations(versions.scalas, versions.platforms)((useCrossQuotes ++ dev.only1VersionInIDE) *)
  .dependsOn(derivationCommons)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-cats-derivation",
    name := "kindlings-cats-derivation",
    description := "Cats type class derivation (Show, Eq, Order, Hash, Semigroup, Monoid, etc.) using Hearth macros"
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(publishSettings *)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % versions.cats,
      "org.typelevel" %%% "alleycats-core" % versions.cats,
      "org.scalacheck" %%% "scalacheck" % versions.scalacheck % Test
    )
  )

lazy val scalacheckDerivation = projectMatrix
  .in(file("scalacheck-derivation"))
  .someVariations(versions.scalas, versions.platforms)((useCrossQuotes ++ dev.only1VersionInIDE) *)
  .dependsOn(derivationCommons)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-scalacheck-derivation",
    name := "kindlings-scalacheck-derivation",
    description := "ScalaCheck Arbitrary, Cogen, and Shrink derivation using Hearth macros"
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(publishSettings *)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalacheck" %%% "scalacheck" % versions.scalacheck
    )
  )

lazy val catsIntegration = projectMatrix
  .in(file("cats-integration"))
  .someVariations(versions.scalas, versions.platforms)((useCrossQuotes ++ dev.only1VersionInIDE) *)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-cats-integration",
    name := "kindlings-cats-integration",
    description := "Cats data types integration — IsCollection/IsMap providers for NonEmptyList, NonEmptyMap, Chain, etc.",
    macroExtensionTraits := Seq("hearth.std.StandardMacroExtension")
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(publishSettings *)
  .settings(libraryDependencies += "org.typelevel" %%% "cats-core" % versions.cats)

// Iron dependency added conditionally for Scala 3 only (ironIntegration has no Scala 2.13 rows)
// Avro and PureConfig are JVM-only — add as conditional deps for integration tests
val jvmOnlyDerivatonsForIntegrationTests = List(
  MatrixAction.ForPlatform(VirtualAxis.jvm).Configure { project =>
    val suffix = project.id.stripPrefix("integrationTests") // "" or "3"
    project
      .dependsOn(LocalProject(s"avroDerivation$suffix"))
      .dependsOn(LocalProject(s"pureconfigDerivation$suffix"))
      .settings(
        // Add scalajvm-3 test sources for JVM + Scala 3 only (Iron × Avro/PureConfig)
        Test / unmanagedSourceDirectories ++= foldVersion(scalaVersion.value)(
          for3 = Seq(baseDirectory.value / "src" / "test" / "scalajvm-3"),
          for2_13 = Seq.empty
        )
      )
  }
)

// Iron dependency added conditionally for Scala 3 only (ironIntegration has no Scala 2.13 rows)
val ironDepForScala3 = List(
  MatrixAction.ForScala(_.isScala3).Configure { project =>
    val suffix = project.id.stripPrefix("integrationTests") // "3", "JS3", "Native3"
    project.dependsOn(LocalProject(s"ironIntegration$suffix"))
  }
)

lazy val integrationTests = projectMatrix
  .in(file("integration-tests"))
  .someVariations(versions.scalas, versions.platforms)(
    (useCrossQuotes ++ dev.only1VersionInIDE ++ ironDepForScala3 ++ jvmOnlyDerivatonsForIntegrationTests) *
  )
  .disablePlugins(WelcomePlugin)
  .dependsOn(
    fastShowPretty,
    circeDerivation,
    jsoniterDerivation,
    ubjsonDerivation,
    yamlDerivation,
    xmlDerivation,
    sconfigDerivation,
    tapirSchemaDerivation,
    refinedIntegration,
    catsIntegration
  )
  .settings(noPublishSettings *)
  .settings(settings *)
  .settings(dependencies *)
  .settings(
    libraryDependencies ++= Seq(
      "eu.timepit" %%% "refined" % versions.refined,
      "io.circe" %%% "circe-core" % versions.circe,
      "io.circe" %%% "circe-parser" % versions.circe,
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % versions.jsoniterScala,
      "org.virtuslab" %%% "scala-yaml" % versions.scalaYaml,
      "com.softwaremill.sttp.tapir" %%% "tapir-core" % versions.tapir,
      "org.scala-lang.modules" %%% "scala-xml" % versions.scalaXml,
      "com.kubuszok" %%% "scala-sax-parser" % versions.scalaSaxParser,
      "org.typelevel" %%% "cats-core" % versions.cats,
      "org.ekrich" %%% "sconfig" % versions.sconfig
    ),
    libraryDependencies ++= foldVersion(scalaVersion.value)(
      for3 = Seq("io.github.iltotore" %%% "iron" % versions.iron),
      for2_13 = Seq.empty
    )
  )

lazy val benchmarks = projectMatrix
  .in(file("benchmarks"))
  .someVariations(versions.scalas, List(VirtualAxis.jvm))(dev.only1VersionInIDE *)
  .enablePlugins(JmhPlugin)
  .dependsOn(
    fastShowPretty,
    circeDerivation,
    jsoniterDerivation,
    catsDerivation,
    scalacheckDerivation,
    ubjsonDerivation,
    yamlDerivation,
    xmlDerivation,
    avroDerivation,
    pureconfigDerivation,
    sconfigDerivation,
    tapirSchemaDerivation
  )
  .disablePlugins(WelcomePlugin)
  .settings(noPublishSettings *)
  .settings(settings *)
  .settings(
    moduleName := "kindlings-benchmarks",
    name := "kindlings-benchmarks",
    description := "JMH benchmarks comparing Kindlings derivation against original library derivation",
    scalacOptions --= Seq("-Werror", "-Xfatal-warnings", "-Xcheck-macros"),
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-parser" % versions.circe,
      "io.circe" %% "circe-generic" % versions.circe,
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % versions.jsoniterScala % Provided,
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-circe" % versions.jsoniterScala,
      "org.typelevel" %% "kittens" % versions.kittens,
      "com.sksamuel.avro4s" %% "avro4s-core" % (if (scalaBinaryVersion.value == "3") versions.avro4s3
                                                else versions.avro4s213)
    ),
    libraryDependencies ++= foldVersion(scalaVersion.value)(
      for2_13 = Seq(
        compilerPlugin("org.typelevel" % "kind-projector" % versions.kindProjector cross CrossVersion.full),
        "com.github.pureconfig" %% "pureconfig-generic" % versions.pureconfig
      ),
      for3 = Seq(
        "com.github.pureconfig" %% "pureconfig-generic-scala3" % versions.pureconfig
      )
    )
  )
