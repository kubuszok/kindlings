import sbt.VirtualAxis

// Dependency and Scala/platform versions for the build.
//
// This lives in `project/` (the meta-build) rather than as an anonymous `new { ... }` value in
// build.sbt: under sbt 2.0 the build definition is compiled with Scala 3, where the members of an
// anonymous refinement are only reachable via structural/reflective selection and most `versions.*`
// lookups fail to resolve. A top-level `object` in build.sbt does not work either, because sbt
// compiles top-level definitions in a `.sbt` file into a separate synthetic object that the settings
// expressions cannot see. Defining it in the meta-build makes `versions` a normal, stably-imported
// object.
object versions {
  // Versions we are publishing for.
  val scala213 = "2.13.18"
  val scala3 = "3.8.4"

  // Which versions should be cross-compiled for publishing.
  val scalas = List(scala213, scala3)
  val platforms = List(VirtualAxis.jvm, VirtualAxis.js, VirtualAxis.native)

  // Dependencies.
  val hearth = "0.3.1-55-g61e5238-SNAPSHOT"
  val kindProjector = "0.13.4"
  val avro = "1.12.1"
  val avro4s213 = "4.1.2"
  val avro4s3 = "5.0.15"
  val cats = "2.13.0"
  val circe = "0.14.16"
  val iron = "3.3.1"
  val jsoniterScala = "2.38.16"
  val kittens = "3.5.0"
  val pureconfig = "0.17.10"
  val quicklens = "1.9.12"
  val tapir = "1.13.23"
  val refined = "0.11.3"
  val sttpApispec = "0.11.10"
  val catsEffect = "3.7.0"
  val tagging = "2.3.5"
  val scalacheck = "1.19.0"
  val scalaJavaTime = "2.7.0"
  val scalaSaxParser = "0.1.1"
  val scalaYaml = "0.3.1"
  val scalaXml = "2.4.0"
  val catsTagless = "0.16.5"
  val sconfig = "1.12.4"
}
