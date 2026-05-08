package hearth.kindlings.benchmarks

import pureconfig.{ConfigReader, ConfigWriter}

// pureconfig on Scala 3 does not provide an auto-derivation import;
// ConfigReader.derived is the only mechanism and it requires intermediates
// for nested types, so it is semi-auto. Delegate to semi-auto instances.
object OriginalPureconfigAutoInstances {
  val simpleCCReader: ConfigReader[SimpleCC] = OriginalPureconfigSemiAutoInstances.simpleCCReader
  val simpleCCWriter: ConfigWriter[SimpleCC] = OriginalPureconfigSemiAutoInstances.simpleCCWriter
  val personReader: ConfigReader[Person] = OriginalPureconfigSemiAutoInstances.personReader
  val personWriter: ConfigWriter[Person] = OriginalPureconfigSemiAutoInstances.personWriter
}
