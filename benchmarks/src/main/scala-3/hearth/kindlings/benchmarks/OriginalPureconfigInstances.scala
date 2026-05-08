package hearth.kindlings.benchmarks

import pureconfig.{ConfigReader, ConfigWriter}

object OriginalPureconfigSemiAutoInstances {

  given addressReader: ConfigReader[Address] = ConfigReader.derived
  given simpleCCReader: ConfigReader[SimpleCC] = ConfigReader.derived
  given personReader: ConfigReader[Person] = ConfigReader.derived

  // pureconfig does not provide ConfigWriter.derived on Scala 3;
  // use kindlings writer to generate ConfigValues, then compare readers only
  val simpleCCWriter: ConfigWriter[SimpleCC] = KindlingsPureconfigInstances.simpleCCWriter
  val personWriter: ConfigWriter[Person] = KindlingsPureconfigInstances.personWriter
}
