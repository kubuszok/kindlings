package hearth.kindlings.benchmarks

import pureconfig.{ConfigReader, ConfigWriter}
import pureconfig.generic.semiauto.{deriveReader, deriveWriter}

object OriginalPureconfigAutoInstances {
  val simpleCCReader: ConfigReader[SimpleCC] = deriveReader
  val simpleCCWriter: ConfigWriter[SimpleCC] = deriveWriter
  // Person hits Scala 3 inline depth limit without intermediates;
  // delegate to semi-auto for complex types
  val personReader: ConfigReader[Person] = OriginalPureconfigSemiAutoInstances.personReader
  val personWriter: ConfigWriter[Person] = OriginalPureconfigSemiAutoInstances.personWriter
}
