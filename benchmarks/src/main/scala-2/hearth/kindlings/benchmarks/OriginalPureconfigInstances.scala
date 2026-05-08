package hearth.kindlings.benchmarks

import pureconfig.{ConfigReader, ConfigWriter}
import pureconfig.generic.semiauto.*

object OriginalPureconfigSemiAutoInstances {

  implicit val addressReader: ConfigReader[Address] = deriveReader[Address]
  implicit val addressWriter: ConfigWriter[Address] = deriveWriter[Address]

  implicit val simpleCCReader: ConfigReader[SimpleCC] = deriveReader[SimpleCC]
  implicit val simpleCCWriter: ConfigWriter[SimpleCC] = deriveWriter[SimpleCC]

  implicit val personReader: ConfigReader[Person] = deriveReader[Person]
  implicit val personWriter: ConfigWriter[Person] = deriveWriter[Person]
}
