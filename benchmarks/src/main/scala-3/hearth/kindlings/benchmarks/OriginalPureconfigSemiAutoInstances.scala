package hearth.kindlings.benchmarks

import pureconfig.{ConfigReader, ConfigWriter}
import pureconfig.generic.semiauto.{deriveReader, deriveWriter}

object OriginalPureconfigSemiAutoInstances {

  given addressReader: ConfigReader[Address] = deriveReader
  given addressWriter: ConfigWriter[Address] = deriveWriter

  given simpleCCReader: ConfigReader[SimpleCC] = deriveReader
  given simpleCCWriter: ConfigWriter[SimpleCC] = deriveWriter

  given personReader: ConfigReader[Person] = deriveReader
  given personWriter: ConfigWriter[Person] = deriveWriter
}
