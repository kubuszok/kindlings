package hearth.kindlings.benchmarks

import pureconfig.{ConfigReader, ConfigWriter}
import pureconfig.generic.auto.*

object OriginalPureconfigAutoInstances {
  val simpleCCReader: ConfigReader[SimpleCC] = implicitly
  val simpleCCWriter: ConfigWriter[SimpleCC] = implicitly
  val personReader: ConfigReader[Person] = implicitly
  val personWriter: ConfigWriter[Person] = implicitly
}
