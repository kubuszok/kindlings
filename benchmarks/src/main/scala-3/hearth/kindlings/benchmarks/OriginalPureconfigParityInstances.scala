package hearth.kindlings.benchmarks

import pureconfig.{ConfigReader, ConfigWriter}
import pureconfig.generic.semiauto.{deriveReader, deriveWriter}

object OriginalPureconfigParityInstances {

  given endpointReader: ConfigReader[(String, Int)] = deriveReader
  given endpointWriter: ConfigWriter[(String, Int)] = deriveWriter
  given windowReader: ConfigReader[(Int, Long, Boolean)] = deriveReader
  given windowWriter: ConfigWriter[(Int, Long, Boolean)] = deriveWriter

  val tuplesReader: ConfigReader[PureconfigTuples] = deriveReader
  val tuplesWriter: ConfigWriter[PureconfigTuples] = deriveWriter
  val optionsReader: ConfigReader[PureconfigOptions] = deriveReader
  val optionsWriter: ConfigWriter[PureconfigOptions] = deriveWriter
}
