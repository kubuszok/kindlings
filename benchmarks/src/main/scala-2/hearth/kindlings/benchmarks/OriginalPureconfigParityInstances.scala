package hearth.kindlings.benchmarks

import pureconfig.{ConfigReader, ConfigWriter}
import pureconfig.generic.semiauto.*

object OriginalPureconfigParityInstances {

  implicit val endpointReader: ConfigReader[(String, Int)] = deriveReader[(String, Int)]
  implicit val endpointWriter: ConfigWriter[(String, Int)] = deriveWriter[(String, Int)]
  implicit val windowReader: ConfigReader[(Int, Long, Boolean)] = deriveReader[(Int, Long, Boolean)]
  implicit val windowWriter: ConfigWriter[(Int, Long, Boolean)] = deriveWriter[(Int, Long, Boolean)]

  val tuplesReader: ConfigReader[PureconfigTuples] = deriveReader[PureconfigTuples]
  val tuplesWriter: ConfigWriter[PureconfigTuples] = deriveWriter[PureconfigTuples]
  val optionsReader: ConfigReader[PureconfigOptions] = deriveReader[PureconfigOptions]
  val optionsWriter: ConfigWriter[PureconfigOptions] = deriveWriter[PureconfigOptions]
}
