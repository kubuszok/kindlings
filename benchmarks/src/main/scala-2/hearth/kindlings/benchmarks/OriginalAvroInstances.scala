package hearth.kindlings.benchmarks

import com.sksamuel.avro4s.RecordFormat

object OriginalAvroAutoInstances {
  val simpleCCFormat: RecordFormat[SimpleCC] = RecordFormat[SimpleCC]
  val personFormat: RecordFormat[Person] = RecordFormat[Person]
}
