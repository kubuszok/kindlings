package hearth.kindlings.benchmarks

import com.sksamuel.avro4s.RecordFormat

object OriginalAvroInstances {
  val simpleCCFormat: RecordFormat[SimpleCC] = RecordFormat[SimpleCC]
  val personFormat: RecordFormat[Person] = RecordFormat[Person]
}
