package hearth.kindlings.benchmarks

import org.apache.avro.generic.GenericRecord

// avro4s 5.x Scala 3 semi-auto has incompatible API; delegate to auto instances
object OriginalAvroSemiAutoInstances {
  val simpleCCFormat: Avro4sCompat[SimpleCC] = OriginalAvroAutoInstances.simpleCCFormat
  val personFormat: Avro4sCompat[Person] = OriginalAvroAutoInstances.personFormat
}
