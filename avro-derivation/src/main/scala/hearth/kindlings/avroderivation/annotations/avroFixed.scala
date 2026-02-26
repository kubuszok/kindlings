package hearth.kindlings.avroderivation.annotations

import scala.annotation.StaticAnnotation

/** Specifies that an `Array[Byte]` field should use Avro's FIXED type with the given size, instead of the default BYTES
  * type.
  *
  * The annotation is only valid on fields of type `Array[Byte]`. Using it on other types will result in a compile-time
  * error.
  *
  * Example: `@avroFixed(16) token: Array[Byte]`
  */
final class avroFixed(val size: Int) extends StaticAnnotation
