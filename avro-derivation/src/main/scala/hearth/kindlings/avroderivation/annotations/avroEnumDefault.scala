package hearth.kindlings.avroderivation.annotations

import scala.annotation.StaticAnnotation

/** Specifies a default value for an Avro enum type. The value must be one of the enum's symbol names.
  *
  * Example: `@avroEnumDefault("Red") sealed trait Color`
  */
final class avroEnumDefault(val value: String) extends StaticAnnotation
