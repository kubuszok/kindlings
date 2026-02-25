package hearth.kindlings.avroderivation.annotations

import scala.annotation.StaticAnnotation

/** Specifies a default value for an Avro field as a JSON string.
  *
  * The JSON string is parsed at runtime by the Avro library. Examples:
  *   - `@avroDefault("null")` for Option fields
  *   - `@avroDefault("0")` for Int fields
  *   - `@avroDefault("\"hello\"")` for String fields
  *   - `@avroDefault("[]")` for List fields
  */
final class avroDefault(val json: String) extends StaticAnnotation
