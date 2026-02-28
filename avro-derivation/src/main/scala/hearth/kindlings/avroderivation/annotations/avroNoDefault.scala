package hearth.kindlings.avroderivation.annotations

import scala.annotation.StaticAnnotation

/** Suppresses the default value for a field in the generated Avro schema. When present on a field that also has
  * `@avroDefault`, a compile-time error is produced.
  *
  * This is useful when you want to ensure a field never has a default in the schema, even if one might be inferred in
  * the future.
  */
final class avroNoDefault extends StaticAnnotation
