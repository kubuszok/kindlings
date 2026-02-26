package hearth.kindlings.avroderivation.annotations

import scala.annotation.StaticAnnotation

/** Adds an alias to the Avro schema for schema evolution. Can be applied to both types (record-level) and fields.
  * Multiple `@avroAlias` annotations can be stacked on the same target.
  *
  * Example: `@avroAlias("OldName") case class NewName(...)`
  */
final class avroAlias(val alias: String) extends StaticAnnotation
