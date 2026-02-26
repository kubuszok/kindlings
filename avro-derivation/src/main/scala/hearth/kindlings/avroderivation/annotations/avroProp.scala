package hearth.kindlings.avroderivation.annotations

import scala.annotation.StaticAnnotation

/** Adds a custom key-value property to the Avro schema. Can be applied to both types (record-level)
  * and fields. Multiple `@avroProp` annotations can be stacked on the same target.
  *
  * Example: `@avroProp("logicalType", "phone-number") phoneNumber: String`
  */
final class avroProp(val key: String, val value: String) extends StaticAnnotation
