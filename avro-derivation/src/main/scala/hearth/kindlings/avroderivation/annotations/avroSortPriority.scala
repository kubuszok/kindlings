package hearth.kindlings.avroderivation.annotations

import scala.annotation.StaticAnnotation

/** Controls the ordering of subtypes within an Avro ENUM or UNION schema. Higher priority values appear first. Subtypes
  * without this annotation default to priority 0.
  *
  * Example: `@avroSortPriority(10) case class Foo(...) extends MySealedTrait` — Foo appears before lower-priority
  * subtypes.
  */
final class avroSortPriority(val priority: Int) extends StaticAnnotation
