package hearth.kindlings.avroderivation.annotations

import scala.annotation.StaticAnnotation

/** Controls the ordering of subtypes within an Avro ENUM or UNION schema. Lower priority values appear first. Subtypes
  * without this annotation default to priority 0.
  *
  * Example: `@avroSortPriority(1) case class Foo(...) extends MySealedTrait`
  */
final class avroSortPriority(val priority: Int) extends StaticAnnotation
