package hearth.kindlings.avroderivation.annotations

import scala.annotation.StaticAnnotation

/** Sets the position of a subtype within an Avro ENUM or UNION schema: subtypes are ordered by ascending position (as
  * avro4s' `@AvroUnionPosition`). Unlike `@avroSortPriority`, new subtypes can be appended with the next position
  * without renumbering the existing ones, which keeps the schema backward compatible.
  *
  * `@avroSortPriority` on the same subtype takes precedence. A subtype with neither annotation sorts after the
  * positioned ones.
  *
  * Example: `@avroUnionPosition(0) case object Unknown extends Fruit`
  */
final class avroUnionPosition(val position: Int) extends StaticAnnotation
