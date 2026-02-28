package hearth.kindlings.avroderivation.annotations

import scala.annotation.StaticAnnotation

/** Disables generic type parameter name encoding in the Avro schema name. When applied to a type, the schema name will
  * use only the base class name without type parameter suffixes.
  *
  * Note: Kindlings currently uses erased names by default (e.g. `Box[Int]` gets schema name `"Box"`), so this
  * annotation currently serves as explicit documentation of intent for compatibility with avro4s.
  *
  * Example: `@avroErasedName case class Box[A](value: A)`
  */
final class avroErasedName extends StaticAnnotation
