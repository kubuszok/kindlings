package hearth.kindlings.avroderivation.annotations

import scala.annotation.StaticAnnotation

/** Disables generic type parameter name encoding in the Avro schema name. When applied to a type, the schema name will
  * use only the base class name without type parameter suffixes.
  *
  * By default, Kindlings encodes type parameters into the Avro record name (e.g. `Box[Int]` gets schema name
  * `"Box__Int"`). This annotation suppresses that encoding, producing just `"Box"`.
  *
  * Example: `@avroErasedName case class Box[A](value: A)`
  */
final class avroErasedName extends StaticAnnotation
