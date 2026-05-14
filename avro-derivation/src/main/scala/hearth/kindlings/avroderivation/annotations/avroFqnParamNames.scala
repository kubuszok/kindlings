package hearth.kindlings.avroderivation.annotations

import scala.annotation.StaticAnnotation

/** Encodes fully-qualified type parameter names into the Avro schema name, replacing dots with underscores. This
  * prevents collisions when two types with the same short name but different packages are used as type parameters.
  *
  * By default, Kindlings uses short names: `GenericClass[com.foo.Value, com.bar.Value]` gets schema name
  * `"GenericClass__Value__Value"` (collision). With this annotation, it becomes
  * `"GenericClass__com_foo_Value__com_bar_Value"` (unambiguous).
  *
  * Overridden by `@avroErasedName`, which strips type parameters entirely.
  *
  * Example: `@avroFqnParamNames case class Wrapper[A, B](a: A, b: B)`
  */
final class avroFqnParamNames extends StaticAnnotation
