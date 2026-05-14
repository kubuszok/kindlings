package hearth.kindlings.avroderivation.annotations

import scala.annotation.StaticAnnotation

/** Overrides the Avro schema name for a type. Takes highest priority over all other naming annotations
  * (`@avroErasedName`, `@avroFqnParamNames`, and the default type-parameter encoding).
  *
  * Example: `@avroName("MyCustomRecord") case class Foo(x: Int)`
  */
final class avroName(name: String) extends StaticAnnotation
