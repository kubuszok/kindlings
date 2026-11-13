package hearth.kindlings.ubjsonderivation.annotations

import scala.annotation.StaticAnnotation

/** Overrides the name of an ADT leaf: a case class, a case object or a Scala 3 enum case (the discriminator value, the
  * wrapper key, or the string of a case object). As jsoniter-scala's `@named` on classes: the name is used as is,
  * `adtLeafClassNameMapper` does not apply to it.
  *
  * On a field (constructor parameter) it renames the field, as `@fieldName` (jsoniter-scala uses `@named` for both).
  */
final class named(val name: String) extends StaticAnnotation
