package hearth.kindlings.tapirschemaderivation.internal.runtime

import hearth.kindlings.tapirschemaderivation.KindlingsSchema
import sttp.tapir.Schema

object TapirSchemaDerivationFactories {

  /** KindlingsSchema remembering the (compile-time) name of the type it was derived for.
    *
    * Used to name type parameters of generic types, when their schema is only available through a `KindlingsSchema[A]`
    * evidence (e.g. `derives KindlingsSchema` on a generic type) and the schema itself has no name (e.g. primitives).
    */
  final class NamedKindlingsSchema[A](val schema: Schema[A], val typeName: String) extends KindlingsSchema[A]

  def instance[A](schemaValue: Schema[A]): KindlingsSchema[A] =
    new KindlingsSchema[A] {
      def schema: Schema[A] = schemaValue
    }

  def instance[A](schemaValue: Schema[A], typeName: String): KindlingsSchema[A] =
    new NamedKindlingsSchema[A](schemaValue, typeName)
}
