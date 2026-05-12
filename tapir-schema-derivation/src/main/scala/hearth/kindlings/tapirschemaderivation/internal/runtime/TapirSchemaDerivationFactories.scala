package hearth.kindlings.tapirschemaderivation.internal.runtime

import hearth.kindlings.tapirschemaderivation.KindlingsSchema
import sttp.tapir.Schema

object TapirSchemaDerivationFactories {

  def instance[A](schemaValue: Schema[A]): KindlingsSchema[A] =
    new KindlingsSchema[A] {
      def schema: Schema[A] = schemaValue
    }
}
