package hearth.kindlings.tapirschemaderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import hearth.kindlings.jsonfieldconfigext.JsonFieldConfigSupport
import sttp.tapir.Schema
import scala.quoted.*

final private[tapirschemaderivation] class SchemaMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      AnnotationSupportScala3,
      JsonFieldConfigSupport,
      SchemaMacrosImpl
private[tapirschemaderivation] object SchemaMacros {

  def deriveSchemaImpl[A: Type](using q: Quotes): Expr[Schema[A]] =
    new SchemaMacros(q).deriveSchema[A]

  def deriveKindlingsSchemaImpl[A: Type](using q: Quotes): Expr[KindlingsSchema[A]] =
    new SchemaMacros(q).deriveKindlingsSchema[A]
}
