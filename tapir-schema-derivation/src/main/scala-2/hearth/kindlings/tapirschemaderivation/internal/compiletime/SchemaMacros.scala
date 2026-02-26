package hearth.kindlings.tapirschemaderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import hearth.kindlings.jsonschemaconfigs.JsonSchemaConfigs
import sttp.tapir.Schema
import scala.reflect.macros.blackbox

final private[tapirschemaderivation] class SchemaMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AnnotationSupportScala2
    with JsonSchemaConfigs
    with SchemaMacrosImpl {

  def deriveSchemaImpl[A: c.WeakTypeTag]: c.Expr[Schema[A]] = deriveSchema[A]

  def deriveKindlingsSchemaImpl[A: c.WeakTypeTag]: c.Expr[KindlingsSchema[A]] = deriveKindlingsSchema[A]
}
