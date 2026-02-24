package hearth.kindlings.avroderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import org.apache.avro.Schema
import scala.reflect.macros.blackbox

final private[avroderivation] class SchemaForMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AnnotationSupportScala2
    with SchemaForMacrosImpl {

  def deriveSchemaForImpl[A: c.WeakTypeTag](
      config: c.Expr[AvroConfig]
  ): c.Expr[AvroSchemaFor[A]] = deriveSchemaForTypeClass[A](config)

  def deriveInlineSchemaImpl[A: c.WeakTypeTag](
      config: c.Expr[AvroConfig]
  ): c.Expr[Schema] = deriveInlineSchema[A](config)
}
