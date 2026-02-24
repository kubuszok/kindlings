package hearth.kindlings.avroderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import org.apache.avro.Schema
import scala.quoted.*

final private[avroderivation] class SchemaForMacros(q: Quotes) extends MacroCommonsScala3(using q), SchemaForMacrosImpl
private[avroderivation] object SchemaForMacros {

  def deriveSchemaForImpl[A: Type](
      config: Expr[AvroConfig]
  )(using q: Quotes): Expr[AvroSchemaFor[A]] =
    new SchemaForMacros(q).deriveSchemaForTypeClass[A](config)

  def deriveInlineSchemaImpl[A: Type](
      config: Expr[AvroConfig]
  )(using q: Quotes): Expr[Schema] =
    new SchemaForMacros(q).deriveInlineSchema[A](config)
}
