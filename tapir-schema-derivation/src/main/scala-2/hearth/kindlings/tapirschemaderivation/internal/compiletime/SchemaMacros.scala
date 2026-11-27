package hearth.kindlings.tapirschemaderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import hearth.kindlings.jsonschemaconfigs.JsonSchemaConfigs
import sttp.tapir.Schema
import scala.reflect.macros.blackbox

final private[tapirschemaderivation] class SchemaMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AnnotationSupport
    with JsonSchemaConfigs
    with SchemaMacrosImpl {

  /** `Obj.Value` of a `scala.Enumeration`: the type of `Obj`. */
  override protected def enumerationObjectOf[A: Type]: Option[??] = {
    import c.universe.*
    UntypedType.fromTyped[A].dealias match {
      case TypeRef(pre, sym, Nil) if sym.name.toString == "Value" && pre.termSymbol.isModule =>
        Some(UntypedType.as_??(pre.asInstanceOf[UntypedType]))
      case _ => None
    }
  }

  def deriveSchemaImpl[A: c.WeakTypeTag]: c.Expr[Schema[A]] = deriveSchema[A]

  def deriveKindlingsSchemaImpl[A: c.WeakTypeTag]: c.Expr[KindlingsSchema[A]] = deriveKindlingsSchema[A]

  def deriveEnumerationImpl[A: c.WeakTypeTag]: c.Expr[Schema[A]] = deriveEnumeration[A]

  def deriveKindlingsSchemaEnumerationImpl[A: c.WeakTypeTag]: c.Expr[KindlingsSchema[A]] =
    deriveKindlingsSchemaEnumeration[A]
}
