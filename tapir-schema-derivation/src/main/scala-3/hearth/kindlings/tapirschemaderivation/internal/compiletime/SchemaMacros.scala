package hearth.kindlings.tapirschemaderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import hearth.kindlings.jsonschemaconfigs.JsonSchemaConfigs
import sttp.tapir.Schema
import scala.quoted.*

final private[tapirschemaderivation] class SchemaMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      AnnotationSupport,
      JsonSchemaConfigs,
      SchemaMacrosImpl {

  /** `T | Null`: `T` without the `Null` member(s) (as jsoniter-derivation's `CodecMacros.nullableUnionUnderlying`). */
  override protected def nullableUnionUnderlying[A: Type]: Option[??] = {
    import q.reflect.*
    def members(t: TypeRepr): List[TypeRepr] = t.dealias match {
      case OrType(l, r) => members(l) ++ members(r)
      case other        => List(other)
    }
    val all = members(TypeRepr.of[A])
    val (nulls, others) = all.partition(_ =:= TypeRepr.of[Null])
    if nulls.isEmpty || others.isEmpty || all.size < 2 then None
    else {
      val underlying = others.reduceLeft((l, r) => OrType(l, r))
      Some(UntypedType.as_??(underlying.asInstanceOf[UntypedType]))
    }
  }
}
private[tapirschemaderivation] object SchemaMacros {

  def deriveSchemaImpl[A: Type](using q: Quotes): Expr[Schema[A]] =
    new SchemaMacros(q).deriveSchema[A]

  def deriveKindlingsSchemaImpl[A: Type](using q: Quotes): Expr[KindlingsSchema[A]] =
    new SchemaMacros(q).deriveKindlingsSchema[A]

  def deriveEnumerationImpl[A: Type](using q: Quotes): Expr[Schema[A]] =
    new SchemaMacros(q).deriveEnumeration[A]

  def deriveKindlingsSchemaEnumerationImpl[A: Type](using q: Quotes): Expr[KindlingsSchema[A]] =
    new SchemaMacros(q).deriveKindlingsSchemaEnumeration[A]
}
