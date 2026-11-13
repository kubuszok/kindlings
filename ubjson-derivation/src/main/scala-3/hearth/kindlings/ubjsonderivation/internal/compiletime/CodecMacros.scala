package hearth.kindlings.ubjsonderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[ubjsonderivation] class CodecMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      AnnotationSupport,
      CodecMacrosImpl {

  /** `T | Null` (jsoniter-scala's "nullable" types): `T` without the `Null` member(s). */
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
private[ubjsonderivation] object CodecMacros {

  def deriveCodecImpl[A: Type](
      config: Expr[UBJsonConfig]
  )(using q: Quotes): Expr[UBJsonValueCodec[A]] =
    new CodecMacros(q).deriveCodecTypeClass[A](config)
}
