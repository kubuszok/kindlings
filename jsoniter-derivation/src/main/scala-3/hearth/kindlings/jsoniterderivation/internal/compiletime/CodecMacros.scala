package hearth.kindlings.jsoniterderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReaderException, JsonValueCodec}
import scala.quoted.*

final private[jsoniterderivation] class CodecMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      AnnotationSupport,
      CodecMacrosImpl {

  /** Hearth does not decompose generic tuples (`H *: T`, `EmptyTuple`, match types like `Tuple.Concat` reducing to
    * them, tuples with more than 22 elements): read their element types from the type itself.
    */
  override protected def tupleElementTypes[A: Type]: Option[List[??]] = {
    import q.reflect.*
    val consSym = Symbol.requiredClass("scala.*:")
    def loop(t: TypeRepr): Option[List[TypeRepr]] = t.dealias.simplified.dealias match {
      case AppliedType(tycon, List(h, tl)) if tycon.typeSymbol == consSym  => loop(tl).map(h :: _)
      case AppliedType(tycon, args) if defn.isTupleClass(tycon.typeSymbol) => Some(args)
      case other if other =:= TypeRepr.of[EmptyTuple]                      => Some(Nil)
      case _                                                               => None
    }
    if !Type[A].isTuple || Type[A].isNamedTuple then None
    else loop(TypeRepr.of[A]).map(_.map(t => UntypedType.as_??(t.asInstanceOf[UntypedType])))
  }

  override protected def constructGenericTuple[A: Type](elements: List[Expr_??]): Option[Expr[A]] = {
    val boxed = elements.map { e =>
      import e.{Underlying as E, value as v}
      '{ ${ v.asInstanceOf[Expr[E]] }.asInstanceOf[Object] }
    }
    Some('{ scala.Tuple.fromArray(scala.Array[Object](${ Varargs(boxed) }*)).asInstanceOf[A] })
  }

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
private[jsoniterderivation] object CodecMacros {

  def deriveCodecImpl[A: Type](
      config: Expr[JsoniterConfig]
  )(using q: Quotes): Expr[JsonValueCodec[A]] =
    new CodecMacros(q).deriveCodecTypeClass[A](config)

  def deriveKindlingsCodecImpl[A: Type](
      config: Expr[JsoniterConfig]
  )(using q: Quotes): Expr[KindlingsJsonValueCodec[A]] =
    new CodecMacros(q).deriveCodecTypeClass[A](config)

  def deriveInlineWriteToStringImpl[A: Type](
      value: Expr[A],
      config: Expr[JsoniterConfig]
  )(using q: Quotes): Expr[String] =
    new CodecMacros(q).deriveInlineWriteToString[A](value, config)

  def deriveInlineReadFromStringImpl[A: Type](
      json: Expr[String],
      config: Expr[JsoniterConfig]
  )(using q: Quotes): Expr[Either[JsonReaderException, A]] =
    new CodecMacros(q).deriveInlineReadFromString[A](json, config)

  def deriveKeyCodecImpl[A: Type](
      config: Expr[JsoniterConfig]
  )(using q: Quotes): Expr[JsonKeyCodec[A]] =
    new CodecMacros(q).deriveKeyCodecTypeClass[A](config)
}
