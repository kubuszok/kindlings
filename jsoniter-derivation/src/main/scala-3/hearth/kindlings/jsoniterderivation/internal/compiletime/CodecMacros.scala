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

  /** Hearth's `semiEval` cannot resolve references to local `val`s. Scala 3 binds every non-inline argument of an
    * inline method (`derived[A](using JsoniterConfig.default.withX)`) to such a local proxy `val`, so without this only
    * stable module references (e.g. `JsoniterConfig.default`) would be evaluated at compile time. Follow immutable
    * local `val`s (never `var`s) to their right-hand side, whose value is the same as the reference's.
    */
  override protected def semiEvalConfig(configExpr: Expr[JsoniterConfig]): Option[JsoniterConfig] = {
    import q.reflect.*
    def unwrap(t: Term): Term = t match {
      case Inlined(_, _, inner) => unwrap(inner)
      case Typed(inner, _)      => unwrap(inner)
      case other                => other
    }
    // Hearth's `semiEval` reads `var`s by reflection, from classes compiled earlier (e.g. by a previous incremental
    // run): the value it sees is not the runtime value. Configs referencing a `var` are only known at runtime.
    def readsVar(term: Term): Boolean =
      new TreeAccumulator[Boolean] {
        def foldTree(found: Boolean, tree: Tree)(owner: Symbol): Boolean =
          found || (tree match {
            case ref: Ref if ref.symbol.flags.is(Flags.Mutable) => true
            case _                                              => foldOverTree(found, tree)(owner)
          })
      }.foldTree(false, term)(Symbol.spliceOwner)
    // Hearth's `semiEval` also reads members of classes/objects by reflection, which only works when their class was
    // compiled by an EARLIER run: for members compiled in this run the result would depend on the incremental state.
    // Such references are never evaluated reflectively; immutable vals among them are followed to their right-hand
    // side below instead (the same value, available in both cases).
    def refsCurrentRunMember(term: Term): Boolean =
      new TreeAccumulator[Boolean] {
        def foldTree(found: Boolean, tree: Tree)(owner: Symbol): Boolean =
          found || (tree match {
            case ref: Ref if ref.symbol.isDefinedInCurrentRun && ref.symbol.owner.isClassDef => true
            case _ => foldOverTree(found, tree)(owner)
          })
      }.foldTree(false, term)(Symbol.spliceOwner)
    // Only vals whose value cannot be overridden: local vals, and final or object member vals (a non-final class/trait
    // val could be overridden, so its right-hand side is not necessarily its value).
    def isFollowable(sym: Symbol): Boolean =
      !sym.flags.is(Flags.Mutable) && !sym.flags.is(Flags.Lazy) && !sym.flags.is(Flags.Method) && (
        !sym.owner.isClassDef || sym.flags.is(Flags.Final) || sym.owner.flags.is(Flags.Module)
      )
    def loop(expr: Expr[JsoniterConfig], depth: Int): Option[JsoniterConfig] =
      (if readsVar(expr.asTerm) || refsCurrentRunMember(expr.asTerm) then None else super.semiEvalConfig(expr)).orElse {
        unwrap(expr.asTerm) match {
          case ref: Ref if depth > 0 && isFollowable(ref.symbol) =>
            scala.util.Try(ref.symbol.tree).toOption.collect { case ValDef(_, _, Some(rhs)) => rhs }.flatMap { rhs =>
              loop(rhs.asExpr.asInstanceOf[Expr[JsoniterConfig]], depth - 1)
            }
          case _ => None
        }
      }
    loop(configExpr, depth = 3)
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
