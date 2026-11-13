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

  /** Hearth's `semiEval` cannot resolve references to local `val`s. Scala 3 binds every non-inline argument of an
    * inline method (`derived[A](using UBJsonConfig.default.withX)`) to such a local proxy `val`, so without this only
    * stable module references (e.g. `UBJsonConfig.default`) would be evaluated at compile time. Follow immutable local
    * `val`s (never `var`s) to their right-hand side, whose value is the same as the reference's (as
    * jsoniter-derivation's `CodecMacros`).
    */
  override protected def semiEvalConfig(configExpr: Expr[UBJsonConfig]): Option[UBJsonConfig] = {
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
    def loop(expr: Expr[UBJsonConfig], depth: Int): Option[UBJsonConfig] =
      (if readsVar(expr.asTerm) || refsCurrentRunMember(expr.asTerm) then None else super.semiEvalConfig(expr)).orElse {
        unwrap(expr.asTerm) match {
          case ref: Ref if depth > 0 && isFollowable(ref.symbol) =>
            scala.util.Try(ref.symbol.tree).toOption.collect { case ValDef(_, _, Some(rhs)) => rhs }.flatMap { rhs =>
              loop(rhs.asExpr.asInstanceOf[Expr[UBJsonConfig]], depth - 1)
            }
          case _ => None
        }
      }
    loop(configExpr, depth = 3)
  }
}
private[ubjsonderivation] object CodecMacros {

  def deriveCodecImpl[A: Type](
      config: Expr[UBJsonConfig]
  )(using q: Quotes): Expr[UBJsonValueCodec[A]] =
    new CodecMacros(q).deriveCodecTypeClass[A](config)
}
