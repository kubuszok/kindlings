package hearth.kindlings.dicats
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox
import cats.effect.kernel.Resource

final private[dicats] class ResourceWiringMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with ResourceWiringMacrosImpl {

  protected def companionResourceExplicitParamTypes(
      companion: Expr_??,
      methodName: String,
      fType: ??,
      expected: ??
  ): Option[List[??]] = {
    import c.universe.*
    import companion.Underlying as Comp
    val fTpe: c.Type = fType.asUntyped
    val expectedTpe: c.Type = expected.asUntyped
    // The companion tree from `Type.companionObject` is untyped on Scala 2 (`tree.tpe` is null), so read the owner type
    // from the existential's `Underlying` (the companion object's type) rather than the tree.
    val ownerTpe: c.Type = Type[Comp].asUntyped
    val memberSym = ownerTpe.member(TermName(methodName))
    val alts = if (memberSym == NoSymbol) Nil else memberSym.alternatives
    // For each `resource` alternative: substitute our `F` for the method's type parameter, then walk the clauses,
    // skipping IMPLICIT/`using` clauses (summoned later) and collecting EXPLICIT ones. We only support a single explicit
    // clause; the (F-substituted) result must conform to `expected` (`Resource[F, <:T]`).
    def tryOne(info: c.Type): Option[List[??]] = info match {
      case PolyType(List(fSym), body) =>
        def sub(tp: c.Type): c.Type = tp.substituteTypes(List(fSym), List(fTpe))
        def collect(tpe: c.Type, clauses: List[List[c.Type]]): (List[List[c.Type]], c.Type) = tpe match {
          case MethodType(params, res) =>
            val isImplicit = params.nonEmpty && params.head.isImplicit
            if (isImplicit) collect(res, clauses)
            else collect(res, clauses :+ params.map(p => sub(p.typeSignature)))
          case NullaryMethodType(res) => collect(res, clauses)
          case other                  => (clauses, sub(other))
        }
        val (clauses, result) = collect(body, Nil)
        if (clauses.sizeIs <= 1 && result <:< expectedTpe) Some(clauses.flatten.map(t => UntypedType.as_??(t)))
        else None
      case _ => None
    }
    alts.iterator.map(m => tryOne(m.infoIn(ownerTpe))).collectFirst { case Some(r) => r }
  }

  protected def companionResourceCall(
      companion: Expr_??,
      methodName: String,
      fType: ??,
      expected: ??,
      explicitArgs: List[Expr_??]
  ): Option[Expr_??] = {
    import c.universe.*
    val companionTree: Tree = companion.value.tree
    val fTpe: c.Type = fType.asUntyped
    val expectedTpe: c.Type = expected.asUntyped
    val argTrees: List[Tree] = explicitArgs.map(_.value.tree)
    // `(companion.resource[F](args)): Resource[F, T]` — the applied `args` fill the explicit value clause; the ascription
    // drives the compiler to insert any `implicit Sync[F]` clause via its own implicit search; typecheck validates the
    // whole thing (and yields `None` if unsuitable).
    val base = q"$companionTree.${TermName(methodName)}[$fTpe]"
    val applied = if (argTrees.isEmpty) base else q"$base(..$argTrees)"
    val ascribed = q"($applied): $expectedTpe"
    scala.util.Try(c.typecheck(ascribed)).toOption.map(typed => UntypedExpr.as_??(typed))
  }

  def wireResourceImpl[F[_], T](dependencies: c.Tree*)(implicit
      ft: c.WeakTypeTag[F[Any]],
      tt: c.WeakTypeTag[T]
  ): c.Expr[Resource[F, T]] = {
    // Recover the `F` type constructor from the `F[Any]` WeakTypeTag (HKT type params are not directly tagged on 2.13).
    val fCtor: Type.Ctor1[F] = Type.Ctor1.fromUntyped[F](ft.tpe.typeConstructor)
    // `UntypedExpr.as_??(tree)` reads each dependency's PRECISE static type (e.g. `Resource[F, X]`, `Config`), unlike
    // `c.Expr[Any](tree)` which would erase it to `Any` and break classification/resolution.
    // On Scala 2 `Type[A] = c.WeakTypeTag[A]`, so `tt` already is the `Type[T]` we need.
    wireResource[F, T](dependencies.toList.map(t => UntypedExpr.as_??(t)))(tt, fCtor)
  }
}
