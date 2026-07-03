package hearth.kindlings.dicats
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*
import cats.effect.kernel.Resource

final private[dicats] class ResourceWiringMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      ResourceWiringMacrosImpl {

  /** Create Type.Ctor1[G] — the cross-quotes plugin rewrites Type.Ctor1.of[G] here because MacroCommons is in scope. */
  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]

  protected def companionResourceExplicitParamTypes(
      companion: Expr_??,
      methodName: String,
      fType: ??,
      expected: ??
  ): Option[List[??]] = {
    import quotes.reflect.*
    val companionTerm = companion.value.asTerm
    val fRepr = fType.asUntyped
    val expectedRepr = expected.asUntyped
    // Walk the (F-applied) clauses: skip IMPLICIT/`using` clauses (summoned later in companionResourceCall), collect
    // EXPLICIT value clauses. We only support a single explicit clause; the final result must conform to `expected`.
    def collect(tpe: TypeRepr, clauses: List[List[TypeRepr]]): (List[List[TypeRepr]], TypeRepr) = tpe.widen match {
      case mt: MethodType if mt.isImplicit => collect(mt.resType, clauses)
      case mt: MethodType                  => collect(mt.resType, clauses :+ mt.paramTypes)
      case other                           => (clauses, other)
    }
    companionTerm.tpe.typeSymbol.methodMember(methodName).headOption.flatMap { sym =>
      scala.util.Try(companionTerm.select(sym).appliedToType(fRepr)).toOption.flatMap { applied =>
        val (clauses, result) = collect(applied.tpe.widen, Nil)
        if clauses.sizeIs <= 1 && result <:< expectedRepr then Some(clauses.flatten.map(UntypedType.as_??))
        else None
      }
    }
  }

  protected def companionResourceCall(
      companion: Expr_??,
      methodName: String,
      fType: ??,
      expected: ??,
      explicitArgs: List[Expr_??]
  ): Option[Expr_??] = {
    import quotes.reflect.*
    val companionTerm = companion.value.asTerm
    val fRepr = fType.asUntyped
    val argTerms: List[Term] = explicitArgs.map(_.value.asTerm)
    // Apply our `F`, then walk each remaining clause: an EXPLICIT clause consumes the next `n` resolved `argTerms`; an
    // IMPLICIT/`using` clause is filled by the compiler's implicit search on its (now `F`-substituted) parameter types.
    def applyClauses(term: Term, remaining: List[Term]): Option[Term] = term.tpe.widen match {
      case mt: MethodType if mt.isImplicit =>
        val maybeArgs = mt.paramTypes.foldRight(Option(List.empty[Term])) { (pt, acc) =>
          acc.flatMap { rest =>
            Implicits.search(pt) match {
              case iss: ImplicitSearchSuccess => Some(iss.tree :: rest)
              case _                          => None
            }
          }
        }
        maybeArgs.flatMap(args => applyClauses(Apply(term, args), remaining))
      case mt: MethodType =>
        val (these, rest) = remaining.splitAt(mt.paramTypes.size)
        if these.sizeIs < mt.paramTypes.size then None
        else applyClauses(Apply(term, these), rest)
      case _ => if remaining.isEmpty then Some(term) else None
    }
    companionTerm.tpe.typeSymbol.methodMember(methodName).headOption.flatMap { sym =>
      scala.util
        .Try(companionTerm.select(sym).appliedToType(fRepr))
        .toOption
        .flatMap(applyClauses(_, argTerms))
        .flatMap { fullyApplied =>
          import expected.Underlying as R
          scala.util.Try(fullyApplied.asExprOf[R]).toOption.map(_.as_??)
        }
    }
  }

  /** Split the (already inlined) varargs literal into the individual dependency expressions, each typed as `Any`. On
    * Scala 3 `Expr[A] = scala.quoted.Expr[A]` and `Type[A] = scala.quoted.Type[A]`, so the conversions are identities.
    */
  def splitDeps(dependencies: Expr[Seq[Any]]): List[Expr_??] = {
    import quotes.reflect.*
    dependencies match {
      // Read each dependency's PRECISE static type (e.g. `Resource[F, X]`, `Config`), unlike `_.as_??` with `Type[Any]`
      // which would erase it to `Any`. WIDEN the term type first: a reference like `dbResource` has the singleton type
      // `dbResource.type`, which has no type arguments and would defeat the `Resource[F, X]` / `F[X]` decomposition.
      case Varargs(exprs) =>
        exprs.toList.map { e =>
          val term = e.asTerm
          val widened = term.tpe.widen.asType
          widened match {
            case '[t] => '{ $e.asInstanceOf[t] }.asExprOf[t].as_??(using summon[scala.quoted.Type[t]])
          }
        }
      case _ =>
        Environment.reportErrorAndAbort("wireResource expects a literal varargs list of dependencies.")
    }
  }
}

private[dicats] object ResourceWiringMacros {

  def wireResourceImpl[F[_]: Type, T: Type](
      dependencies: Expr[Seq[Any]]
  )(using q: Quotes): Expr[Resource[F, T]] = {
    val m = new ResourceWiringMacros(q)
    // `summon[scala.quoted.Type[T]]` is exactly `m.Type[T]`, `m.mkCtor1[F]` supplies the F type constructor.
    m.wireResource[F, T](m.splitDeps(dependencies))(summon[Type[T]], m.mkCtor1[F])
  }
}
