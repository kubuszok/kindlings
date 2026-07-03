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

  protected def companionResourceCall(
      companion: Expr_??,
      methodName: String,
      fType: ??,
      expected: ??
  ): Option[Expr_??] = {
    import quotes.reflect.*
    val companionTerm = companion.value.asTerm
    val fRepr = fType.asUntyped
    // Apply our `F`, then apply each remaining IMPLICIT/`using` clause by running the compiler's implicit search on its
    // (now `F`-substituted) parameter types. A remaining EXPLICIT clause means the method needs arguments we cannot
    // supply here, so we bail (`None`) and let ordinary construction take over.
    def applyClauses(term: Term): Option[Term] = term.tpe.widen match {
      case mt: MethodType if mt.isImplicit =>
        val maybeArgs = mt.paramTypes.foldRight(Option(List.empty[Term])) { (pt, acc) =>
          acc.flatMap { rest =>
            Implicits.search(pt) match {
              case iss: ImplicitSearchSuccess => Some(iss.tree :: rest)
              case _                          => None
            }
          }
        }
        maybeArgs.flatMap(args => applyClauses(Apply(term, args)))
      case _: MethodType => None
      case _             => Some(term)
    }
    companionTerm.tpe.typeSymbol.methodMember(methodName).headOption.flatMap { sym =>
      scala.util
        .Try(companionTerm.select(sym).appliedToType(fRepr))
        .toOption
        .flatMap(applyClauses)
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
