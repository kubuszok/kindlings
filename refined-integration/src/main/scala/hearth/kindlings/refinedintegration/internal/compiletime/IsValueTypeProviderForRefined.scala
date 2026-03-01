package hearth.kindlings.refinedintegration.internal.compiletime

import hearth.fp.data.NonEmptyList
import hearth.MacroCommons
import hearth.std.{ProviderResult, StandardMacroExtension, StdExtensions}

final class IsValueTypeProviderForRefined extends StandardMacroExtension { loader =>

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsValueType.registerProvider(new IsValueType.Provider {

      override def name: String = loader.getClass.getName

      // Use fromUntyped to avoid cross-compilation-boundary issues with Type.Ctor2.of's Impl.
      // The Impl captures scala.quoted.Type[HKT] at compile time, which may resolve to a
      // different TypeRepr path than the one seen during macro expansion in downstream projects.
      // FromUntypedImpl uses =:= and baseType for matching, which handles path differences.
      private lazy val RefinedCtor = {
        val impl = Type.Ctor2.of[eu.timepit.refined.api.Refined]
        Type.Ctor2.fromUntyped[eu.timepit.refined.api.Refined](impl.asUntyped)
      }

      @scala.annotation.nowarn("msg=is never used")
      override def parse[A](tpe: Type[A]): ProviderResult[IsValueType[A]] =
        RefinedCtor.unapply(tpe) match {
          case Some((inner, pred)) =>
            import inner.Underlying as Inner
            import pred.Underlying as Pred
            implicit val AT: Type[A] = tpe

            // Construct Type[Validate[Inner, Pred]] for summoning later
            // NOT implicit to avoid Type.of bootstrap cycle
            val validateType: Type[eu.timepit.refined.api.Validate[Inner, Pred]] = {
              val impl = Type.Ctor2.of[eu.timepit.refined.api.Validate]
              Type.Ctor2.fromUntyped[eu.timepit.refined.api.Validate](impl.asUntyped).apply[Inner, Pred]
            }

            // Unwrap: use Refined.unapply which works on both Scala 2 (AnyVal with .value)
            // and Scala 3 (opaque type identity).
            // Cast to Refined[Inner, Pred] first since A is abstract.
            val unwrapExpr: Expr[A] => Expr[Inner] = outerExpr =>
              Expr.quote {
                eu.timepit.refined.api.Refined
                  .unapply(
                    Expr.splice(outerExpr).asInstanceOf[eu.timepit.refined.api.Refined[Inner, Pred]]
                  )
                  .get
              }

            val eitherCtor = CtorLikeOf.EitherStringOrValue[Inner, A](
              ctor = innerExpr => {
                // Summon Validate[Inner, Pred] at macro expansion time (user's call site)
                val validateExpr = Expr
                  .summonImplicit(using validateType)
                  .toOption
                  .getOrElse(
                    throw new RuntimeException(
                      s"No Validate instance found for Refined[${Type[Inner].prettyPrint}, ${Type[Pred].prettyPrint}]"
                    )
                  )
                // Use refineV which returns Either[String, Refined[Inner, Pred]] and works
                // on both Scala 2 (AnyVal) and Scala 3 (opaque type)
                Expr.quote {
                  val v = Expr.splice(innerExpr)
                  val validate = Expr.splice(validateExpr)
                  eu.timepit.refined.refineV[Pred].apply[Inner](v)(validate).asInstanceOf[Either[String, A]]
                }
              },
              method = None
            )

            ProviderResult.Matched(
              Existential[IsValueTypeOf[A, *], Inner](
                new IsValueTypeOf[A, Inner] {
                  override val unwrap: Expr[A] => Expr[Inner] = unwrapExpr
                  override val wrap: CtorLikeOf[Inner, A] = eitherCtor
                  override lazy val ctors: CtorLikes[A] = NonEmptyList.one(
                    Existential[CtorLikeOf[*, A], Inner](eitherCtor)
                  )
                }
              )
            )

          case None => skipped(s"${tpe.prettyPrint} is not a Refined type")
        }
    })
  }
}
