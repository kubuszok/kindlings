package hearth.kindlings.ironintegration.internal.compiletime

import hearth.fp.data.NonEmptyList
import hearth.MacroCommons
import hearth.std.{ProviderResult, StandardMacroExtension, StdExtensions}

final class IsValueTypeProviderForIron extends StandardMacroExtension { loader =>

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsValueType.registerProvider(new IsValueType.Provider {

      override def name: String = loader.getClass.getName

      // Use fromUntyped to avoid cross-compilation-boundary issues with Type.Ctor2.of.
      // The Impl (from .of) captures scala.quoted.Type[HKT] at compile time, which may
      // resolve to a different TypeRepr path than the one seen during macro expansion in
      // downstream projects. FromUntypedImpl uses =:= and baseType for matching, which
      // handles path differences correctly.
      private lazy val IronTypeCtor = {
        val impl = Type.Ctor2.of[io.github.iltotore.iron.IronType]
        Type.Ctor2.fromUntyped[io.github.iltotore.iron.IronType](impl.asUntyped)
      }

      @scala.annotation.nowarn("msg=is never used")
      override def parse[A](tpe: Type[A]): ProviderResult[IsValueType[A]] =
        IronTypeCtor.unapply(tpe) match {
          case Some((inner, constraint)) =>
            import inner.Underlying as Inner
            import constraint.Underlying as Constr
            implicit val AT: Type[A] = tpe

            // Construct Type[RuntimeConstraint[Inner, Constr]] for summoning
            // NOT implicit to avoid Type.of bootstrap cycle
            val runtimeConstraintType: Type[io.github.iltotore.iron.RuntimeConstraint[Inner, Constr]] =
              Type.Ctor2.of[io.github.iltotore.iron.RuntimeConstraint].apply[Inner, Constr]

            // Unwrap: IronType is opaque — underlying value IS the same type at runtime
            val unwrapExpr: Expr[A] => Expr[Inner] = outerExpr =>
              Expr.quote { Expr.splice(outerExpr).asInstanceOf[Inner] }

            // Wrap (runtime): validate via RuntimeConstraint[Inner, C].test + .message
            val eitherCtor = CtorLikeOf.EitherStringOrValue[Inner, A](
              ctor = innerExpr => {
                // Summon RuntimeConstraint[Inner, Constr] at macro expansion time
                val constraintExpr = Expr.summonImplicit(using runtimeConstraintType).toOption.getOrElse(
                  throw new RuntimeException(
                    s"No RuntimeConstraint instance found for IronType[${Type[Inner].prettyPrint}, ${Type[Constr].prettyPrint}]"
                  )
                )
                Expr.quote {
                  val v = Expr.splice(innerExpr)
                  val c = Expr.splice(constraintExpr)
                  if (c.test(v)) Right(v.asInstanceOf[A])
                  else Left("Should satisfy " + c.message)
                }
              },
              method = None
            )

            ProviderResult.Matched(Existential[IsValueTypeOf[A, *], Inner](
              new IsValueTypeOf[A, Inner] {
                override val unwrap: Expr[A] => Expr[Inner] = unwrapExpr
                override val wrap: CtorLikeOf[Inner, A] = eitherCtor
                override lazy val ctors: CtorLikes[A] = NonEmptyList.one(
                  Existential[CtorLikeOf[*, A], Inner](eitherCtor)
                )
              }
            ))

          case None => skipped(s"${tpe.prettyPrint} is not an Iron type")
        }
    })
  }
}
