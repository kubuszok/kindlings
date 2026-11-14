package hearth.kindlings.ironintegration.internal.compiletime

import hearth.fp.data.NonEmptyList
import hearth.MacroCommons
import hearth.std.{ProviderResult, StandardMacroExtension, StdExtensions}

final class IsValueTypeProviderForIron extends StandardMacroExtension { loader =>

  override def priority: Int = 1000

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsValueType.registerProvider(new IsValueType.Provider {

      override def name: String = loader.getClass.getName

      private lazy val IronTypeCtor = Type.Ctor2.of[io.github.iltotore.iron.IronType]
      // `object Key extends RefinedType[A, C]` / `RefinedSubtype[A, C]` (their common parent `Refined` is private[iron])
      private lazy val RefinedTypeCtor = Type.Ctor2.of[io.github.iltotore.iron.RefinedType]
      private lazy val RefinedSubtypeCtor = Type.Ctor2.of[io.github.iltotore.iron.RefinedSubtype]

      override def parse[A](tpe: Type[A]): ProviderResult[IsValueType[A]] =
        parseRefinedTypeCompanion(tpe).getOrElse(parseIronType(tpe))

      /** `Key.T` (or an alias of it) where `object Key extends RefinedType[A, C]` (or `RefinedSubtype[A, C]`): an
        * opaque type member `T = A :| C` of the companion `Key`, which does not unify with `IronType[A, C]` outside of
        * it. Unwrapped to `A`, wrapped (validated) with the companion's `either(value: A): Either[String, T]`, which
        * checks the companion's `RuntimeConstraint[A, C]`.
        */
      @scala.annotation.nowarn("msg=is never used")
      private def parseRefinedTypeCompanion[A](tpe: Type[A]): Option[ProviderResult[IsValueType[A]]] =
        if (!tpe.isOpaqueType) None
        else
          Type.pseudoCompanionObject(using tpe).flatMap { companion =>
            import companion.{Underlying as Companion, value as companionExpr}
            RefinedTypeCtor.unapply(Type[Companion]).orElse(RefinedSubtypeCtor.unapply(Type[Companion])).map {
              case (inner, _) =>
                import inner.Underlying as Inner
                implicit val AT: Type[A] = tpe
                // `Companion.either(value: Inner): Either[String, Companion.T]`, applied to the companion module. Its
                // result type also proves that `A` IS `Companion.T`, not some other type member of the companion.
                val eitherResult = Type.of[Either[String, A]]
                val eitherMethods: List[Method.ApplyValues] = Class[Companion]
                  .method("either")
                  .collect {
                    case m: Method.OnInstance if m.isUnary =>
                      m.apply(companionExpr.asInstanceOf[Expr[m.Instance]])
                  }
                  .collect {
                    case m: Method.ApplyValues if m.knownReturning.exists(_.Underlying =:= eitherResult) => m
                  }
                eitherMethods match {
                  case eitherMethod :: Nil => matchedRefinedType[A, Inner](eitherMethod)
                  case _                   =>
                    skippedLazily(
                      s"${tpe.prettyPrint} looks like an Iron RefinedType, but its companion " +
                        s"${Type.prettyPrint[Companion]} has no single `either(value): Either[String, ${tpe.prettyPrint}]`"
                    )
                }
            }
          }

      private def matchedRefinedType[A: Type, Inner: Type](
          eitherMethod: Method.ApplyValues
      ): ProviderResult[IsValueType[A]] = {
        val inputName = eitherMethod.totalParameters.flatten.head._1
        // NOT implicit to avoid Type.of bootstrap cycle
        val eitherType: Type[Either[String, A]] = Type.of[Either[String, A]]

        // Unwrap: `T` is opaque - its runtime representation is the underlying value
        val unwrapExpr: Expr[A] => Expr[Inner] =
          outerExpr => Expr.quote(Expr.splice(outerExpr).asInstanceOf[Inner])

        val eitherCtor = CtorLikeOf.EitherStringOrValue[Inner, A](
          ctor = innerExpr => {
            def fail(reason: String): Nothing =
              throw new IllegalStateException(s"Cannot call `either` of ${Type.prettyPrint[A]}'s companion: $reason")
            eitherMethod(Map(inputName -> innerExpr.as_??)) match {
              case r: Method.Result[?] =>
                r.build() match {
                  case Right(call) => UntypedExpr.toTyped[Either[String, A]](call.asUntyped)(using eitherType)
                  case Left(error) => fail(error)
                }
              case other => fail(s"unexpected ${other.getClass.getSimpleName} step after applying the argument")
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
      }

      @scala.annotation.nowarn("msg=is never used")
      private def parseIronType[A](tpe: Type[A]): ProviderResult[IsValueType[A]] =
        IronTypeCtor.unapply(tpe) match {
          case Some((inner, constraint)) =>
            import inner.Underlying as Inner
            import constraint.Underlying as Constr
            implicit val AT: Type[A] = tpe

            // Construct Type[RuntimeConstraint[Inner, Constr]] for summoning
            // NOT implicit to avoid Type.of bootstrap cycle
            val runtimeConstraintType: Type[io.github.iltotore.iron.RuntimeConstraint[Inner, Constr]] =
              Type.Ctor2.of[io.github.iltotore.iron.RuntimeConstraint].apply[Inner, Constr]

            // Summon RuntimeConstraint[Inner, Constr] at macro expansion time. Done eagerly
            // (not inside the ctor callback) so that a missing instance becomes an aggregated
            // skip reason instead of an exception crashing the compiler.
            Expr.summonImplicit(using runtimeConstraintType).toOption match {
              case None =>
                skippedLazily(
                  s"${tpe.prettyPrint} is an Iron type, but no RuntimeConstraint instance was found for IronType[${Type[Inner].prettyPrint}, ${Type[Constr].prettyPrint}] — cannot generate validation"
                )

              case Some(constraintExpr) =>
                // Unwrap: IronType is opaque — underlying value IS the same type at runtime
                val unwrapExpr: Expr[A] => Expr[Inner] =
                  outerExpr => Expr.quote(Expr.splice(outerExpr).asInstanceOf[Inner])

                // Wrap (runtime): validate via RuntimeConstraint[Inner, C].test + .message
                val eitherCtor = CtorLikeOf.EitherStringOrValue[Inner, A](
                  ctor = innerExpr =>
                    Expr.quote {
                      val v = Expr.splice(innerExpr)
                      val c = Expr.splice(constraintExpr)
                      if (c.test(v)) Right(v.asInstanceOf[A])
                      else Left("Should satisfy " + c.message)
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
            }

          case None => skippedLazily(s"${tpe.prettyPrint} is not an Iron type")
        }
    })
  }
}
