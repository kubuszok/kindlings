package hearth.kindlings.catsintegration.internal.compiletime

import hearth.MacroCommons
import hearth.std.{ProviderResult, StandardMacroExtension, StdExtensions}

/** IsEither provider for `cats.data.Validated[E, A]`.
  *
  * Maps `Validated[E, A]` to `IsEither` with `LeftValue = E` (Invalid) and `RightValue = A` (Valid). Since
  * `ValidatedNel[E, A]` is just `Validated[NonEmptyList[E], A]`, it is handled automatically.
  *
  * Uses `Validated.toEither` / `Validated.fromEither` for fold operations and construction via runtime helpers to avoid
  * Scala 2 reification issues with Validated's companion methods.
  */
final class IsEitherProviderForValidated extends StandardMacroExtension { loader =>

  @scala.annotation.nowarn("msg=is never used")
  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsEither.registerProvider(new IsEither.Provider {

      override def name: String = loader.getClass.getName

      private lazy val ValidatedCtor = {
        val impl = Type.Ctor2.of[cats.data.Validated]
        Type.Ctor2.fromUntyped[cats.data.Validated](impl.asUntyped)
      }

      @scala.annotation.nowarn("msg=is never used")
      private def mkIsEither[A, E0, A0](
          tpe: Type[A],
          errType: Type[E0],
          valType: Type[A0]
      ): IsEither[A] = {
        implicit val aType: Type[A] = tpe
        implicit val eType: Type[E0] = errType
        implicit val a0Type: Type[A0] = valType

        val impl = new IsEitherOf[A, E0, A0] {

          override def left(leftValue: Expr[E0]): Expr[A] =
            Expr.quote {
              cats.data.Validated.invalid[E0, A0](Expr.splice(leftValue)).asInstanceOf[A]
            }

          override def right(rightValue: Expr[A0]): Expr[A] =
            Expr.quote {
              cats.data.Validated.valid[E0, A0](Expr.splice(rightValue)).asInstanceOf[A]
            }

          override def fold[B: Type](
              either: Expr[A]
          )(onLeft: Expr[E0] => Expr[B], onRight: Expr[A0] => Expr[B]): Expr[B] =
            Expr.quote {
              Expr
                .splice(either)
                .asInstanceOf[cats.data.Validated[E0, A0]]
                .fold[B](
                  Expr.splice(LambdaBuilder.of1[E0]("left").buildWith(onLeft)),
                  Expr.splice(LambdaBuilder.of1[A0]("right").buildWith(onRight))
                )
            }

          override def getOrElse(either: Expr[A])(default: Expr[A0]): Expr[A0] =
            Expr.quote {
              Expr.splice(either).asInstanceOf[cats.data.Validated[E0, A0]].getOrElse(Expr.splice(default))
            }

          override def orElse(either: Expr[A])(default: Expr[A]): Expr[A] =
            Expr.quote {
              Expr
                .splice(either)
                .asInstanceOf[cats.data.Validated[E0, A0]]
                .orElse(Expr.splice(default).asInstanceOf[cats.data.Validated[E0, A0]])
                .asInstanceOf[A]
            }
        }

        new IsEither[A] {
          override type LeftValue = E0
          implicit override val LeftValue: Type[LeftValue] = errType
          override type RightValue = A0
          implicit override val RightValue: Type[RightValue] = valType
          override val value: IsEitherOf[A, E0, A0] = impl
        }
      }

      @scala.annotation.nowarn("msg=is never used")
      override def parse[A](tpe: Type[A]): ProviderResult[IsEither[A]] =
        ValidatedCtor.unapply(tpe) match {
          case Some((errType, valType)) =>
            ProviderResult.Matched(mkIsEither(tpe, errType.Underlying, valType.Underlying))
          case None => skipped(s"${tpe.prettyPrint} is not a Validated type")
        }
    })
  }
}
