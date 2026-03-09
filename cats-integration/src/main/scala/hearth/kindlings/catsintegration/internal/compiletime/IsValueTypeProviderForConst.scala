package hearth.kindlings.catsintegration.internal.compiletime

import hearth.fp.data.NonEmptyList
import hearth.MacroCommons
import hearth.std.{ProviderResult, StandardMacroExtension, StdExtensions}

/** IsValueType provider for `cats.data.Const[A, B]`.
  *
  * `Const[A, B]` wraps a value of type `A`, with `B` as a phantom type parameter. Unwrapping extracts the `A` value,
  * wrapping creates a `Const[A, B]` from an `A` value (always succeeds — no validation needed).
  */
final class IsValueTypeProviderForConst extends StandardMacroExtension { loader =>

  @scala.annotation.nowarn("msg=is never used")
  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsValueType.registerProvider(new IsValueType.Provider {

      override def name: String = loader.getClass.getName

      private lazy val ConstCtor = {
        val impl = Type.Ctor2.of[cats.data.Const]
        Type.Ctor2.fromUntyped[cats.data.Const](impl.asUntyped)
      }

      @scala.annotation.nowarn("msg=is never used")
      override def parse[A](tpe: Type[A]): ProviderResult[IsValueType[A]] =
        ConstCtor.unapply(tpe) match {
          case Some((inner, phantom)) =>
            import inner.Underlying as Inner
            implicit val AT: Type[A] = tpe

            val unwrapExpr: Expr[A] => Expr[Inner] = outerExpr =>
              Expr.quote {
                Expr.splice(outerExpr).asInstanceOf[cats.data.Const[Inner, Any]].getConst
              }

            val plainCtor = CtorLikeOf.PlainValue[Inner, A](
              ctor = innerExpr =>
                Expr.quote {
                  cats.data.Const(Expr.splice(innerExpr)).asInstanceOf[A]
                },
              method = None
            )

            ProviderResult.Matched(
              Existential[IsValueTypeOf[A, *], Inner](
                new IsValueTypeOf[A, Inner] {
                  override val unwrap: Expr[A] => Expr[Inner] = unwrapExpr
                  override val wrap: CtorLikeOf[Inner, A] = plainCtor
                  override lazy val ctors: CtorLikes[A] = NonEmptyList.one(
                    Existential[CtorLikeOf[*, A], Inner](plainCtor)
                  )
                }
              )
            )

          case None => skipped(s"${tpe.prettyPrint} is not a Const type")
        }
    })
  }
}
