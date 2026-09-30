package hearth.kindlings.newtypeintegration.internal.compiletime

import hearth.fp.data.NonEmptyList
import hearth.MacroCommons
import hearth.std.{ProviderResult, StandardMacroExtension, StdExtensions}

/** Teaches every Kindlings derivation to treat a [[https://github.com/estatico/scala-newtype scala-newtype]]
  * `@newtype`/`@newsubtype` as a value type: unwrap to the wrapped (`Repr`) type on encoding, re-wrap on decoding.
  *
  * `@newtype case class Foo(value: A)` expands (via the macro annotation on Scala 2.13 and via the
  * [[https://github.com/kubuszok/scala-newtype-compat scala-newtype-compat]] compiler plugin on Scala 3) into
  * {{{
  * type Foo = Foo.Type
  * object Foo {
  *   type Repr = A
  *   type Base = ...
  *   trait Tag extends Any
  *   type Type <: Base with Tag
  *   ...
  * }
  * }}}
  * `Foo.Type` is an abstract type member, so neither the `AnyVal` nor the opaque-type provider can see it - this one
  * matches that shape structurally (see [[NewtypeReprPlatform]]).
  *
  * Newtypes are zero-cost wrappers without validation (the same `Coercible` casts the library itself uses), so both
  * directions are plain casts.
  */
final class IsValueTypeProviderForNewtype extends StandardMacroExtension { loader =>

  override def priority: Int = 1000

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsValueType.registerProvider(new IsValueType.Provider {

      override def name: String = loader.getClass.getName

      @scala.annotation.nowarn("msg=is never used")
      override def parse[A](tpe: Type[A]): ProviderResult[IsValueType[A]] =
        NewtypeReprPlatform.reprOf(ctx)(UntypedType.fromTyped(using tpe)) match {
          case Some(reprType) =>
            val inner = UntypedType.as_??(reprType)
            import inner.Underlying as Inner
            implicit val AT: Type[A] = tpe

            // Newtype and its Repr share the runtime representation - this is what Coercible does.
            val unwrapExpr: Expr[A] => Expr[Inner] =
              outerExpr => Expr.quote(Expr.splice(outerExpr).asInstanceOf[Inner])

            val plainCtor = CtorLikeOf.PlainValue[Inner, A](
              ctor = innerExpr => Expr.quote(Expr.splice(innerExpr).asInstanceOf[A]),
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

          case None => skippedLazily(s"${tpe.prettyPrint} is not a scala-newtype @newtype/@newsubtype")
        }
    })
  }
}
