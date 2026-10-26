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
  * `Foo.Type` is an abstract type member, so neither the `AnyVal` nor the opaque-type provider can see it. This one
  * finds the object `Foo` with `Type.pseudoCompanionObject` (the prefix of the type member `Foo.Type`) and reads the
  * wrapped type from the result type of its `unsafeUnwrap: Coercible[Type, Repr]` member, which both encodings always
  * generate (unlike `apply`, which the Scala 2 macro only generates for a `case class`). That type also proves that the
  * matched type IS `Foo.Type` rather than some other type member of `Foo`.
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

      // `Coercible[From, To]` - `unsafeUnwrap`'s result type names both the newtype and its wrapped type.
      private lazy val CoercibleCtor = Type.Ctor2.of[io.estatico.newtype.Coercible]

      @scala.annotation.nowarn("msg=is never used")
      override def parse[A](tpe: Type[A]): ProviderResult[IsValueType[A]] = {
        implicit val AT: Type[A] = tpe
        reprOf[A] match {
          case Some(inner) =>
            import inner.Underlying as Inner

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
      }

      /** If `A` is `Foo.Type[Args]` of a `@newtype`/`@newsubtype` companion `Foo`, returns `Foo.Repr[Args]`. */
      private def reprOf[A: Type]: Option[??] =
        Type.pseudoCompanionObject[A].flatMap { companion =>
          import companion.{Underlying as Companion, value as companionExpr}
          // A generic newtype's `unsafeUnwrap[T1, ...]` takes the newtype's type parameters, in order.
          val typeArgs = Type.typeArguments[A]
          Class[Companion]
            .method("unsafeUnwrap")
            .iterator
            .flatMap { method =>
              method
                .fold(
                  onInstance = _ => companionExpr.as_??,
                  onTypes = applyTypes => applyTypes.typeParams.flatten.zip(typeArgs).toMap,
                  onValues = _ => Map.empty
                )
                .toOption
                .flatMap(coercion => CoercibleCtor.unapply(coercion.Underlying))
                .collect {
                  case (from, to) if from.Underlying =:= Type[A] => UntypedType.dealias(to.Underlying.asUntyped).as_??
                }
            }
            .nextOption()
        }
    })
  }
}
