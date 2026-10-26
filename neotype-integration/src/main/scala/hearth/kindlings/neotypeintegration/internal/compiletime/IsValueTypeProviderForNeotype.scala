package hearth.kindlings.neotypeintegration.internal.compiletime

import hearth.fp.data.NonEmptyList
import hearth.MacroCommons
import hearth.std.{ProviderResult, StandardMacroExtension, StdExtensions}

/** Teaches every Kindlings derivation to treat a [[https://github.com/kitlangton/neotype neotype]] `Newtype`/`Subtype`
  * as a value type: unwrap to the underlying type on encoding, validate and re-wrap with the companion's `make` on
  * decoding.
  *
  * neotype exposes its `Newtype.WithType` witness as a `transparent inline given`, which is invisible to the macro-time
  * implicit search performed by the "use implicit when available" rules - so a generic `given [A, B](using
  * Newtype.WithType[B, A], ...)` fallback would resolve at a normal call site but never inside a derivation macro. A
  * value-type provider sidesteps implicit search entirely by matching the type structurally at macro-expansion time.
  *
  * Registered with priority 1000, so it runs before Hearth's built-in opaque-type provider (priority -1000), which
  * would otherwise match the neotype's opaque `Type` and wrap it without running `validate`.
  */
final class IsValueTypeProviderForNeotype extends StandardMacroExtension { loader =>

  override def priority: Int = 1000

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsValueType.registerProvider(new IsValueType.Provider {

      override def name: String = loader.getClass.getName

      // Cheap sound pre-filter: every neotype `Type` is an opaque type, so a non-opaque type can never match.
      override def mightMatch[A](tpe: Type[A]): Boolean = tpe.isOpaqueType

      // The underlying type is the companion's `Newtype[A]`/`Subtype[A]` type argument - exactly one level, so that
      // a neotype over another neotype keeps the inner validation (`Type.opaqueUnderlyingType` would collapse the
      // whole chain of opaque types and skip it).
      private lazy val NewtypeCtor = Type.Ctor1.of[neotype.Newtype]
      private lazy val SubtypeCtor = Type.Ctor1.of[neotype.Subtype]

      // `Foo.Type` (and any alias of it) is an opaque type member of `object Foo extends Newtype[A]`: its pseudo
      // companion is `Foo`, the prefix of the type member.
      @scala.annotation.nowarn("msg=is never used")
      override def parse[A](tpe: Type[A]): ProviderResult[IsValueType[A]] =
        Type.pseudoCompanionObject(using tpe) match {
          case Some(companion) =>
            import companion.{Underlying as Companion, value as companionExpr}
            parseCompanion[A, Companion](companionExpr)(using tpe, Companion)
          case None => skippedLazily(s"${tpe.prettyPrint} is not a neotype Newtype/Subtype")
        }

      private def parseCompanion[A: Type, Companion: Type](
          companion: Expr[Companion]
      ): ProviderResult[IsValueType[A]] = {
        val underlying = NewtypeCtor.unapply(Type[Companion]).orElse(SubtypeCtor.unapply(Type[Companion]))
        // `Companion.make(input: Underlying): Either[String, Companion.Type]`, applied to the companion module. Its
        // result type also proves that `A` IS `Companion.Type`, not some other type member of the companion.
        val makeResult = Type.of[Either[String, A]]
        val make: List[Method.ApplyValues] = Class[Companion]
          .method("make")
          .collect {
            case m: Method.OnInstance if m.isUnary =>
              m.apply(companion.asInstanceOf[Expr[m.Instance]])
          }
          .collect {
            case m: Method.ApplyValues if m.knownReturning.exists(_.Underlying =:= makeResult) => m
          }
        (underlying, make) match {
          case (Some(inner), makeMethod :: Nil) =>
            import inner.Underlying as Inner
            matched[A, Inner](makeMethod)
          case _ =>
            skippedLazily(
              s"${Type.prettyPrint[A]} looks like a neotype, but its companion ${Type.prettyPrint[Companion]} has no " +
                s"single `make(input): Either[String, ${Type.prettyPrint[A]}]` method or no " +
                "`Newtype[A]`/`Subtype[A]` base type"
            )
        }
      }

      private def matched[A: Type, Inner: Type](makeMethod: Method.ApplyValues): ProviderResult[IsValueType[A]] = {
        val inputName = makeMethod.totalParameters.flatten.head._1

        // Unwrap: an opaque type has the same runtime representation as its underlying type.
        val unwrapExpr: Expr[A] => Expr[Inner] =
          outerExpr => Expr.quote(Expr.splice(outerExpr).asInstanceOf[Inner])

        // Wrap: `Companion.make(value)` runs the (possibly overridden) `validate` and returns
        // `Either[String, Companion.Type]`, i.e. `Either[String, A]`.
        val eitherCtor = CtorLikeOf.EitherStringOrValue[Inner, A](
          ctor = innerExpr => callMake[A](makeMethod, Map(inputName -> innerExpr.as_??)),
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

      private def callMake[A: Type](
          makeMethod: Method.ApplyValues,
          args: Map[String, Expr_??]
      ): Expr[Either[String, A]] = {
        // NOT implicit to avoid Type.of bootstrap cycle
        val eitherType: Type[Either[String, A]] = Type.of[Either[String, A]]
        def fail(reason: String): Nothing =
          throw new IllegalStateException(s"Cannot call `make` of neotype ${Type.prettyPrint[A]}: $reason")
        makeMethod(args) match {
          case r: Method.Result[?] =>
            r.build() match {
              // `Either[String, Companion.Type]` is `Either[String, A]` - `A` IS `Companion.Type`.
              case Right(call) => UntypedExpr.toTyped[Either[String, A]](call.asUntyped)(using eitherType)
              case Left(error) => fail(error)
            }
          case other => fail(s"unexpected ${other.getClass.getSimpleName} step after applying the argument")
        }
      }
    })
  }
}
