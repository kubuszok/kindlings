package hearth.kindlings.derivation.compiletime

import hearth.MacroCommons
import hearth.std.*

/** Reusable zero-allocation construction of a value from N field-decode results typed as `Either[E, _]`.
  *
  * The naive way to combine N `Either` field results into a constructor is to put them in a `List`, `sequence` it into
  * an `Either[E, Array[Any]]`, then `map` a constructor over the array. That allocates a `List` (N cons cells), an
  * `Array[Any]`, and a closure on every decode — pure overhead on the hot path.
  *
  * [[buildEitherFailFast]] replaces that with nested [[IsEitherOf.fold]] calls. On Hearth's zero-closure `fold` (a
  * `match`, not `Either.fold(closure, closure)`) this lowers to a straight-line short-circuit with no intermediate
  * `List`/`Array`/closure:
  * {{{
  *   r0 match {
  *     case Right(v0) => r1 match {
  *       case Right(v1) => ... => Right(construct(v0, v1, ...))
  *       case Left(e)   => Left(e)
  *     }
  *     case Left(e) => Left(e)
  *   }
  * }}}
  * It also short-circuits decoding itself: a field is only decoded if all earlier fields succeeded.
  *
  * The construction goes through Hearth's `IsEither` std extension so the same code works for any `Either`-like error
  * type (circe's `DecodingFailure`, yaml's `ConstructError`, pureconfig's `ConfigReaderFailures`, …). Requires the
  * standard Scala-`Either` extension to be loaded (every derivation entry point loads it via
  * [[LoadStandardExtensionsOnce]]).
  */
trait EitherFieldsConstruct { this: MacroCommons & StdExtensions =>

  /** Combine per-field `Either[E, Any]` results into `Either[E, A]`, fail-fast (short-circuit on the first `Left`).
    *
    * @param fieldResults
    *   the per-field decode results in constructor-argument order; values are boxed to `Any` (the caller casts them
    *   back to the field type inside `construct`)
    * @param construct
    *   given the successfully-decoded `Right` values (in the same order), build the final `Expr[A]`
    */
  protected def buildEitherFailFast[E: Type, A: Type](
      fieldResults: List[Expr[Either[E, Any]]]
  )(construct: List[Expr[Any]] => Expr[A]): Expr[Either[E, A]] = {
    val EitherCtor = Type.Ctor2.of[Either]
    implicit val AnyT: Type[Any] = Type.of[Any]
    implicit val EitherEA: Type[Either[E, A]] = EitherCtor[E, A]
    implicit val EitherEAny: Type[Either[E, Any]] = EitherCtor[E, Any]

    def isEitherOf[L: Type, R: Type](implicit ET: Type[Either[L, R]]): IsEitherOf[Either[L, R], L, R] =
      IsEither.unapply(ET) match {
        case Some(isE) =>
          // The existential LeftValue/RightValue of the parsed instance are exactly L/R (we asked for Either[L, R]);
          // the cast just re-exposes them so `fold`/`left`/`right` line up with our `Expr[L]`/`Expr[R]`.
          isE.value.asInstanceOf[IsEitherOf[Either[L, R], L, R]]
        case None =>
          Environment.reportErrorAndAbort(
            s"EitherFieldsConstruct: ${ET.prettyPrint} is not recognized as Either (is the Scala Either std extension loaded?)"
          )
      }

    val resultEither = isEitherOf[E, A]
    val fieldEither = isEitherOf[E, Any]

    def loop(remaining: List[Expr[Either[E, Any]]], boundReversed: List[Expr[Any]]): Expr[Either[E, A]] =
      remaining match {
        case Nil =>
          resultEither.right(construct(boundReversed.reverse))
        case result :: rest =>
          fieldEither.fold[Either[E, A]](result)(
            onLeft = (e: Expr[E]) => resultEither.left(e),
            onRight = (value: Expr[Any]) => loop(rest, value :: boundReversed)
          )
      }

    loop(fieldResults, Nil)
  }
}
