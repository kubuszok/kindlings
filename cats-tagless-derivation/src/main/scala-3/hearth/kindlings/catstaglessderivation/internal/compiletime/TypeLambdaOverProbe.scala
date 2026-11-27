package hearth.kindlings.catstaglessderivation.internal.compiletime

import hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories

/** Builds `[F[_]] =>> T[F]` from `T[KProbe]`: every occurrence of the probe constructor becomes the lambda's parameter.
  *
  * Hearth has no cross-platform API for building type lambdas (see
  * `docs/research/hearth-gap-type-lambda-construction.md`), so this is the per-version part of the derivation; it is
  * what cats-tagless' own macros do (`summonLambda` on Scala 3, `polyType` on Scala 2) to look up e.g.
  * `FunctorK[[F[_]] =>> EitherT[F, E, A]]`.
  */
private[compiletime] object TypeLambdaOverProbe {

  def apply(commons: hearth.MacroCommons)(tpe: Any): Any = commons match {
    case scala3: hearth.MacroCommonsScala3 =>
      given scala.quoted.Quotes = scala3.quotes
      import scala3.quotes.reflect.*
      val probe = TypeRepr.of[CatsTaglessFactories.KProbe].typeSymbol
      val body = tpe.asInstanceOf[TypeRepr]
      val hkBounds = TypeBounds(
        TypeRepr.of[Nothing],
        TypeLambda(List("X"), _ => List(TypeBounds.empty), _ => TypeRepr.of[Any])
      )
      TypeLambda(List("F"), _ => List(hkBounds), tl => body.substituteTypes(List(probe), List(tl.param(0))))
    case _ => throw new IllegalStateException("TypeLambdaOverProbe: expected Scala 3 macro commons")
  }

  /** `T` of a by-name type `=> T` (the type itself otherwise). */
  def byNameUnderlying(commons: hearth.MacroCommons)(tpe: Any): Any = commons match {
    case scala3: hearth.MacroCommonsScala3 =>
      import scala3.quotes.reflect.*
      tpe.asInstanceOf[TypeRepr] match {
        case ByNameType(underlying) => underlying
        case other                  => other
      }
    case _ => tpe
  }
}
