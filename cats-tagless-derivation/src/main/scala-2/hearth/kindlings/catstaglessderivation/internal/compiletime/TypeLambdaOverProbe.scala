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
    case scala2: hearth.MacroCommonsScala2 =>
      val c = scala2.c
      import c.universe.*
      val probe = c.universe.typeOf[CatsTaglessFactories.KProbe[Any]].typeConstructor.typeSymbol
      val body = tpe.asInstanceOf[c.Type]
      val param =
        c.internal.newTypeSymbol(NoSymbol, TypeName(c.freshName("F")), NoPosition, Flag.PARAM | Flag.DEFERRED)
      val inner = c.internal.newTypeSymbol(param, TypeName("X"), NoPosition, Flag.PARAM | Flag.DEFERRED)
      c.internal.setInfo(inner, c.internal.typeBounds(definitions.NothingTpe, definitions.AnyTpe))
      c.internal.setInfo(
        param,
        c.internal.polyType(List(inner), c.internal.typeBounds(definitions.NothingTpe, definitions.AnyTpe))
      )
      val substituted = body.map {
        case TypeRef(_, sym, args) if sym == probe => c.internal.typeRef(NoPrefix, param, args)
        case other                                 => other
      }
      c.internal.polyType(List(param), substituted)
    case _ => throw new IllegalStateException("TypeLambdaOverProbe: expected Scala 2 macro commons")
  }

  /** `T` of a by-name type `=> T` (the type itself otherwise). */
  def byNameUnderlying(commons: hearth.MacroCommons)(tpe: Any): Any = commons match {
    case scala2: hearth.MacroCommonsScala2 =>
      val c = scala2.c
      val t = tpe.asInstanceOf[c.Type]
      if (t.typeSymbol == c.universe.definitions.ByNameParamClass) t.typeArgs.head else t
    case _ => tpe
  }
}
