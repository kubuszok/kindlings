package hearth.kindlings.catstaglessderivation

import scala.language.experimental.macros

object KindlingsInvariantK {
  def derived[Alg[_[_]]]: cats.tagless.InvariantK[Alg] =
    macro internal.compiletime.InvariantKMacros.deriveInvariantKImpl[Alg]
}

object KindlingsFunctorK {
  def derived[Alg[_[_]]]: cats.tagless.FunctorK[Alg] =
    macro internal.compiletime.FunctorKMacros.deriveFunctorKImpl[Alg]
}

object KindlingsContravariantK {
  def derived[Alg[_[_]]]: cats.tagless.ContravariantK[Alg] =
    macro internal.compiletime.ContravariantKMacros.deriveContravariantKImpl[Alg]
}

object KindlingsSemigroupalK {
  def derived[Alg[_[_]]]: cats.tagless.SemigroupalK[Alg] =
    macro internal.compiletime.SemigroupalKMacros.deriveSemigroupalKImpl[Alg]
}

object KindlingsApplyK {
  def derived[Alg[_[_]]]: cats.tagless.ApplyK[Alg] =
    macro internal.compiletime.ApplyKMacros.deriveApplyKImpl[Alg]
}

object KindlingsInstrument {
  def derived[Alg[_[_]]]: cats.tagless.aop.Instrument[Alg] =
    macro internal.compiletime.InstrumentMacros.deriveInstrumentImpl[Alg]
}
