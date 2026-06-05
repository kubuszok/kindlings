package hearth.kindlings.catstaglessderivation

object KindlingsInvariantK {
  inline def derived[Alg[_[_]]]: cats.tagless.InvariantK[Alg] = ${
    internal.compiletime.InvariantKMacros.deriveInvariantKImpl[Alg]
  }
}

object KindlingsFunctorK {
  inline def derived[Alg[_[_]]]: cats.tagless.FunctorK[Alg] = ${
    internal.compiletime.FunctorKMacros.deriveFunctorKImpl[Alg]
  }
}

object KindlingsContravariantK {
  inline def derived[Alg[_[_]]]: cats.tagless.ContravariantK[Alg] = ${
    internal.compiletime.ContravariantKMacros.deriveContravariantKImpl[Alg]
  }
}

object KindlingsSemigroupalK {
  inline def derived[Alg[_[_]]]: cats.tagless.SemigroupalK[Alg] = ${
    internal.compiletime.SemigroupalKMacros.deriveSemigroupalKImpl[Alg]
  }
}

object KindlingsApplyK {
  inline def derived[Alg[_[_]]]: cats.tagless.ApplyK[Alg] = ${
    internal.compiletime.ApplyKMacros.deriveApplyKImpl[Alg]
  }
}

object KindlingsInstrument {
  inline def derived[Alg[_[_]]]: cats.tagless.aop.Instrument[Alg] = ${
    internal.compiletime.InstrumentMacros.deriveInstrumentImpl[Alg]
  }
}
