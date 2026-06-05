package hearth.kindlings.catstaglessderivation.internal.runtime

import cats.arrow.FunctionK
import cats.tagless.{ApplyK, ContravariantK, FunctorK, InvariantK, SemigroupalK}
import cats.tagless.aop.{Instrument, Instrumentation}

object CatsTaglessFactories {

  type WCtor1[A] = Any
  type WCtor2[A] = Any
  type WCtor3[A] = Any

  type DummyHKT[F[_]] = Any

  // --- Kind (* -> *) -> *, erasure-based ---
  //
  // WCtor1/WCtor2 are type constructors (kind * -> *) that only appear as type ARGUMENTS
  // (e.g. Alg[WCtor1], FunctionK[WCtor1, WCtor2]), never as direct parameter types.
  // This keeps Scala 3 lambda bridge checkcast instructions safe.

  def invariantKInstance[Alg[_[_]]](
      imapKFn: (Alg[WCtor1], FunctionK[WCtor1, WCtor2], FunctionK[WCtor2, WCtor1]) => Alg[WCtor2]
  ): InvariantK[Alg] = new InvariantK[Alg] {
    def imapK[F[_], G[_]](af: Alg[F])(fk: FunctionK[F, G])(gk: FunctionK[G, F]): Alg[G] =
      imapKFn
        .asInstanceOf[(Alg[F], FunctionK[F, G], FunctionK[G, F]) => Alg[G]]
        .apply(af, fk, gk)
  }

  def functorKInstance[Alg[_[_]]](
      mapKFn: (Alg[WCtor1], FunctionK[WCtor1, WCtor2]) => Alg[WCtor2]
  ): FunctorK[Alg] = new FunctorK[Alg] {
    def mapK[F[_], G[_]](af: Alg[F])(fk: FunctionK[F, G]): Alg[G] =
      mapKFn
        .asInstanceOf[(Alg[F], FunctionK[F, G]) => Alg[G]]
        .apply(af, fk)
  }

  def contravariantKInstance[Alg[_[_]]](
      contramapKFn: (Alg[WCtor1], FunctionK[WCtor2, WCtor1]) => Alg[WCtor2]
  ): ContravariantK[Alg] = new ContravariantK[Alg] {
    def contramapK[F[_], G[_]](af: Alg[F])(fk: FunctionK[G, F]): Alg[G] =
      contramapKFn
        .asInstanceOf[(Alg[F], FunctionK[G, F]) => Alg[G]]
        .apply(af, fk)
  }

  def semigroupalKInstance[Alg[_[_]]](
      productKFn: (Alg[WCtor1], Alg[WCtor2]) => Alg[WCtor3]
  ): SemigroupalK[Alg] = new SemigroupalK[Alg] {
    def productK[F[_], G[_]](af: Alg[F], ag: Alg[G]): Alg[cats.data.Tuple2K[F, G, *]] =
      productKFn
        .asInstanceOf[(Alg[F], Alg[G]) => Alg[cats.data.Tuple2K[F, G, *]]]
        .apply(af, ag)
  }

  def applyKInstance[Alg[_[_]]](
      mapKFn: (Alg[WCtor1], FunctionK[WCtor1, WCtor2]) => Alg[WCtor2],
      productKFn: (Alg[WCtor1], Alg[WCtor2]) => Alg[WCtor3]
  ): ApplyK[Alg] = new ApplyK[Alg] {
    def mapK[F[_], G[_]](af: Alg[F])(fk: FunctionK[F, G]): Alg[G] =
      mapKFn
        .asInstanceOf[(Alg[F], FunctionK[F, G]) => Alg[G]]
        .apply(af, fk)
    def productK[F[_], G[_]](af: Alg[F], ag: Alg[G]): Alg[cats.data.Tuple2K[F, G, *]] =
      productKFn
        .asInstanceOf[(Alg[F], Alg[G]) => Alg[cats.data.Tuple2K[F, G, *]]]
        .apply(af, ag)
  }

  def instrumentInstance[Alg[_[_]]](
      mapKFn: (Alg[WCtor1], FunctionK[WCtor1, WCtor2]) => Alg[WCtor2],
      instrumentFn: Alg[WCtor1] => Alg[WCtor2]
  ): Instrument[Alg] = new Instrument[Alg] {
    def mapK[F[_], G[_]](af: Alg[F])(fk: FunctionK[F, G]): Alg[G] =
      mapKFn
        .asInstanceOf[(Alg[F], FunctionK[F, G]) => Alg[G]]
        .apply(af, fk)
    def instrument[F[_]](af: Alg[F]): Alg[Instrumentation[F, *]] =
      instrumentFn
        .asInstanceOf[Alg[F] => Alg[Instrumentation[F, *]]]
        .apply(af)
  }
}
