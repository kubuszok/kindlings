package hearth.kindlings.catstaglessderivation

import cats.tagless.{ApplyK, ContravariantK, FunctorK, InvariantK, SemigroupalK}
import cats.tagless.aop.Instrument

object examples {

  // --- InvariantK ---

  case class DirectAlg[F[_]](a: F[Int], b: F[String])
  object DirectAlg {
    implicit val invariantK: InvariantK[DirectAlg] = KindlingsInvariantK.derived
    implicit val functorK: FunctorK[DirectAlg] = KindlingsFunctorK.derived
    implicit val semigroupalK: SemigroupalK[DirectAlg] = KindlingsSemigroupalK.derived
    implicit val applyK: ApplyK[DirectAlg] = KindlingsApplyK.derived
    implicit val instrument: Instrument[DirectAlg] = KindlingsInstrument.derived
  }

  case class MixedAlg[F[_]](a: F[Int], name: String, b: F[Boolean])
  object MixedAlg {
    implicit val invariantK: InvariantK[MixedAlg] = KindlingsInvariantK.derived
    implicit val functorK: FunctorK[MixedAlg] = KindlingsFunctorK.derived
    implicit val semigroupalK: SemigroupalK[MixedAlg] = KindlingsSemigroupalK.derived
    implicit val applyK: ApplyK[MixedAlg] = KindlingsApplyK.derived
    implicit val instrument: Instrument[MixedAlg] = KindlingsInstrument.derived
  }

  case class InvariantOnlyAlg[F[_]](name: String, count: Int)
  object InvariantOnlyAlg {
    implicit val invariantK: InvariantK[InvariantOnlyAlg] = KindlingsInvariantK.derived
    implicit val functorK: FunctorK[InvariantOnlyAlg] = KindlingsFunctorK.derived
    implicit val contravariantK: ContravariantK[InvariantOnlyAlg] = KindlingsContravariantK.derived
    implicit val semigroupalK: SemigroupalK[InvariantOnlyAlg] = KindlingsSemigroupalK.derived
    implicit val applyK: ApplyK[InvariantOnlyAlg] = KindlingsApplyK.derived
    implicit val instrument: Instrument[InvariantOnlyAlg] = KindlingsInstrument.derived
  }

  case class InnerAlg[F[_]](value: F[Int])
  object InnerAlg {
    implicit val invariantK: InvariantK[InnerAlg] = KindlingsInvariantK.derived
    implicit val functorK: FunctorK[InnerAlg] = KindlingsFunctorK.derived
  }

  case class OuterAlg[F[_]](inner: InnerAlg[F], x: F[String])
  object OuterAlg {
    implicit val invariantK: InvariantK[OuterAlg] = KindlingsInvariantK.derived
    implicit val functorK: FunctorK[OuterAlg] = KindlingsFunctorK.derived
  }

  // Nested algebra where InnerAlg2 has NO hand-written instance —
  // tests recursive derivation.
  case class InnerAlg2[F[_]](value: F[Boolean])
  case class OuterAlg2[F[_]](inner: InnerAlg2[F], x: F[Int])
  object OuterAlg2 {
    implicit val invariantK: InvariantK[OuterAlg2] = KindlingsInvariantK.derived
    implicit val functorK: FunctorK[OuterAlg2] = KindlingsFunctorK.derived
  }

  // --- ContravariantK: nested-only algebra (no direct F[X] fields) ---

  case class InnerContra[F[_]](name: String)
  object InnerContra {
    implicit val contravariantK: ContravariantK[InnerContra] = KindlingsContravariantK.derived
  }

  case class OuterContra[F[_]](inner: InnerContra[F], label: String)
  object OuterContra {
    implicit val contravariantK: ContravariantK[OuterContra] = KindlingsContravariantK.derived
  }
}
