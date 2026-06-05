package hearth.kindlings.catstaglessderivation

import cats.tagless.{ApplyK, ContravariantK, FunctorK, InvariantK, SemigroupalK}
import cats.tagless.aop.Instrument

object traitExamples {

  trait ServiceAlg[F[_]] {
    def getUser(id: Int): F[String]
    def getAge(name: String): F[Int]
  }
  object ServiceAlg {
    implicit val functorK: FunctorK[ServiceAlg] = KindlingsFunctorK.derived
    implicit val invariantK: InvariantK[ServiceAlg] = KindlingsInvariantK.derived
    implicit val semigroupalK: SemigroupalK[ServiceAlg] = KindlingsSemigroupalK.derived
    implicit val applyK: ApplyK[ServiceAlg] = KindlingsApplyK.derived
    implicit val instrument: Instrument[ServiceAlg] = KindlingsInstrument.derived
  }

  trait MixedTraitAlg[F[_]] {
    def fetch(id: Int): F[String]
    def version: String
  }
  object MixedTraitAlg {
    implicit val functorK: FunctorK[MixedTraitAlg] = KindlingsFunctorK.derived
    implicit val invariantK: InvariantK[MixedTraitAlg] = KindlingsInvariantK.derived
    implicit val semigroupalK: SemigroupalK[MixedTraitAlg] = KindlingsSemigroupalK.derived
    implicit val applyK: ApplyK[MixedTraitAlg] = KindlingsApplyK.derived
    implicit val instrument: Instrument[MixedTraitAlg] = KindlingsInstrument.derived
  }

  // ContravariantK for a trait with no F[X] methods (all invariant)
  trait ConfigAlg[F[_]] {
    def name: String
    def version: Int
  }
  object ConfigAlg {
    implicit val contravariantK: ContravariantK[ConfigAlg] = KindlingsContravariantK.derived
  }

  // --- Variance-aware trait derivation examples ---

  // F[X] in both parameter and return position — InvariantK can handle this
  trait TransformAlg[F[_]] {
    def transform(input: F[Int]): F[String]
  }
  object TransformAlg {
    implicit val invariantK: InvariantK[TransformAlg] = KindlingsInvariantK.derived
  }

  // F[X] only in parameter position — ContravariantK can handle this
  trait ConsumerAlg[F[_]] {
    def consume(data: F[Boolean]): Unit
  }
  object ConsumerAlg {
    implicit val contravariantK: ContravariantK[ConsumerAlg] = KindlingsContravariantK.derived
    implicit val invariantK: InvariantK[ConsumerAlg] = KindlingsInvariantK.derived
  }
}
