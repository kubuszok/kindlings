package hearth.kindlings.catstaglessderivation

import cats.tagless.{ContravariantK, FunctorK, InvariantK, SemigroupalK}
import cats.tagless.aop.Instrument

object Scala2TraitExamples {

  trait SimpleService[F[_]] {
    def get(id: Int): F[String]
    def getAge(name: String): F[Int]
  }
  object SimpleService {
    implicit val functorK: FunctorK[SimpleService] = KindlingsFunctorK.derived
    implicit val invariantK: InvariantK[SimpleService] = KindlingsInvariantK.derived
    implicit val semigroupalK: SemigroupalK[SimpleService] = KindlingsSemigroupalK.derived
    implicit val instrument: Instrument[SimpleService] = KindlingsInstrument.derived
  }

  trait MixedService[F[_]] {
    def fetch(id: Int): F[String]
    def version: String
  }
  object MixedService {
    implicit val functorK: FunctorK[MixedService] = KindlingsFunctorK.derived
  }

  trait InvariantOnly[F[_]] {
    def name: String
  }
  object InvariantOnly {
    implicit val contravariantK: ContravariantK[InvariantOnly] = KindlingsContravariantK.derived
  }
}
