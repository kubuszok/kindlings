package hearth.kindlings.catstaglessderivation

import cats.arrow.FunctionK
import cats.tagless.{FunctorK, InvariantK}
import hearth.MacroSuite

import scala.util.Try

/** cats-tagless' `autoFunctorKTests` / `autoInvariantKTests` "Alg with extra type parameters": a type lambda over the
  * effect works on Scala 3 (on Scala 2 it is a Hearth gap, see `KnownGapsSpec`).
  */
final class ExtraTypeParamsSpec extends MacroSuite {
  import UpstreamAlgebras.*

  private val fk: FunctionK[Try, Option] = new FunctionK[Try, Option] {
    def apply[A](fa: Try[A]): Option[A] = fa.toOption
  }

  test("Alg with extra type parameters (after and before the effect type)") {
    def functorK[T]: FunctorK[[F[_]] =>> AlgWithExtraTP[F, T]] = KindlingsFunctorK.derived
    val tryAlg = new AlgWithExtraTP[Try, String] { def a(i: Int) = Try(i.toString) }
    functorK[String].mapK(tryAlg)(fk).a(5) ==> Some("5")

    val functorK2: FunctorK[[F[_]] =>> AlgWithExtraTP2[String, F]] = KindlingsFunctorK.derived
    functorK2.mapK(new AlgWithExtraTP2[String, Try] { def a(i: Int) = Try(i.toString) })(fk).a(5) ==> Some("5")

    val invariantK: InvariantK[[F[_]] =>> AlgWithExtraTP[F, String]] = KindlingsInvariantK.derived
    invariantK
      .imapK(tryAlg)(fk)(new FunctionK[Option, Try] { def apply[A](fa: Option[A]): Try[A] = Try(fa.get) })
      .a(6) ==> Some("6")
  }
}
