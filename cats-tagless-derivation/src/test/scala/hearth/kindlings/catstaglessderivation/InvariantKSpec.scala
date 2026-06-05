package hearth.kindlings.catstaglessderivation

import hearth.MacroSuite
import cats.arrow.FunctionK
import cats.data.Tuple2K

final class InvariantKSpec extends MacroSuite {

  private val fk = new FunctionK[Option, List] { def apply[A](fa: Option[A]): List[A] = fa.toList }
  private val gk = new FunctionK[List, Option] { def apply[A](fa: List[A]): Option[A] = fa.headOption }

  group("InvariantK") {

    test("direct fields only") {
      val result = examples.DirectAlg.invariantK.imapK(examples.DirectAlg[Option](Some(42), Some("hello")))(fk)(gk)
      result.a ==> List(42)
      result.b ==> List("hello")
    }

    test("invariant fields preserved") {
      val result = examples.MixedAlg.invariantK.imapK(examples.MixedAlg[Option](Some(42), "test", Some(true)))(fk)(gk)
      result.a ==> List(42)
      result.name ==> "test"
      result.b ==> List(true)
    }

    test("all invariant fields") {
      val result = examples.InvariantOnlyAlg.invariantK.imapK(examples.InvariantOnlyAlg[Option]("test", 5))(fk)(gk)
      result.name ==> "test"
      result.count ==> 5
    }

    test("nested algebra") {
      val alg = examples.OuterAlg[Option](examples.InnerAlg[Option](Some(99)), Some("outer"))
      val result = examples.OuterAlg.invariantK.imapK(alg)(fk)(gk)
      result.inner.value ==> List(99)
      result.x ==> List("outer")
    }

    test("nested algebra without explicit instance (recursive derivation)") {
      val alg = examples.OuterAlg2[Option](examples.InnerAlg2[Option](Some(true)), Some(7))
      val result = examples.OuterAlg2.invariantK.imapK(alg)(fk)(gk)
      result.inner.value ==> List(true)
      result.x ==> List(7)
    }

    test("empty to non-empty via fk") {
      val result = examples.DirectAlg.invariantK.imapK(examples.DirectAlg[Option](None, None))(fk)(gk)
      result.a ==> List.empty[Int]
      result.b ==> List.empty[String]
    }
  }

  group("FunctorK") {

    test("direct fields only") {
      val result = examples.DirectAlg.functorK.mapK(examples.DirectAlg[Option](Some(42), Some("hello")))(fk)
      result.a ==> List(42)
      result.b ==> List("hello")
    }

    test("mixed direct and invariant") {
      val result = examples.MixedAlg.functorK.mapK(examples.MixedAlg[Option](Some(42), "test", Some(true)))(fk)
      result.a ==> List(42)
      result.name ==> "test"
      result.b ==> List(true)
    }

    test("all invariant fields") {
      val result = examples.InvariantOnlyAlg.functorK.mapK(examples.InvariantOnlyAlg[Option]("test", 5))(fk)
      result.name ==> "test"
      result.count ==> 5
    }

    test("nested algebra") {
      val alg = examples.OuterAlg[Option](examples.InnerAlg[Option](Some(99)), Some("outer"))
      val result = examples.OuterAlg.functorK.mapK(alg)(fk)
      result.inner.value ==> List(99)
      result.x ==> List("outer")
    }

    test("nested algebra without explicit instance (recursive derivation)") {
      val alg = examples.OuterAlg2[Option](examples.InnerAlg2[Option](Some(true)), Some(7))
      val result = examples.OuterAlg2.functorK.mapK(alg)(fk)
      result.inner.value ==> List(true)
      result.x ==> List(7)
    }
  }

  group("ContravariantK") {

    test("all invariant fields") {
      val result = examples.InvariantOnlyAlg.contravariantK.contramapK(
        examples.InvariantOnlyAlg[Option]("test", 5)
      )(gk)
      result.name ==> "test"
      result.count ==> 5
    }

    test("nested algebra with invariant-only fields") {
      val alg = examples.OuterContra[Option](examples.InnerContra[Option]("inner"), "outer")
      val result = examples.OuterContra.contravariantK.contramapK(alg)(gk)
      result.inner.name ==> "inner"
      result.label ==> "outer"
    }
  }

  group("SemigroupalK") {

    test("direct fields only") {
      val algOpt = examples.DirectAlg[Option](Some(42), Some("hello"))
      val algList = examples.DirectAlg[List](List(1, 2), List("a", "b"))
      val result = examples.DirectAlg.semigroupalK.productK(algOpt, algList)
      result.a.first ==> Some(42)
      result.a.second ==> List(1, 2)
      result.b.first ==> Some("hello")
      result.b.second ==> List("a", "b")
    }

    test("mixed direct and invariant") {
      val algOpt = examples.MixedAlg[Option](Some(42), "test", Some(true))
      val algList = examples.MixedAlg[List](List(1), "test", List(false))
      val result = examples.MixedAlg.semigroupalK.productK(algOpt, algList)
      result.a.first ==> Some(42)
      result.a.second ==> List(1)
      result.name ==> "test"
      result.b.first ==> Some(true)
      result.b.second ==> List(false)
    }

    test("all invariant fields") {
      val algOpt = examples.InvariantOnlyAlg[Option]("test", 5)
      val algList = examples.InvariantOnlyAlg[List]("test", 5)
      val result = examples.InvariantOnlyAlg.semigroupalK.productK(algOpt, algList)
      result.name ==> "test"
      result.count ==> 5
    }
  }

  group("ApplyK") {

    test("mapK from ApplyK") {
      val result = examples.DirectAlg.applyK.mapK(examples.DirectAlg[Option](Some(42), Some("hello")))(fk)
      result.a ==> List(42)
      result.b ==> List("hello")
    }

    test("productK from ApplyK") {
      val algOpt = examples.DirectAlg[Option](Some(42), Some("hello"))
      val algList = examples.DirectAlg[List](List(1, 2), List("a", "b"))
      val result = examples.DirectAlg.applyK.productK(algOpt, algList)
      result.a.first ==> Some(42)
      result.a.second ==> List(1, 2)
      result.b.first ==> Some("hello")
      result.b.second ==> List("a", "b")
    }

    test("map2K from ApplyK") {
      val algOpt = examples.DirectAlg[Option](Some(42), Some("hello"))
      val algList = examples.DirectAlg[List](List(1, 2), List("a", "b"))
      val result = examples.DirectAlg.applyK.map2K(algOpt, algList)(
        new FunctionK[Tuple2K[Option, List, *], Option] {
          def apply[A](fa: Tuple2K[Option, List, A]): Option[A] = fa.first
        }
      )
      result.a ==> Some(42)
      result.b ==> Some("hello")
    }

    test("mixed direct and invariant") {
      val result = examples.MixedAlg.applyK.mapK(examples.MixedAlg[Option](Some(42), "test", Some(true)))(fk)
      result.a ==> List(42)
      result.name ==> "test"
      result.b ==> List(true)
    }

    test("all invariant fields") {
      val result = examples.InvariantOnlyAlg.applyK.mapK(examples.InvariantOnlyAlg[Option]("test", 5))(fk)
      result.name ==> "test"
      result.count ==> 5
    }
  }

  group("Instrument") {

    test("case class direct fields wrapped in Instrumentation") {
      val alg = examples.DirectAlg[Option](Some(42), Some("hello"))
      val instrumented = examples.DirectAlg.instrument.instrument(alg)
      instrumented.a.value ==> Some(42)
      instrumented.a.algebraName ==> "DirectAlg"
      instrumented.a.methodName ==> "a"
      instrumented.b.value ==> Some("hello")
      instrumented.b.algebraName ==> "DirectAlg"
      instrumented.b.methodName ==> "b"
    }

    test("mixed direct and invariant fields") {
      val alg = examples.MixedAlg[Option](Some(42), "test", Some(true))
      val instrumented = examples.MixedAlg.instrument.instrument(alg)
      instrumented.a.value ==> Some(42)
      instrumented.a.methodName ==> "a"
      instrumented.name ==> "test"
      instrumented.b.value ==> Some(true)
      instrumented.b.methodName ==> "b"
    }

    test("all invariant fields preserved") {
      val alg = examples.InvariantOnlyAlg[Option]("test", 5)
      val instrumented = examples.InvariantOnlyAlg.instrument.instrument(alg)
      instrumented.name ==> "test"
      instrumented.count ==> 5
    }

    test("mapK still works on Instrument") {
      val result = examples.DirectAlg.instrument.mapK(examples.DirectAlg[Option](Some(42), Some("hello")))(fk)
      result.a ==> List(42)
      result.b ==> List("hello")
    }
  }
}
