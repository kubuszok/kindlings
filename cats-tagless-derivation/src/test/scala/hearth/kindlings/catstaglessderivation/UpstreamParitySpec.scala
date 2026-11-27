package hearth.kindlings.catstaglessderivation

import cats.arrow.FunctionK
import cats.data.{Cokleisli, EitherT, Kleisli, OptionT, Tuple2K}
import cats.tagless.{ApplyK, ContravariantK, FunctorK, InvariantK, SemigroupalK}
import cats.tagless.aop.Instrument
import hearth.MacroSuite

import scala.util.Try

/** The trait-algebra cases of cats-tagless' own test suites (v0.16.6, `tests/src/test/scala-{2,3}/cats/tagless/tests`:
  * `autoFunctorKTests`, `autoInvariantKTests`, `autoContravariantKTests`, `autoSemigroupalKTests`, `autoApplyKTests`,
  * `autoInstrumentTests`, `TestAlgebras`), derived with Kindlings on both Scala versions.
  */
object UpstreamParitySpec {
  type OptPair[A] = Tuple2K[Option, Option, A]
}

final class UpstreamParitySpec extends MacroSuite {
  import UpstreamAlgebras.*

  private val fk: FunctionK[Try, Option] = new FunctionK[Try, Option] { def apply[A](fa: Try[A]): Option[A] = fa.toOption }
  private val gk: FunctionK[Option, Try] = new FunctionK[Option, Try] {
    def apply[A](fa: Option[A]): Try[A] = Try(fa.get)
  }
  private val listToOption: FunctionK[List, Option] = new FunctionK[List, Option] {
    def apply[A](fa: List[A]): Option[A] = fa.headOption
  }

  group("FunctorK (autoFunctorKTests)") {

    test("Alg with non effect method") {
      val functorK: FunctorK[AlgWithNonEffectMethod] = KindlingsFunctorK.derived
      val tryAlg = new AlgWithNonEffectMethod[Try] {
        def a(i: Int): Try[Int] = Try(i)
        def b(i: Int): Int = i
      }
      functorK.mapK(tryAlg)(fk).a(3) ==> Some(3)
      functorK.mapK(tryAlg)(fk).b(2) ==> 2
    }

    test("Alg with non effect method with default impl: delegates to the source's implementation, like cats-tagless") {
      val functorK: FunctorK[AlgWithDefaultImpl] = KindlingsFunctorK.derived
      val tryAlg = new AlgWithDefaultImpl[Try] {
        def plusOne(i: Int): Try[Int] = Try(i + 1)
      }
      functorK.mapK(tryAlg)(fk).plusOne(3) ==> Some(4)
      functorK.mapK(tryAlg)(fk).minusOne(2) ==> 1
      // cats-tagless overrides every non-final member and forwards it to the source algebra
      val overriding = new AlgWithDefaultImpl[Try] {
        def plusOne(i: Int): Try[Int] = Try(i + 1)
        override def minusOne(i: Int): Int = i - 100
      }
      functorK.mapK(overriding)(fk).minusOne(2) ==> -98
    }

    test("defs with no params, method with type params, curried method with default, default parameter") {
      val fDef: FunctorK[AlgWithDef] = KindlingsFunctorK.derived
      fDef.mapK(new AlgWithDef[Try] { def a = Try(1) })(fk).a ==> Some(1)

      val fTParam: FunctorK[AlgWithTParamInMethod] = KindlingsFunctorK.derived
      fTParam.mapK(new AlgWithTParamInMethod[Try] { def a[T](t: T): Try[String] = Try(t.toString) })(fk).a(32) ==>
        Some("32")

      val fCurry: FunctorK[AlgWithCurryMethod] = KindlingsFunctorK.derived
      fCurry.mapK(new AlgWithCurryMethod[Try] { def a(t: Int)(b: String) = Try(s"$t$b") })(fk).a()("x") ==> Some("42x")

      val fDefault: FunctorK[AlgWithDefaultParameter] = KindlingsFunctorK.derived
      val greeter = fDefault.mapK(new AlgWithDefaultParameter[Try] { def greet(name: String) = Try(s"Hello $name") })(fk)
      greeter.greet() ==> Some("Hello World")
      greeter.greet("John Doe") ==> Some("Hello John Doe")
    }

    test("alg with final method, by-name parameter and context bounds") {
      val fFinal: FunctorK[AlgWithFinalMethod] = KindlingsFunctorK.derived
      val logger = fFinal.mapK(new AlgWithFinalMethod[Try] { def log(msg: String) = Try(msg) })(fk)
      logger.info("green") ==> Some("[info] green")
      logger.warn("yellow") ==> Some("[warn] yellow")

      val fByName: FunctorK[AlgWithByNameParameter] = KindlingsFunctorK.derived
      var evaluated = 0
      val byName = fByName.mapK(new AlgWithByNameParameter[Try] { def log(msg: => String) = Try(msg + msg) })(fk)
      byName.log { evaluated += 1; "level".reverse } ==> Some("levellevel")
      evaluated ==> 2

      val fBounds: FunctorK[AlgWithContextBounds] = KindlingsFunctorK.derived
      fBounds
        .mapK(new AlgWithContextBounds[Try] { def a[T: cats.Show](t: Int) = Try(t.toString) })(fk)
        .a[Int](3) ==> Some("3")
    }

    test("alg with varargs parameter") {
      val functorK: FunctorK[AlgWithVarArgsParameter] = KindlingsFunctorK.derived
      val mapped = functorK.mapK(new AlgWithVarArgsParameter[Try] {
        def sum(xs: Int*): Int = xs.sum
        def fSum(xs: Int*): Try[Int] = Try(xs.sum)
      })(fk)
      mapped.sum(1, 2, 3) ==> 6
      mapped.fSum(1, 2, 3) ==> Some(6)
    }

    test("alg with abstract val") {
      val functorK: FunctorK[AlgWithValues] = KindlingsFunctorK.derived
      val mapped = functorK.mapK(new AlgWithValues[Try] { val int: Try[Int] = Try(5) })(fk)
      mapped.int ==> Some(5)
    }

    test("builder-style algebra: a method returning the algebra is mapped recursively") {
      val functorK: FunctorK[BuilderAlgebra] = KindlingsFunctorK.derived
      val optionBuilder = functorK.mapK(BuilderAlgebra.Named("foo"): BuilderAlgebra[List])(listToOption)
      optionBuilder.withFoo("bar").unit ==> Some(())
      optionBuilder.withFoo("bar").withFoo("baz").unit ==> Some(())
    }

    test("F nested in results through EitherT / Kleisli / OptionT (their FunctorK)") {
      val functorK: FunctorK[AlgWithTransformers] = KindlingsFunctorK.derived
      val mapped = functorK.mapK(new AlgWithTransformers[Try] {
        def either(i: Int) = EitherT.rightT[Try, String](i)
        def kleisli(i: Int) = Kleisli((s: String) => Try(s.length + i))
        def option(i: Int) = OptionT.some[Try](i)
      })(fk)
      mapped.either(1).value ==> Some(Right(1))
      mapped.kleisli(1).run("abc") ==> Some(4)
      mapped.option(2).value ==> Some(Some(2))
    }

    test("F nested in a parameter through Cokleisli (its ContravariantK)") {
      val functorK: FunctorK[AlgWithContravariantK] = KindlingsFunctorK.derived
      val tryAlg = new AlgWithContravariantK[Try] {
        def app(f: Cokleisli[Try, String, Int])(x: String): Try[Int] = Try(f.run(Try(x)))
        def unsafe(f: Cokleisli[Try, Int, String])(x: Int): String = f.run(Try(x))
      }
      val mapped = functorK.mapK(tryAlg)(fk)
      mapped.app(Cokleisli((o: Option[String]) => o.fold(0)(_.length)))("abcd") ==> Some(4)
      mapped.unsafe(Cokleisli((o: Option[Int]) => o.fold("none")(_.toString)))(7) ==> "7"
    }

    test("F nested in a result through a type without FunctorK is a compile error, like cats-tagless") {
      compileErrors("KindlingsFunctorK.derived[UpstreamAlgebras.AlgWithListOfF]").check(
        "Cannot derive FunctorK for trait: method 'all' has F nested in its result type"
      )
    }
  }

  group("InvariantK (autoInvariantKTests, TestAlgebras)") {

    test("SafeInvAlg: F[A] parameters, Kleisli result, EitherT parameter") {
      val invariantK: InvariantK[SafeInvAlg] = KindlingsInvariantK.derived
      val tryAlg = new SafeInvAlg[Try] {
        def parseInt(fs: Try[String]): Try[Int] = fs.map(_.toInt)
        def doubleParser(precision: Int): Kleisli[Try, String, Double] =
          Kleisli(s => Try(BigDecimal(s).setScale(precision, BigDecimal.RoundingMode.DOWN).toDouble))
        def parseIntOrError(fs: EitherT[Try, String, String]): Try[Int] = fs.value.map(_.fold(_ => -1, _.toInt))
      }
      val optAlg = invariantK.imapK(tryAlg)(fk)(gk)
      optAlg.parseInt(Some("3")) ==> Some(3)
      optAlg.parseInt(None) ==> None
      optAlg.doubleParser(1).run("1.25") ==> Some(1.2)
      optAlg.parseIntOrError(EitherT.rightT[Option, String]("5")) ==> Some(5)
      optAlg.parseIntOrError(EitherT.leftT[Option, String]("boom")) ==> Some(-1)
    }

    test("varargs: contravariant and invariant varargs are mapped element by element") {
      val invariantK: InvariantK[InvAlgWithVarArgsParameter] = KindlingsInvariantK.derived
      val tryAlg = new InvAlgWithVarArgsParameter[Try] {
        def sum(xs: Int*): Int = xs.sum
        def covariantSum(xs: Int*): Try[Int] = Try(xs.sum)
        def contravariantSum(xs: Try[Int]*): Int = xs.map(_.getOrElse(100)).sum
        def invariantSum(xs: Try[Int]*): Try[Int] = Try(xs.map(_.get).sum)
      }
      val optAlg = invariantK.imapK(tryAlg)(fk)(gk)
      optAlg.sum(1, 2) ==> 3
      optAlg.covariantSum(1, 2) ==> Some(3)
      optAlg.contravariantSum(Some(1), None, Some(2)) ==> 103
      optAlg.invariantSum(Some(1), Some(2)) ==> Some(3)
      optAlg.invariantSum(Some(1), None) ==> None
    }

    test("by-name F parameter stays by-name") {
      val invariantK: InvariantK[InvAlgWithByNameParameter] = KindlingsInvariantK.derived
      val tryAlg = new InvAlgWithByNameParameter[Try] {
        def whenM(cond: Try[Boolean])(action: => Try[Unit]): Try[Unit] = cond.flatMap(c => if (c) action else Try(()))
      }
      val optAlg = invariantK.imapK(tryAlg)(fk)(gk)
      var ran = 0
      optAlg.whenM(Some(false)) { ran += 1; Some(()) } ==> Some(())
      ran ==> 0
      optAlg.whenM(Some(true)) { ran += 1; Some(()) } ==> Some(())
      ran ==> 1
    }

    test("default implementation with F parameter is overridden and forwarded") {
      val invariantK: InvariantK[InvAlgWithDefaultImpl] = KindlingsInvariantK.derived
      val optAlg = invariantK.imapK(new InvAlgWithDefaultImpl[Try] {})(fk)(gk)
      optAlg.const(Some(1)) ==> Some(1)
    }
  }

  group("ContravariantK (autoContravariantKTests)") {

    test("F[A] parameters, F[A] varargs and a Cokleisli result") {
      val contravariantK: ContravariantK[ContraTestAlgebra] = KindlingsContravariantK.derived
      val optAlg = new ContraTestAlgebra[Option] {
        def sum(xs: Option[Int]): Int = xs.getOrElse(0)
        def sumAll(xss: Option[Int]*): Int = xss.map(_.getOrElse(0)).sum
        def foldSpecialized(init: String)(f: (Int, String) => Int): Cokleisli[Option, String, Int] =
          Cokleisli(o => o.fold(init.length)(s => f(init.length, s)))
      }
      val listAlg = contravariantK.contramapK(optAlg)(listToOption)
      listAlg.sum(List(3, 4)) ==> 3
      listAlg.sumAll(List(1), Nil, List(5, 6)) ==> 6
      listAlg.foldSpecialized("ab")((i, s) => i + s.length).run(List("xyz")) ==> 5
      listAlg.foldSpecialized("ab")((i, s) => i + s.length).run(Nil) ==> 2
    }
  }

  group("SemigroupalK / ApplyK (autoSemigroupalKTests, autoApplyKTests)") {

    test("CalculatorAlg: F[A] parameters are projected from the Tuple2K") {
      val semigroupalK: SemigroupalK[CalculatorAlg] = KindlingsSemigroupalK.derived
      def alg(offset: Int): CalculatorAlg[Option] = new CalculatorAlg[Option] {
        def lit(i: Int) = Some(i + offset)
        def add(x: Option[Int], y: Option[Int]) = for { a <- x; b <- y } yield a + b + offset
        def show[A](expr: Option[A]) = s"<$expr>"
      }
      val p = semigroupalK.productK(alg(0), alg(10))
      p.lit(1) ==> Tuple2K[Option, Option, Int](Some(1), Some(11))
      p.add(Tuple2K(Some(1), Some(2)), Tuple2K(Some(3), None)) ==> Tuple2K[Option, Option, Int](Some(4), None)
      p.show(Tuple2K[Option, Option, Int](Some(1), None)) ==> "<Some(1)><None>" // Semigroup[String], like cats-tagless
    }

    test("AutoApplyKTestAlg: EitherT result through its ApplyK") {
      val applyK: ApplyK[AutoApplyKTestAlg] = KindlingsApplyK.derived
      def alg(ok: Boolean): AutoApplyKTestAlg[Option] = new AutoApplyKTestAlg[Option] {
        def parseInt(str: String) = if (ok) Some(str.toInt) else None
        def parseDouble(str: String) = EitherT.fromOption[Option](Option(str.toDouble).filter(_ => ok), "nope")
        def divide(dividend: Float, divisor: Float) = if (ok) Some(dividend / divisor) else None
      }
      val p = applyK.productK(alg(true), alg(false))
      p.parseInt("2") ==> Tuple2K[Option, Option, Int](Some(2), None)
      p.parseDouble("1.5").value ==> Tuple2K[Option, Option, Either[String, Double]](Some(Right(1.5)), Some(Left("nope")))
      applyK.mapK(alg(true))(new FunctionK[Option, List] { def apply[A](fa: Option[A]) = fa.toList })
        .parseDouble("2.5")
        .value ==> List(Right(2.5))
    }

    test("SafeInvAlg: Kleisli result and EitherT parameter through their FunctorK / SemigroupalK") {
      val semigroupalK: SemigroupalK[SafeInvAlg] = KindlingsSemigroupalK.derived
      def alg(tag: String): SafeInvAlg[Option] = new SafeInvAlg[Option] {
        def parseInt(fs: Option[String]) = fs.map(s => (s + tag).length)
        def doubleParser(precision: Int) = Kleisli((s: String) => Some(s.length.toDouble + precision))
        def parseIntOrError(fs: EitherT[Option, String, String]) = fs.value.map(_.fold(_.length, _ => 0))
      }
      val p = semigroupalK.productK(alg("a"), alg("bb"))
      p.parseInt(Tuple2K(Some("x"), None)) ==> Tuple2K[Option, Option, Int](Some(2), None)
      p.doubleParser(1).run("abc") ==> Tuple2K[Option, Option, Double](Some(4.0), Some(4.0))
      p.parseIntOrError(EitherT.leftT[UpstreamParitySpec.OptPair, String]("err")) ==>
        Tuple2K[Option, Option, Int](Some(3), Some(3))
    }
  }

  group("Instrument (autoInstrumentTests)") {

    test("method names with symbols") {
      val instrument: Instrument[Lookup] = KindlingsInstrument.derived
      val inst = instrument.instrument(new Lookup[Option] { def ->(id: String) = Some(Some(1L)) })
      val result = inst.->("x")
      (result.value, result.algebraName, result.methodName) ==> ((Some(Some(1L)), "Lookup", "->"))
    }
  }
}
