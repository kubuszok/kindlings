package hearth.kindlings.oracle.catstagless

import cats.arrow.FunctionK
import cats.data.{Cokleisli, EitherT, Kleisli, OptionT, Tuple2K}
import cats.tagless.{ApplyK, ContravariantK, FunctorK, InvariantK, SemigroupalK}
import hearth.MacroSuite
import hearth.kindlings.oracle.Oracle

import scala.collection.mutable.ListBuffer

/** What both sides provide for the shapes of cats-tagless' own test suites. */
trait TgShapeInstances {
  def functorKDefaultImpl: FunctorK[TgDefaultImpl]
  def functorKTransformers: FunctorK[TgTransformers]
  def functorKCokleisliParam: FunctorK[TgCokleisliParam]
  def functorKBuilder: FunctorK[TgBuilder]
  def invariantKSafeInv: InvariantK[TgSafeInv]
  def invariantKVarargs: InvariantK[TgInvVarargs]
  def contravariantKContra: ContravariantK[TgContra]
  def semigroupalKCalculator: SemigroupalK[TgCalculator]
  def semigroupalKSafeInv: SemigroupalK[TgSafeInv]
  def applyKAlg: ApplyK[TgApplyKAlg]
}

/** Kindlings' cats-tagless-derivation vs cats-tagless' `Derive.*` on algebras nesting `F` in other types (`EitherT`,
  * `Kleisli`, `OptionT`, `Cokleisli`, the algebra itself), with concrete members and `F` varargs: results of every
  * member, and when the natural transformations run.
  */
final class CatsTaglessShapesOracleSpec extends MacroSuite {
  import CatsTaglessShapesOracleSpec.*

  private val k: TgShapeInstances = TgShapesKindlingsSide
  private val u: TgShapeInstances = TgShapesCatsTaglessSide

  private def compare[R](label: String)(observe: TgShapeInstances => R): List[String] = {
    def run(i: TgShapeInstances): Any =
      try observe(i)
      catch { case e: Exception => s"threw ${e.getClass.getSimpleName}" }
    Oracle.diff(run(k), run(u)).map(d => s"$label ${d.stripPrefix("$")}".replace("upstream=", "cats-tagless="))
  }

  private def check(differences: List[String]): Unit =
    assertEquals(differences, Nil, s"Kindlings vs cats-tagless differences:\n${differences.mkString("\n")}")

  test("FunctorK: concrete members, F nested in results and parameters, builder-style algebras") {
    check(
      compare("mapK default impl") { i =>
        val log = ListBuffer.empty[String]
        val mapped = i.functorKDefaultImpl.mapK(defaultImpl)(optionToList(log))
        (mapped.plusOne(1), mapped.minusOne(5), log.toList)
      } ++
        compare("mapK transformers") { i =>
          val log = ListBuffer.empty[String]
          val mapped = i.functorKTransformers.mapK(transformers)(optionToList(log))
          (mapped.either(1).value, mapped.either(-1).value, mapped.kleisli(2).run("ab"), mapped.option(3).value, log.toList)
        } ++
        compare("mapK cokleisli param") { i =>
          val mapped = i.functorKCokleisliParam.mapK(cokleisliParam)(optionToList(ListBuffer.empty))
          (
            mapped.app(Cokleisli((l: List[String]) => l.map(_.length).sum))("abc"),
            mapped.unsafe(Cokleisli((l: List[Int]) => l.mkString(",")))(4)
          )
        } ++
        compare("mapK builder") { i =>
          val log = ListBuffer.empty[String]
          val mapped = i.functorKBuilder.mapK(builder("a"))(optionToList(log))
          (mapped.unit, mapped.withFoo("b").unit, mapped.withFoo("b").withFoo("").unit, log.toList)
        }
    )
  }

  test("InvariantK: F[A], EitherT parameters, Kleisli results, F varargs") {
    check(
      compare("imapK safe inv") { i =>
        val log = ListBuffer.empty[String]
        val mapped = i.invariantKSafeInv.imapK(safeInv("x"))(optionToList(log))(listToOption)
        (
          mapped.parseInt(List("12", "3")),
          mapped.parseInt(Nil),
          mapped.doubleParser(2).run("abc"),
          mapped.parseIntOrError(EitherT.rightT[List, String]("7")),
          mapped.parseIntOrError(EitherT.leftT[List, String]("err")),
          log.toList
        )
      } ++
        compare("imapK varargs") { i =>
          val mapped = i.invariantKVarargs.imapK(invVarargs)(optionToList(ListBuffer.empty))(listToOption)
          (
            mapped.sum(1, 2),
            mapped.covariantSum(1, 2),
            mapped.contravariantSum(List(1), Nil, List(5, 6)),
            mapped.invariantSum(List(1), List(2)),
            mapped.invariantSum(List(1), Nil)
          )
        }
    )
  }

  test("ContravariantK: F[A] parameters, F varargs, Cokleisli results") {
    check(compare("contramapK contra") { i =>
      val mapped = i.contravariantKContra.contramapK(contra)(listToOption)
      (
        mapped.sum(List(3, 4)),
        mapped.sumAll(List(1), Nil, List(5)),
        mapped.foldSpecialized("ab")((n, s) => n + s.length).run(List("xyz")),
        mapped.foldSpecialized("ab")((n, s) => n + s.length).run(Nil)
      )
    })
  }

  test("SemigroupalK / ApplyK: F[A] parameters projected, nested results producted, Semigroup for the rest") {
    def parts[A](t: Tuple2K[Option, Option, A]) = (t.first, t.second)
    check(
      compare("productK calculator") { i =>
        val p = i.semigroupalKCalculator.productK(calculator(0), calculator(10))
        (
          parts(p.lit(1)),
          parts(p.add(Tuple2K(Some(1), Some(2)), Tuple2K(Some(3), None))),
          p.show(Tuple2K[Option, Option, Int](Some(1), None))
        )
      } ++
        compare("productK safe inv") { i =>
          val p = i.semigroupalKSafeInv.productK(safeInvOpt("a"), safeInvOpt("bb"))
          (
            parts(p.parseInt(Tuple2K(Some("x"), None))),
            parts(p.doubleParser(1).run("abc")),
            parts(p.parseIntOrError(EitherT.leftT[OptPair, String]("err")))
          )
        } ++
        compare("productK / map2K applyK") { i =>
          val p = i.applyKAlg.productK(applyKAlg(true), applyKAlg(false))
          val m = i.applyKAlg.map2K(applyKAlg(true), applyKAlg(false))(bothToList)
          (parts(p.parseInt("2")), parts(p.parseDouble("1.5").value), m.divide(1f, 2f), m.parseDouble("2").value)
        }
    )
  }

  test("F nested in a type without a FunctorK is rejected by both") {
    assert(compileErrors("cats.tagless.Derive.functorK[TgListOfF]").nonEmpty)
    assert(
      compileErrors("hearth.kindlings.catstaglessderivation.KindlingsFunctorK.derived[TgListOfF]").contains(
        "has F nested in its result type"
      )
    )
  }
}
object CatsTaglessShapesOracleSpec {

  type OptPair[A] = Tuple2K[Option, Option, A]

  def optionToList(log: ListBuffer[String]): FunctionK[Option, List] = new FunctionK[Option, List] {
    def apply[A](fa: Option[A]): List[A] = { log += s"fk($fa)"; fa.toList }
  }
  val listToOption: FunctionK[List, Option] = new FunctionK[List, Option] {
    def apply[A](fa: List[A]): Option[A] = fa.headOption
  }
  val bothToList: FunctionK[OptPair, List] = new FunctionK[OptPair, List] {
    def apply[A](fa: OptPair[A]): List[A] = fa.first.toList ++ fa.second.toList
  }

  val defaultImpl: TgDefaultImpl[Option] = new TgDefaultImpl[Option] {
    def plusOne(i: Int): Option[Int] = Some(i + 1)
    override def minusOne(i: Int): Int = i - 100
  }
  val transformers: TgTransformers[Option] = new TgTransformers[Option] {
    def either(i: Int) = if (i > 0) EitherT.rightT[Option, String](i) else EitherT.leftT[Option, Int]("neg")
    def kleisli(i: Int) = Kleisli((s: String) => Option(s.length + i))
    def option(i: Int) = OptionT.some[Option](i)
  }
  val cokleisliParam: TgCokleisliParam[Option] = new TgCokleisliParam[Option] {
    def app(f: Cokleisli[Option, String, Int])(x: String): Option[Int] = Some(f.run(Some(x)))
    def unsafe(f: Cokleisli[Option, Int, String])(x: Int): String = f.run(Some(x))
  }
  def builder(name: String): TgBuilder[Option] = new TgBuilder[Option] {
    def unit: Option[Unit] = if (name.nonEmpty) Some(()) else None
    def withFoo(foo: String): TgBuilder[Option] = builder(foo)
  }
  def safeInv(tag: String): TgSafeInv[Option] = new TgSafeInv[Option] {
    def parseInt(fs: Option[String]) = fs.map(s => (s + tag).length)
    def doubleParser(precision: Int) = Kleisli((s: String) => Option(s.length.toDouble + precision))
    def parseIntOrError(fs: EitherT[Option, String, String]) = fs.value.map(_.fold(_.length, _.toInt))
  }
  def safeInvOpt(tag: String): TgSafeInv[Option] = safeInv(tag)
  val invVarargs: TgInvVarargs[Option] = new TgInvVarargs[Option] {
    def sum(xs: Int*): Int = xs.sum
    def covariantSum(xs: Int*): Option[Int] = Some(xs.sum)
    def contravariantSum(xs: Option[Int]*): Int = xs.map(_.getOrElse(100)).sum
    def invariantSum(xs: Option[Int]*): Option[Int] = xs.foldLeft(Option(0))((a, b) => for { x <- a; y <- b } yield x + y)
  }
  val contra: TgContra[Option] = new TgContra[Option] {
    def sum(xs: Option[Int]): Int = xs.getOrElse(0)
    def sumAll(xss: Option[Int]*): Int = xss.map(_.getOrElse(0)).sum
    def foldSpecialized(init: String)(f: (Int, String) => Int): Cokleisli[Option, String, Int] =
      Cokleisli(o => o.fold(init.length)(s => f(init.length, s)))
  }
  def calculator(offset: Int): TgCalculator[Option] = new TgCalculator[Option] {
    def lit(i: Int) = Some(i + offset)
    def add(x: Option[Int], y: Option[Int]) = for { a <- x; b <- y } yield a + b + offset
    def show[A](expr: Option[A]) = s"<$expr>"
  }
  def applyKAlg(ok: Boolean): TgApplyKAlg[Option] = new TgApplyKAlg[Option] {
    def parseInt(str: String) = if (ok) Some(str.toInt) else None
    def parseDouble(str: String) = EitherT.fromOption[Option](Option(str.toDouble).filter(_ => ok), "nope")
    def divide(dividend: Float, divisor: Float) = if (ok) Some(dividend / divisor) else None
  }
}

/** Kindlings: `KindlingsXxx.derived`. */
object TgShapesKindlingsSide extends TgShapeInstances {
  import hearth.kindlings.catstaglessderivation.*

  val functorKDefaultImpl: FunctorK[TgDefaultImpl] = KindlingsFunctorK.derived[TgDefaultImpl]
  val functorKTransformers: FunctorK[TgTransformers] = KindlingsFunctorK.derived[TgTransformers]
  val functorKCokleisliParam: FunctorK[TgCokleisliParam] = KindlingsFunctorK.derived[TgCokleisliParam]
  val functorKBuilder: FunctorK[TgBuilder] = KindlingsFunctorK.derived[TgBuilder]
  val invariantKSafeInv: InvariantK[TgSafeInv] = KindlingsInvariantK.derived[TgSafeInv]
  val invariantKVarargs: InvariantK[TgInvVarargs] = KindlingsInvariantK.derived[TgInvVarargs]
  val contravariantKContra: ContravariantK[TgContra] = KindlingsContravariantK.derived[TgContra]
  val semigroupalKCalculator: SemigroupalK[TgCalculator] = KindlingsSemigroupalK.derived[TgCalculator]
  val semigroupalKSafeInv: SemigroupalK[TgSafeInv] = KindlingsSemigroupalK.derived[TgSafeInv]
  val applyKAlg: ApplyK[TgApplyKAlg] = KindlingsApplyK.derived[TgApplyKAlg]
}

/** cats-tagless: `cats.tagless.Derive.*`; a builder-style algebra needs its own `FunctorK` in implicit scope. */
object TgShapesCatsTaglessSide extends TgShapeInstances {
  import cats.tagless.Derive

  val functorKDefaultImpl: FunctorK[TgDefaultImpl] = Derive.functorK[TgDefaultImpl]
  val functorKTransformers: FunctorK[TgTransformers] = Derive.functorK[TgTransformers]
  val functorKCokleisliParam: FunctorK[TgCokleisliParam] = Derive.functorK[TgCokleisliParam]
  implicit lazy val functorKBuilder: FunctorK[TgBuilder] = Derive.functorK[TgBuilder]
  val invariantKSafeInv: InvariantK[TgSafeInv] = Derive.invariantK[TgSafeInv]
  val invariantKVarargs: InvariantK[TgInvVarargs] = Derive.invariantK[TgInvVarargs]
  val contravariantKContra: ContravariantK[TgContra] = Derive.contravariantK[TgContra]
  val semigroupalKCalculator: SemigroupalK[TgCalculator] = Derive.semigroupalK[TgCalculator]
  val semigroupalKSafeInv: SemigroupalK[TgSafeInv] = Derive.semigroupalK[TgSafeInv]
  val applyKAlg: ApplyK[TgApplyKAlg] = Derive.applyK[TgApplyKAlg]
}
