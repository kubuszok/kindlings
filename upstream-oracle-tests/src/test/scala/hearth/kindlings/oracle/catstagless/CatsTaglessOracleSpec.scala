package hearth.kindlings.oracle.catstagless

import cats.arrow.FunctionK
import cats.data.Tuple2K
import cats.tagless.{ApplyK, ContravariantK, FunctorK, InvariantK, SemigroupalK}
import cats.tagless.aop.{Instrument, Instrumentation}
import hearth.MacroSuite
import hearth.kindlings.oracle.Oracle

import scala.collection.mutable.ListBuffer

/** What both sides of the oracle provide: the instances derived for the shared algebras. */
trait TgInstances {
  def functorKSimple: FunctorK[TgSimple]
  def functorKCurried: FunctorK[TgCurried]
  def functorKByName: FunctorK[TgByName]
  def functorKGeneric: FunctorK[TgGeneric]
  def functorKDefault: FunctorK[TgDefault]
  def functorKVarargs: FunctorK[TgVarargs]
  def invariantKSimple: InvariantK[TgSimple]
  def invariantKTransform: InvariantK[TgTransform]
  def contravariantKConsumer: ContravariantK[TgConsumer]
  def semigroupalKSimple: SemigroupalK[TgSimple]
  def applyKSimple: ApplyK[TgSimple]
  def instrumentSimple: Instrument[TgSimple]
  def instrumentCurried: Instrument[TgCurried]
}

/** Kindlings' cats-tagless-derivation (`KindlingsXxx.derived`) vs cats-tagless' `Derive.*`, on algebras with various
  * method shapes: the results of calling every method of the transformed algebra, when the natural transformations run,
  * how often by-name arguments are evaluated, and the names `Instrument` reports.
  */
final class CatsTaglessOracleSpec extends MacroSuite {
  import CatsTaglessOracleSpec.*

  private val k: TgInstances = TgKindlingsSide
  private val u: TgInstances = TgCatsTaglessSide

  private def compare[R](label: String)(observe: TgInstances => R): List[String] = {
    def run(i: TgInstances): Any =
      try observe(i)
      catch { case e: Exception => s"threw ${e.getClass.getSimpleName}" }
    val kr = run(k)
    val ur = run(u)
    Oracle.diff(kr, ur).map(d => s"$label ${d.stripPrefix("$")}".replace("upstream=", "cats-tagless="))
  }

  private def check(differences: List[String], expected: List[String] = Nil): Unit =
    assertEquals(differences, expected, s"Kindlings vs cats-tagless differences:\n${differences.mkString("\n")}")

  test("FunctorK.mapK: results and when the natural transformation runs") {
    check(
      compare("mapK simple") { i =>
        val log = ListBuffer.empty[String]
        val mapped = i.functorKSimple.mapK(simple)(optionToList(log))
        val afterMapK = log.toList
        (afterMapK, mapped.get(1), mapped.get(0), mapped.count, mapped.put(1, "a"), mapped.label(3), log.toList)
      } ++
        compare("mapK curried") { i =>
          val mapped = i.functorKCurried.mapK(curried)(optionToList(ListBuffer.empty))
          (mapped.curried(1)("a"), mapped.curried(0)("b"))
        } ++
        compare("mapK by-name") { i =>
          val mapped = i.functorKByName.mapK(byName)(optionToList(ListBuffer.empty))
          var evaluations = 0
          val result = mapped.byName { evaluations += 1; 5 }
          (result, evaluations)
        } ++
        compare("mapK generic") { i =>
          val mapped = i.functorKGeneric.mapK(generic)(optionToList(ListBuffer.empty))
          (mapped.generic("a"), mapped.generic(1))
        } ++
        compare("mapK default") { i =>
          val mapped = i.functorKDefault.mapK(withDefault)(optionToList(ListBuffer.empty))
          (mapped.withDefault(1), mapped.withDefault(1, 10))
        } ++
        compare("mapK varargs") { i =>
          val mapped = i.functorKVarargs.mapK(varargs)(optionToList(ListBuffer.empty))
          (mapped.sum(), mapped.sum(1, 2, 3))
        }
    )
  }

  test("InvariantK.imapK and ContravariantK.contramapK") {
    check(
      compare("imapK simple") { i =>
        val mapped = i.invariantKSimple.imapK(simple)(optionToList(ListBuffer.empty))(listToOption)
        (mapped.get(1), mapped.count, mapped.label(2))
      } ++
        compare("imapK transform") { i =>
          val log = ListBuffer.empty[String]
          val mapped = i.invariantKTransform.imapK(transform)(optionToList(log))(listToOption)
          (mapped.transform(List(1, 2)), mapped.transform(Nil), log.toList)
        } ++
        compare("contramapK consumer") { i =>
          val mapped = i.contravariantKConsumer.contramapK(consumer)(listToOption)
          (mapped.consume(List(5, 6)), mapped.consume(Nil))
        }
    )
  }

  test("SemigroupalK.productK and ApplyK.map2K") {
    check(
      compare("productK simple") { i =>
        val p = i.semigroupalKSimple.productK(simple, simpleAlt)
        def parts[A](t: Tuple2K[Option, Option, A]) = (t.first, t.second)
        (parts(p.get(1)), parts(p.get(-1)), parts(p.count), parts(p.put(1, "")), p.label(7))
      } ++
        compare("map2K simple") { i =>
          val m = i.applyKSimple.map2K(simple, simpleAlt)(bothToList)
          (m.get(1), m.get(-1), m.count, m.put(1, ""), m.label(7))
        }
    )
  }

  test("Instrument: values, algebra and method names") {
    def parts[F[_], A](i: Instrumentation[F, A]) = (i.value, i.algebraName, i.methodName)
    check(
      compare("instrument simple") { i =>
        val inst = i.instrumentSimple.instrument(simple)
        (parts(inst.get(1)), parts(inst.count), parts(inst.put(1, "x")), inst.label(2))
      } ++
        compare("instrument curried") { i =>
          parts(i.instrumentCurried.instrument(curried).curried(2)("z"))
        } ++
        compare("instrument mapK") { i =>
          // `Instrument` is a `FunctorK` too.
          i.instrumentSimple.mapK(simple)(optionToList(ListBuffer.empty)).get(3)
        }
    )
  }
}
object CatsTaglessOracleSpec {

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

  val simple: TgSimple[Option] = new TgSimple[Option] {
    def get(id: Int): Option[String] = if (id > 0) Some(s"user$id") else None
    def count: Option[Int] = Some(42)
    def put(id: Int, name: String): Option[Unit] = if (name.nonEmpty) Some(()) else None
    def label(x: Int): String = s"label$x"
  }
  val simpleAlt: TgSimple[Option] = new TgSimple[Option] {
    def get(id: Int): Option[String] = Some(s"alt$id")
    def count: Option[Int] = None
    def put(id: Int, name: String): Option[Unit] = Some(())
    def label(x: Int): String = s"alt$x"
  }
  val curried: TgCurried[Option] = new TgCurried[Option] {
    def curried(a: Int)(b: String): Option[String] = if (a > 0) Some(b * a) else None
  }
  val byName: TgByName[Option] = new TgByName[Option] {
    def byName(x: => Int): Option[Int] = Some(x + x)
  }
  val generic: TgGeneric[Option] = new TgGeneric[Option] {
    def generic[A](a: A): Option[List[A]] = Some(List(a, a))
  }
  val withDefault: TgDefault[Option] = new TgDefault[Option] {
    def withDefault(a: Int, b: Int): Option[Int] = Some(a * 100 + b)
  }
  val varargs: TgVarargs[Option] = new TgVarargs[Option] {
    def sum(xs: Int*): Option[Int] = if (xs.isEmpty) None else Some(xs.sum)
  }
  val consumer: TgConsumer[Option] = new TgConsumer[Option] {
    def consume(data: Option[Int]): String = s"consumed $data"
  }
  val transform: TgTransform[Option] = new TgTransform[Option] {
    def transform(input: Option[Int]): Option[String] = input.map(i => s"<$i>")
  }
}

/** Kindlings: `KindlingsXxx.derived`. */
object TgKindlingsSide extends TgInstances {
  import hearth.kindlings.catstaglessderivation.*

  val functorKSimple: FunctorK[TgSimple] = KindlingsFunctorK.derived[TgSimple]
  val functorKCurried: FunctorK[TgCurried] = KindlingsFunctorK.derived[TgCurried]
  val functorKByName: FunctorK[TgByName] = KindlingsFunctorK.derived[TgByName]
  val functorKGeneric: FunctorK[TgGeneric] = KindlingsFunctorK.derived[TgGeneric]
  val functorKDefault: FunctorK[TgDefault] = KindlingsFunctorK.derived[TgDefault]
  val functorKVarargs: FunctorK[TgVarargs] = KindlingsFunctorK.derived[TgVarargs]
  val invariantKSimple: InvariantK[TgSimple] = KindlingsInvariantK.derived[TgSimple]
  val invariantKTransform: InvariantK[TgTransform] = KindlingsInvariantK.derived[TgTransform]
  val contravariantKConsumer: ContravariantK[TgConsumer] = KindlingsContravariantK.derived[TgConsumer]
  val semigroupalKSimple: SemigroupalK[TgSimple] = KindlingsSemigroupalK.derived[TgSimple]
  val applyKSimple: ApplyK[TgSimple] = KindlingsApplyK.derived[TgSimple]
  val instrumentSimple: Instrument[TgSimple] = KindlingsInstrument.derived[TgSimple]
  val instrumentCurried: Instrument[TgCurried] = KindlingsInstrument.derived[TgCurried]
}

/** cats-tagless: `cats.tagless.Derive.*` (on Scala 3 it is `@experimental`; this module compiles with `-experimental`).
  */
object TgCatsTaglessSide extends TgInstances {
  import cats.tagless.Derive

  val functorKSimple: FunctorK[TgSimple] = Derive.functorK[TgSimple]
  val functorKCurried: FunctorK[TgCurried] = Derive.functorK[TgCurried]
  val functorKByName: FunctorK[TgByName] = Derive.functorK[TgByName]
  val functorKGeneric: FunctorK[TgGeneric] = Derive.functorK[TgGeneric]
  val functorKDefault: FunctorK[TgDefault] = Derive.functorK[TgDefault]
  val functorKVarargs: FunctorK[TgVarargs] = Derive.functorK[TgVarargs]
  val invariantKSimple: InvariantK[TgSimple] = Derive.invariantK[TgSimple]
  val invariantKTransform: InvariantK[TgTransform] = Derive.invariantK[TgTransform]
  val contravariantKConsumer: ContravariantK[TgConsumer] = Derive.contravariantK[TgConsumer]
  val semigroupalKSimple: SemigroupalK[TgSimple] = Derive.semigroupalK[TgSimple]
  val applyKSimple: ApplyK[TgSimple] = Derive.applyK[TgSimple]
  val instrumentSimple: Instrument[TgSimple] = Derive.instrument[TgSimple]
  val instrumentCurried: Instrument[TgCurried] = Derive.instrument[TgCurried]
}
