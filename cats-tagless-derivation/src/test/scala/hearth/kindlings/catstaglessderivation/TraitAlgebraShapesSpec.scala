package hearth.kindlings.catstaglessderivation

import cats.arrow.FunctionK
import cats.data.Tuple2K
import cats.tagless.{ApplyK, ContravariantK, FunctorK, InvariantK, SemigroupalK}
import hearth.MacroSuite

/** Trait algebras whose method shapes cats-tagless' `Derive.*` supports (checked against cats-tagless in
  * `upstream-oracle-tests`), on both Scala versions.
  */
object TraitAlgebraShapesSpec {

  trait ShapeRepo[F[_]] {
    def get(id: Int): F[String]
    def label(x: Int): String
    def size: Int
  }

  trait ShapeGeneric[F[_]] {
    def generic[A](a: A): F[List[A]]
  }

  trait ShapeConsumer[F[_]] {
    def consume(data: F[Int]): String
  }

  trait ShapeTransform[F[_]] {
    def transform(input: F[Int]): F[String]
  }

  type OptPair[A] = Tuple2K[Option, Option, A]

  /** A result type whose `Semigroup` is an `implicit def` counting how often it is created: the derived instances must
    * resolve it once per instance, not on every call of the method returning it.
    */
  final case class CountedLabel(value: String)
  object CountedLabel {
    @volatile var instancesCreated: Int = 0
    implicit def semigroupCountedLabel: cats.kernel.Semigroup[CountedLabel] = {
      instancesCreated += 1
      cats.kernel.Semigroup.instance((x, y) => CountedLabel(x.value + y.value))
    }
  }

  trait CountedRepo[F[_]] {
    def get(id: Int): F[String]
    def label(x: Int): CountedLabel
  }
}

final class TraitAlgebraShapesSpec extends MacroSuite {
  import TraitAlgebraShapesSpec.*

  private val fk = new FunctionK[Option, List] { def apply[A](fa: Option[A]): List[A] = fa.toList }
  private val gk = new FunctionK[List, Option] { def apply[A](fa: List[A]): Option[A] = fa.headOption }

  private def repo(prefix: String, n: Int): ShapeRepo[Option] = new ShapeRepo[Option] {
    def get(id: Int): Option[String] = Some(s"$prefix$id")
    def label(x: Int): String = s"$prefix$x"
    def size: Int = n
  }

  test("SemigroupalK/ApplyK combine members without F with their Semigroup, like cats-tagless") {
    val semigroupalK: SemigroupalK[ShapeRepo] = KindlingsSemigroupalK.derived[ShapeRepo]
    val product = semigroupalK.productK(repo("a", 1), repo("b", 2))
    product.get(1) ==> Tuple2K[Option, Option, String](Some("a1"), Some("b1"))
    product.label(7) ==> "a7b7"
    product.size ==> 3

    val applyK: ApplyK[ShapeRepo] = KindlingsApplyK.derived[ShapeRepo]
    val both = new FunctionK[OptPair, List] {
      def apply[A](fa: OptPair[A]): List[A] = fa.first.toList ++ fa.second.toList
    }
    val mapped = applyK.map2K(repo("a", 1), repo("b", 2))(both)
    mapped.get(1) ==> List("a1", "b1")
    mapped.label(7) ==> "a7b7"
  }

  test("SemigroupalK/ApplyK resolve the Semigroup of members without F once per instance, not per call") {
    def countedRepo(prefix: String): CountedRepo[Option] = new CountedRepo[Option] {
      def get(id: Int): Option[String] = Some(s"$prefix$id")
      def label(x: Int): CountedLabel = CountedLabel(s"$prefix$x")
    }
    // Calls `op` three times and returns how many Semigroups the 2nd and 3rd call created.
    def createdByRepeatedCalls(op: () => Any): Int = {
      op()
      val before = CountedLabel.instancesCreated
      op()
      op()
      CountedLabel.instancesCreated - before
    }

    val semigroupalK: SemigroupalK[CountedRepo] = KindlingsSemigroupalK.derived[CountedRepo]
    val product = semigroupalK.productK(countedRepo("a"), countedRepo("b"))
    product.label(7) ==> CountedLabel("a7b7")
    createdByRepeatedCalls(() => product.label(7)) ==> 0

    val applyK: ApplyK[CountedRepo] = KindlingsApplyK.derived[CountedRepo]
    val both = new FunctionK[OptPair, List] {
      def apply[A](fa: OptPair[A]): List[A] = fa.first.toList ++ fa.second.toList
    }
    val mapped = applyK.map2K(countedRepo("a"), countedRepo("b"))(both)
    mapped.label(7) ==> CountedLabel("a7b7")
    createdByRepeatedCalls(() => mapped.label(7)) ==> 0
  }

  test("FunctorK for a polymorphic method") {
    val functorK: FunctorK[ShapeGeneric] = KindlingsFunctorK.derived[ShapeGeneric]
    val mapped = functorK.mapK(new ShapeGeneric[Option] {
      def generic[A](a: A): Option[List[A]] = Some(List(a, a))
    })(fk)
    mapped.generic("x") ==> List(List("x", "x"))
    mapped.generic(1) ==> List(List(1, 1))
  }

  test("ContravariantK and InvariantK for F in parameter position") {
    val contravariantK: ContravariantK[ShapeConsumer] = KindlingsContravariantK.derived[ShapeConsumer]
    val consumer = contravariantK.contramapK(new ShapeConsumer[Option] {
      def consume(data: Option[Int]): String = s"got $data"
    })(gk)
    consumer.consume(List(5, 6)) ==> "got Some(5)"
    consumer.consume(Nil) ==> "got None"

    val invariantK: InvariantK[ShapeTransform] = KindlingsInvariantK.derived[ShapeTransform]
    val transform = invariantK.imapK(new ShapeTransform[Option] {
      def transform(input: Option[Int]): Option[String] = input.map(i => s"<$i>")
    })(fk)(gk)
    transform.transform(List(1, 2)) ==> List("<1>")
    transform.transform(Nil) ==> Nil
  }
}
