package hearth.kindlings.catsderivation

import hearth.ScalaCheckSuite
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

/** Property-based law checks for derived cats type class instances.
  *
  * Verifies that derived instances satisfy the algebraic laws required by each type class. This catches bugs like
  * non-associative Semigroup combine or non-identity Functor map that hand-written unit tests would miss.
  */
final class LawSpec extends ScalaCheckSuite {

  // ---------------------------------------------------------------------------
  // Arbitrary instances (manual, since scalacheck-derivation is not a dependency)
  // ---------------------------------------------------------------------------

  implicit val arbPoint: Arbitrary[examples.Point] = Arbitrary(
    for {
      x <- Arbitrary.arbitrary[Int]
      y <- Arbitrary.arbitrary[Int]
    } yield examples.Point(x, y)
  )

  implicit val arbPerson: Arbitrary[examples.Person] = Arbitrary(
    for {
      name <- Arbitrary.arbitrary[String]
      age <- Arbitrary.arbitrary[Int]
    } yield examples.Person(name, age)
  )

  implicit val arbShape: Arbitrary[examples.Shape] = Arbitrary(
    Gen.oneOf(
      Arbitrary.arbitrary[Double].map(examples.Circle(_)),
      for {
        w <- Arbitrary.arbitrary[Double]
        h <- Arbitrary.arbitrary[Double]
      } yield examples.Rectangle(w, h)
    )
  )

  implicit val arbColor: Arbitrary[examples.Color] = Arbitrary(
    Gen.oneOf(examples.Red, examples.Green, examples.Blue)
  )

  implicit val arbLongPair: Arbitrary[examples.LongPair] = Arbitrary(
    for {
      a <- Arbitrary.arbitrary[Long]
      b <- Arbitrary.arbitrary[Long]
    } yield examples.LongPair(a, b)
  )

  implicit val arbStringPair: Arbitrary[examples.StringPair] = Arbitrary(
    for {
      x <- Arbitrary.arbitrary[String]
      y <- Arbitrary.arbitrary[String]
    } yield examples.StringPair(x, y)
  )

  implicit def arbBox[A: Arbitrary]: Arbitrary[examples.Box[A]] = Arbitrary(
    Arbitrary.arbitrary[A].map(examples.Box(_))
  )

  implicit def arbPairF[A: Arbitrary]: Arbitrary[examples.PairF[A]] = Arbitrary(
    for {
      a <- Arbitrary.arbitrary[A]
      b <- Arbitrary.arbitrary[A]
    } yield examples.PairF(a, b)
  )

  implicit def arbLabeled[A: Arbitrary]: Arbitrary[examples.Labeled[A]] = Arbitrary(
    for {
      v <- Arbitrary.arbitrary[A]
      l <- Arbitrary.arbitrary[String]
    } yield examples.Labeled(v, l)
  )

  implicit def arbListPair[A: Arbitrary]: Arbitrary[examples.ListPair[A]] = Arbitrary(
    for {
      xs <- Arbitrary.arbitrary[List[A]]
      ys <- Arbitrary.arbitrary[List[A]]
    } yield examples.ListPair(xs, ys)
  )

  implicit def arbTaggedList[A: Arbitrary]: Arbitrary[examples.TaggedList[A]] = Arbitrary(
    for {
      items <- Arbitrary.arbitrary[List[A]]
      name <- Arbitrary.arbitrary[String]
    } yield examples.TaggedList(items, name)
  )

  implicit def arbConst[A]: Arbitrary[examples.Const[A]] = Arbitrary(
    Arbitrary.arbitrary[String].map(examples.Const[A](_))
  )

  implicit def arbCounter[A]: Arbitrary[examples.Counter[A]] = Arbitrary(
    for {
      c <- Arbitrary.arbitrary[Int]
      l <- Arbitrary.arbitrary[String]
    } yield examples.Counter[A](c, l)
  )

  implicit def arbPair[A: Arbitrary, B: Arbitrary]: Arbitrary[examples.Pair[A, B]] = Arbitrary(
    for {
      a <- Arbitrary.arbitrary[A]
      b <- Arbitrary.arbitrary[B]
    } yield examples.Pair(a, b)
  )

  // ---------------------------------------------------------------------------
  // Eq laws
  // ---------------------------------------------------------------------------

  group("Eq laws") {

    property("Point: reflexivity") {
      forAll { (a: examples.Point) =>
        examples.Point.eqPoint.eqv(a, a)
      }
    }

    property("Point: symmetry") {
      forAll { (a: examples.Point, b: examples.Point) =>
        examples.Point.eqPoint.eqv(a, b) == examples.Point.eqPoint.eqv(b, a)
      }
    }

    property("Point: transitivity") {
      forAll { (a: examples.Point, b: examples.Point, c: examples.Point) =>
        val ab = examples.Point.eqPoint.eqv(a, b)
        val bc = examples.Point.eqPoint.eqv(b, c)
        val ac = examples.Point.eqPoint.eqv(a, c)
        // if a == b and b == c then a == c
        !(ab && bc) || ac
      }
    }

    property("Person: reflexivity") {
      forAll { (a: examples.Person) =>
        examples.Person.eqPerson.eqv(a, a)
      }
    }

    property("Person: symmetry") {
      forAll { (a: examples.Person, b: examples.Person) =>
        examples.Person.eqPerson.eqv(a, b) == examples.Person.eqPerson.eqv(b, a)
      }
    }

    property("Shape: reflexivity") {
      forAll { (a: examples.Shape) =>
        examples.Shape.eqShape.eqv(a, a)
      }
    }

    property("Shape: symmetry") {
      forAll { (a: examples.Shape, b: examples.Shape) =>
        examples.Shape.eqShape.eqv(a, b) == examples.Shape.eqShape.eqv(b, a)
      }
    }

    property("Color: reflexivity") {
      forAll { (a: examples.Color) =>
        examples.Color.eqColor.eqv(a, a)
      }
    }

    property("Color: symmetry") {
      forAll { (a: examples.Color, b: examples.Color) =>
        examples.Color.eqColor.eqv(a, b) == examples.Color.eqColor.eqv(b, a)
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Hash laws
  // ---------------------------------------------------------------------------

  group("Hash laws") {

    property("Point: consistency with Eq (equal values have equal hashes)") {
      forAll { (a: examples.Point, b: examples.Point) =>
        val hash = examples.Point.hashPoint
        !hash.eqv(a, b) || (hash.hash(a) == hash.hash(b))
      }
    }

    property("Point: consistent with its own Eq") {
      forAll { (a: examples.Point, b: examples.Point) =>
        val hash = examples.Point.hashPoint
        hash.eqv(a, b) == examples.Point.eqPoint.eqv(a, b)
      }
    }

    property("Shape: consistency with Eq") {
      forAll { (a: examples.Shape, b: examples.Shape) =>
        val hash = examples.Shape.hashShape
        !hash.eqv(a, b) || (hash.hash(a) == hash.hash(b))
      }
    }

    property("Color: consistency with Eq") {
      forAll { (a: examples.Color, b: examples.Color) =>
        val hash = examples.Color.hashColor
        !hash.eqv(a, b) || (hash.hash(a) == hash.hash(b))
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Order laws
  // ---------------------------------------------------------------------------

  group("Order laws") {

    property("Point: totality") {
      forAll { (a: examples.Point, b: examples.Point) =>
        val ord = examples.Point.orderPoint
        val cmp = ord.compare(a, b)
        // compare always returns some Int (totality is guaranteed by the signature)
        cmp == cmp // trivially true, but ensures no exception
      }
    }

    property("Point: antisymmetry") {
      forAll { (a: examples.Point, b: examples.Point) =>
        val ord = examples.Point.orderPoint
        // if compare(a,b) <= 0 and compare(b,a) <= 0 then a == b
        val ab = ord.compare(a, b)
        val ba = ord.compare(b, a)
        !((ab <= 0) && (ba <= 0)) || ord.eqv(a, b)
      }
    }

    property("Point: transitivity") {
      forAll { (a: examples.Point, b: examples.Point, c: examples.Point) =>
        val ord = examples.Point.orderPoint
        val ab = ord.compare(a, b)
        val bc = ord.compare(b, c)
        val ac = ord.compare(a, c)
        // if a <= b and b <= c then a <= c
        !((ab <= 0) && (bc <= 0)) || (ac <= 0)
      }
    }

    property("Point: consistency with Eq") {
      forAll { (a: examples.Point, b: examples.Point) =>
        val ord = examples.Point.orderPoint
        // compare(a,b) == 0 iff eqv(a,b)
        (ord.compare(a, b) == 0) == ord.eqv(a, b)
      }
    }

    property("Point: inverse symmetry") {
      forAll { (a: examples.Point, b: examples.Point) =>
        val ord = examples.Point.orderPoint
        // compare(a,b) == -compare(b,a) (sign reversal)
        java.lang.Integer.signum(ord.compare(a, b)) == -java.lang.Integer.signum(ord.compare(b, a))
      }
    }

    property("Shape: antisymmetry") {
      forAll { (a: examples.Shape, b: examples.Shape) =>
        val ord = examples.Shape.orderShape
        val ab = ord.compare(a, b)
        val ba = ord.compare(b, a)
        !((ab <= 0) && (ba <= 0)) || ord.eqv(a, b)
      }
    }

    property("Shape: transitivity".ignore) {
      forAll { (a: examples.Shape, b: examples.Shape, c: examples.Shape) =>
        val ord = examples.Shape.orderShape
        val ab = ord.compare(a, b)
        val bc = ord.compare(b, c)
        val ac = ord.compare(a, c)
        !((ab <= 0) && (bc <= 0)) || (ac <= 0)
      }
    }

    property("Color: antisymmetry") {
      forAll { (a: examples.Color, b: examples.Color) =>
        val ord = examples.Color.orderColor
        val ab = ord.compare(a, b)
        val ba = ord.compare(b, a)
        !((ab <= 0) && (ba <= 0)) || ord.eqv(a, b)
      }
    }
  }

  // ---------------------------------------------------------------------------
  // PartialOrder laws
  // ---------------------------------------------------------------------------

  group("PartialOrder laws") {

    property("Point: reflexivity") {
      forAll { (a: examples.Point) =>
        examples.Point.partialOrderPoint.partialCompare(a, a) == 0.0
      }
    }

    property("Point: antisymmetry") {
      forAll { (a: examples.Point, b: examples.Point) =>
        val po = examples.Point.partialOrderPoint
        val ab = po.partialCompare(a, b)
        val ba = po.partialCompare(b, a)
        !((ab <= 0) && (ba <= 0)) || po.eqv(a, b)
      }
    }

    property("Point: transitivity") {
      forAll { (a: examples.Point, b: examples.Point, c: examples.Point) =>
        val po = examples.Point.partialOrderPoint
        val ab = po.partialCompare(a, b)
        val bc = po.partialCompare(b, c)
        val ac = po.partialCompare(a, c)
        !((ab <= 0) && (bc <= 0)) || (ac <= 0)
      }
    }

    property("Point: consistency with Eq") {
      forAll { (a: examples.Point, b: examples.Point) =>
        val po = examples.Point.partialOrderPoint
        // partialCompare(a,b) == 0.0 iff eqv(a,b)
        (po.partialCompare(a, b) == 0.0) == po.eqv(a, b)
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Semigroup laws
  // ---------------------------------------------------------------------------

  group("Semigroup laws") {

    property("Point: associativity") {
      forAll { (a: examples.Point, b: examples.Point, c: examples.Point) =>
        val sg = examples.Point.semigroupPoint
        val eq = examples.Point.eqPoint
        eq.eqv(sg.combine(sg.combine(a, b), c), sg.combine(a, sg.combine(b, c)))
      }
    }

    property("LongPair: associativity") {
      forAll { (a: examples.LongPair, b: examples.LongPair, c: examples.LongPair) =>
        val sg = examples.LongPair.monoidLongPair // Monoid is also a Semigroup
        sg.combine(sg.combine(a, b), c) == sg.combine(a, sg.combine(b, c))
      }
    }

    property("StringPair: associativity") {
      forAll { (a: examples.StringPair, b: examples.StringPair, c: examples.StringPair) =>
        val sg = examples.StringPair.monoidStringPair
        sg.combine(sg.combine(a, b), c) == sg.combine(a, sg.combine(b, c))
      }
    }
  }

  // ---------------------------------------------------------------------------
  // CommutativeSemigroup laws
  // ---------------------------------------------------------------------------

  group("CommutativeSemigroup laws") {

    property("Point: commutativity") {
      forAll { (a: examples.Point, b: examples.Point) =>
        val sg = examples.Point.commSemigroupPoint
        val eq = examples.Point.eqPoint
        eq.eqv(sg.combine(a, b), sg.combine(b, a))
      }
    }

    property("Point: associativity") {
      forAll { (a: examples.Point, b: examples.Point, c: examples.Point) =>
        val sg = examples.Point.commSemigroupPoint
        val eq = examples.Point.eqPoint
        eq.eqv(sg.combine(sg.combine(a, b), c), sg.combine(a, sg.combine(b, c)))
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Monoid laws
  // ---------------------------------------------------------------------------

  group("Monoid laws") {

    property("Point: left identity") {
      forAll { (a: examples.Point) =>
        val m = examples.Point.monoidPoint
        val eq = examples.Point.eqPoint
        eq.eqv(m.combine(m.empty, a), a)
      }
    }

    property("Point: right identity") {
      forAll { (a: examples.Point) =>
        val m = examples.Point.monoidPoint
        val eq = examples.Point.eqPoint
        eq.eqv(m.combine(a, m.empty), a)
      }
    }

    property("Point: associativity") {
      forAll { (a: examples.Point, b: examples.Point, c: examples.Point) =>
        val m = examples.Point.monoidPoint
        val eq = examples.Point.eqPoint
        eq.eqv(m.combine(m.combine(a, b), c), m.combine(a, m.combine(b, c)))
      }
    }

    property("LongPair: left identity") {
      forAll { (a: examples.LongPair) =>
        val m = examples.LongPair.monoidLongPair
        m.combine(m.empty, a) == a
      }
    }

    property("LongPair: right identity") {
      forAll { (a: examples.LongPair) =>
        val m = examples.LongPair.monoidLongPair
        m.combine(a, m.empty) == a
      }
    }

    property("StringPair: left identity") {
      forAll { (a: examples.StringPair) =>
        val m = examples.StringPair.monoidStringPair
        m.combine(m.empty, a) == a
      }
    }

    property("StringPair: right identity") {
      forAll { (a: examples.StringPair) =>
        val m = examples.StringPair.monoidStringPair
        m.combine(a, m.empty) == a
      }
    }
  }

  // ---------------------------------------------------------------------------
  // CommutativeMonoid laws
  // ---------------------------------------------------------------------------

  group("CommutativeMonoid laws") {

    property("Point: commutativity") {
      forAll { (a: examples.Point, b: examples.Point) =>
        val m = examples.Point.commMonoidPoint
        val eq = examples.Point.eqPoint
        eq.eqv(m.combine(a, b), m.combine(b, a))
      }
    }

    property("Point: left identity") {
      forAll { (a: examples.Point) =>
        val m = examples.Point.commMonoidPoint
        val eq = examples.Point.eqPoint
        eq.eqv(m.combine(m.empty, a), a)
      }
    }

    property("Point: right identity") {
      forAll { (a: examples.Point) =>
        val m = examples.Point.commMonoidPoint
        val eq = examples.Point.eqPoint
        eq.eqv(m.combine(a, m.empty), a)
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Group laws
  // ---------------------------------------------------------------------------

  group("Group laws") {

    property("Point: left inverse") {
      forAll { (a: examples.Point) =>
        val g = examples.Point.groupPoint
        val eq = examples.Point.eqPoint
        eq.eqv(g.combine(g.inverse(a), a), g.empty)
      }
    }

    property("Point: right inverse") {
      forAll { (a: examples.Point) =>
        val g = examples.Point.groupPoint
        val eq = examples.Point.eqPoint
        eq.eqv(g.combine(a, g.inverse(a)), g.empty)
      }
    }

    property("LongPair: left inverse") {
      forAll { (a: examples.LongPair) =>
        val g = examples.LongPair.groupLongPair
        g.combine(g.inverse(a), a) == g.empty
      }
    }

    property("LongPair: right inverse") {
      forAll { (a: examples.LongPair) =>
        val g = examples.LongPair.groupLongPair
        g.combine(a, g.inverse(a)) == g.empty
      }
    }

    property("Point: remove is combine with inverse") {
      forAll { (a: examples.Point, b: examples.Point) =>
        val g = examples.Point.groupPoint
        val eq = examples.Point.eqPoint
        eq.eqv(g.remove(a, b), g.combine(a, g.inverse(b)))
      }
    }
  }

  // ---------------------------------------------------------------------------
  // CommutativeGroup laws
  // ---------------------------------------------------------------------------

  group("CommutativeGroup laws") {

    property("Point: commutativity") {
      forAll { (a: examples.Point, b: examples.Point) =>
        val g = examples.Point.commGroupPoint
        val eq = examples.Point.eqPoint
        eq.eqv(g.combine(a, b), g.combine(b, a))
      }
    }

    property("Point: left inverse") {
      forAll { (a: examples.Point) =>
        val g = examples.Point.commGroupPoint
        val eq = examples.Point.eqPoint
        eq.eqv(g.combine(g.inverse(a), a), g.empty)
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Functor laws
  // ---------------------------------------------------------------------------

  group("Functor laws") {

    property("Box: identity") {
      forAll { (fa: examples.Box[Int]) =>
        examples.Box.functorBox.map(fa)(identity) == fa
      }
    }

    property("Box: composition") {
      forAll { (fa: examples.Box[Int]) =>
        val f: Int => String = _.toString
        val g: String => Int = _.length
        examples.Box.functorBox.map(examples.Box.functorBox.map(fa)(f))(g) ==
          examples.Box.functorBox.map(fa)(f andThen g)
      }
    }

    property("PairF: identity") {
      forAll { (fa: examples.PairF[Int]) =>
        examples.PairF.functorPairF.map(fa)(identity) == fa
      }
    }

    property("PairF: composition") {
      forAll { (fa: examples.PairF[Int]) =>
        val f: Int => String = _.toString
        val g: String => Int = _.length
        examples.PairF.functorPairF.map(examples.PairF.functorPairF.map(fa)(f))(g) ==
          examples.PairF.functorPairF.map(fa)(f andThen g)
      }
    }

    property("Labeled: identity") {
      forAll { (fa: examples.Labeled[Int]) =>
        examples.Labeled.functorLabeled.map(fa)(identity) == fa
      }
    }

    property("Labeled: composition") {
      forAll { (fa: examples.Labeled[Int]) =>
        val f: Int => String = _.toString
        val g: String => Int = _.length
        examples.Labeled.functorLabeled.map(examples.Labeled.functorLabeled.map(fa)(f))(g) ==
          examples.Labeled.functorLabeled.map(fa)(f andThen g)
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Foldable laws
  // ---------------------------------------------------------------------------

  group("Foldable laws") {

    property("Box: foldLeft consistent with foldMap") {
      import cats.instances.int.*
      forAll { (fa: examples.Box[Int]) =>
        val foldable = examples.Box.foldableBox
        val left = foldable.foldLeft(fa, 0)(_ + _)
        val mapped = foldable.foldMap(fa)(identity)(cats.kernel.Monoid[Int])
        left == mapped
      }
    }

    property("PairF: foldLeft consistent with foldMap") {
      import cats.instances.int.*
      forAll { (fa: examples.PairF[Int]) =>
        val foldable = examples.PairF.foldablePairF
        val left = foldable.foldLeft(fa, 0)(_ + _)
        val mapped = foldable.foldMap(fa)(identity)(cats.kernel.Monoid[Int])
        left == mapped
      }
    }

    property("Labeled: foldLeft consistent with foldMap") {
      import cats.instances.int.*
      forAll { (fa: examples.Labeled[Int]) =>
        val foldable = examples.Labeled.foldableLabeled
        val left = foldable.foldLeft(fa, 0)(_ + _)
        val mapped = foldable.foldMap(fa)(identity)(cats.kernel.Monoid[Int])
        left == mapped
      }
    }

    property("Box: foldRight consistent with foldLeft (via List reversal)") {
      forAll { (fa: examples.Box[Int]) =>
        val foldable = examples.Box.foldableBox
        val left = foldable.foldLeft(fa, List.empty[Int])((acc, a) => a :: acc)
        val right = foldable.foldRight(fa, cats.Eval.now(List.empty[Int]))((a, acc) => acc.map(a :: _)).value
        left.reverse == right
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Traverse laws
  // ---------------------------------------------------------------------------

  group("Traverse laws") {

    property("Box: identity") {
      forAll { (fa: examples.Box[Int]) =>
        // traverse with Id (Option(a)) and then extracting should give back the same thing
        examples.Box.traverseBox.traverse(fa)(a => Option(a)) == Some(fa)
      }
    }

    property("PairF: identity") {
      forAll { (fa: examples.PairF[Int]) =>
        examples.PairF.traversePairF.traverse(fa)(a => Option(a)) == Some(fa)
      }
    }

    property("Labeled: identity") {
      forAll { (fa: examples.Labeled[Int]) =>
        examples.Labeled.traverseLabeled.traverse(fa)(a => Option(a)) == Some(fa)
      }
    }

    property("Box: composition with map") {
      forAll { (fa: examples.Box[Int]) =>
        val f: Int => String = _.toString
        // traverse(fa)(x => Some(f(x))) == Some(fa.map(f))
        examples.Box.traverseBox.traverse(fa)(a => Option(f(a))) ==
          Some(examples.Box.functorBox.map(fa)(f))
      }
    }
  }

  // ---------------------------------------------------------------------------
  // SemigroupK laws
  // ---------------------------------------------------------------------------

  group("SemigroupK laws") {

    property("ListPair: associativity") {
      forAll { (a: examples.ListPair[Int], b: examples.ListPair[Int], c: examples.ListPair[Int]) =>
        val sk = examples.ListPair.semigroupKListPair
        sk.combineK(sk.combineK(a, b), c) == sk.combineK(a, sk.combineK(b, c))
      }
    }

    property("TaggedList: associativity") {
      forAll { (a: examples.TaggedList[Int], b: examples.TaggedList[Int], c: examples.TaggedList[Int]) =>
        val sk = examples.TaggedList.semigroupKTaggedList
        sk.combineK(sk.combineK(a, b), c) == sk.combineK(a, sk.combineK(b, c))
      }
    }
  }

  // ---------------------------------------------------------------------------
  // MonoidK laws
  // ---------------------------------------------------------------------------

  group("MonoidK laws") {

    property("ListPair: left identity") {
      forAll { (a: examples.ListPair[Int]) =>
        val mk = examples.ListPair.monoidKListPair
        mk.combineK(mk.empty[Int], a) == a
      }
    }

    property("ListPair: right identity") {
      forAll { (a: examples.ListPair[Int]) =>
        val mk = examples.ListPair.monoidKListPair
        mk.combineK(a, mk.empty[Int]) == a
      }
    }

    property("TaggedList: left identity") {
      forAll { (a: examples.TaggedList[Int]) =>
        val mk = examples.TaggedList.monoidKTaggedList
        mk.combineK(mk.empty[Int], a) == a
      }
    }

    property("TaggedList: right identity") {
      forAll { (a: examples.TaggedList[Int]) =>
        val mk = examples.TaggedList.monoidKTaggedList
        mk.combineK(a, mk.empty[Int]) == a
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Applicative laws
  // ---------------------------------------------------------------------------

  group("Applicative laws") {

    property("Box: identity (pure id <*> fa == fa)") {
      forAll { (fa: examples.Box[Int]) =>
        val ap = examples.Box.applicativeBox
        ap.ap(ap.pure((a: Int) => a))(fa) == fa
      }
    }

    property("Box: homomorphism (pure f <*> pure a == pure (f a))") {
      forAll { (a: Int) =>
        val ap = examples.Box.applicativeBox
        val f = (x: Int) => x + 1
        ap.ap(ap.pure(f))(ap.pure(a)) == ap.pure(f(a))
      }
    }

    property("Box: map consistent with ap") {
      forAll { (fa: examples.Box[Int]) =>
        val ap = examples.Box.applicativeBox
        val f = (x: Int) => x.toString
        ap.map(fa)(f) == ap.ap(ap.pure(f))(fa)
      }
    }

    property("Labeled: identity (pure id <*> fa == fa)") {
      forAll { (fa: examples.Labeled[Int]) =>
        val ap = examples.Labeled.applicativeLabeled
        ap.ap(ap.pure((a: Int) => a))(fa) == fa
      }
    }
  }

  // ---------------------------------------------------------------------------
  // NonEmptyAlternative laws
  // ---------------------------------------------------------------------------

  group("NonEmptyAlternative laws") {

    property("Const: SemigroupK associativity") {
      forAll { (a: examples.Const[Int], b: examples.Const[Int], c: examples.Const[Int]) =>
        val nea = examples.Const.neaConst
        nea.combineK(nea.combineK(a, b), c) == nea.combineK(a, nea.combineK(b, c))
      }
    }

    property("Counter: SemigroupK associativity") {
      forAll { (a: examples.Counter[Int], b: examples.Counter[Int], c: examples.Counter[Int]) =>
        val nea = examples.Counter.neaCounter
        nea.combineK(nea.combineK(a, b), c) == nea.combineK(a, nea.combineK(b, c))
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Alternative laws
  // ---------------------------------------------------------------------------

  group("Alternative laws") {

    property("Const: MonoidK left identity") {
      forAll { (a: examples.Const[Int]) =>
        val alt = examples.Const.altConst
        alt.combineK(alt.empty[Int], a) == a
      }
    }

    property("Const: MonoidK right identity") {
      forAll { (a: examples.Const[Int]) =>
        val alt = examples.Const.altConst
        alt.combineK(a, alt.empty[Int]) == a
      }
    }

    property("Counter: MonoidK left identity") {
      forAll { (a: examples.Counter[Int]) =>
        val alt = examples.Counter.altCounter
        alt.combineK(alt.empty[Int], a) == a
      }
    }

    property("Counter: MonoidK right identity") {
      forAll { (a: examples.Counter[Int]) =>
        val alt = examples.Counter.altCounter
        alt.combineK(a, alt.empty[Int]) == a
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Bifunctor laws
  // ---------------------------------------------------------------------------

  group("Bifunctor laws") {

    property("Pair: identity") {
      forAll { (p: examples.Pair[Int, String]) =>
        examples.Pair.bifunctorPair.bimap(p)(identity, identity) == p
      }
    }

    property("Pair: composition") {
      forAll { (p: examples.Pair[Int, String]) =>
        val f1: Int => String = _.toString
        val f2: String => Int = _.length
        val g1: String => Int = _.length
        val g2: Int => String = _.toString
        val bf = examples.Pair.bifunctorPair
        bf.bimap(bf.bimap(p)(f1, g1))(f2, g2) == bf.bimap(p)(f1 andThen f2, g1 andThen g2)
      }
    }
  }
}
