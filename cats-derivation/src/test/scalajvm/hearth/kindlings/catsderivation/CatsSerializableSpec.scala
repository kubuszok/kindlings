package hearth.kindlings.catsderivation

import hearth.MacroSuite

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

/** Verifies that derived cats type class instances survive Java serialization round-trips (JVM only).
  *
  * This is important for Spark, Flink, and other distributed frameworks that serialize closures containing type class
  * instances. Gap 3.5 from GAPS.md.
  */
final class CatsSerializableSpec extends MacroSuite {

  private def roundTripSerialize[A <: Serializable](a: A): A = {
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(a)
    oos.close()
    val bais = new ByteArrayInputStream(baos.toByteArray)
    val ois = new ObjectInputStream(bais)
    ois.readObject().asInstanceOf[A]
  }

  group("Serializable round-trip: monomorphic type classes") {

    test("Show instance survives serialization") {
      val original = examples.Point.showPoint
      val deserialized = roundTripSerialize(original.asInstanceOf[Serializable]).asInstanceOf[cats.Show[examples.Point]]
      deserialized.show(examples.Point(1, 2)) ==> original.show(examples.Point(1, 2))
    }

    test("Eq instance survives serialization") {
      val original = examples.Point.eqPoint
      val deserialized =
        roundTripSerialize(original.asInstanceOf[Serializable]).asInstanceOf[cats.kernel.Eq[examples.Point]]
      deserialized.eqv(examples.Point(1, 2), examples.Point(1, 2)) ==> true
      deserialized.eqv(examples.Point(1, 2), examples.Point(3, 4)) ==> false
    }

    test("Order instance survives serialization") {
      val original = examples.Point.orderPoint
      val deserialized =
        roundTripSerialize(original.asInstanceOf[Serializable]).asInstanceOf[cats.kernel.Order[examples.Point]]
      deserialized.compare(examples.Point(1, 2), examples.Point(1, 2)) ==> 0
      assert(deserialized.compare(examples.Point(1, 2), examples.Point(2, 2)) < 0)
    }

    test("Hash instance survives serialization") {
      val original = examples.Point.hashPoint
      val deserialized =
        roundTripSerialize(original.asInstanceOf[Serializable]).asInstanceOf[cats.kernel.Hash[examples.Point]]
      val a = examples.Point(1, 2)
      val b = examples.Point(1, 2)
      deserialized.hash(a) ==> deserialized.hash(b)
      deserialized.eqv(a, b) ==> true
      deserialized.eqv(a, examples.Point(3, 4)) ==> false
    }

    test("Semigroup instance survives serialization") {
      val original = examples.Point.semigroupPoint
      val deserialized =
        roundTripSerialize(original.asInstanceOf[Serializable]).asInstanceOf[cats.kernel.Semigroup[examples.Point]]
      deserialized.combine(examples.Point(1, 2), examples.Point(3, 4)) ==> examples.Point(4, 6)
    }

    test("Monoid instance survives serialization") {
      val original = examples.Point.monoidPoint
      val deserialized =
        roundTripSerialize(original.asInstanceOf[Serializable]).asInstanceOf[cats.kernel.Monoid[examples.Point]]
      deserialized.empty ==> examples.Point(0, 0)
      deserialized.combine(examples.Point(1, 2), examples.Point(3, 4)) ==> examples.Point(4, 6)
    }

    test("Group instance survives serialization") {
      val original = examples.Point.groupPoint
      val deserialized =
        roundTripSerialize(original.asInstanceOf[Serializable]).asInstanceOf[cats.kernel.Group[examples.Point]]
      deserialized.empty ==> examples.Point(0, 0)
      deserialized.inverse(examples.Point(3, 7)) ==> examples.Point(-3, -7)
      deserialized.combine(examples.Point(1, 2), examples.Point(3, 4)) ==> examples.Point(4, 6)
    }

    test("Empty instance survives serialization") {
      val original = examples.Point.emptyPoint
      val deserialized =
        roundTripSerialize(original.asInstanceOf[Serializable]).asInstanceOf[alleycats.Empty[examples.Point]]
      deserialized.empty ==> examples.Point(0, 0)
    }
  }

  group("Serializable round-trip: enum type classes") {

    test("Show for sealed trait survives serialization") {
      val original = examples.Color.showColor
      val deserialized =
        roundTripSerialize(original.asInstanceOf[Serializable]).asInstanceOf[cats.Show[examples.Color]]
      deserialized.show(examples.Red) ==> original.show(examples.Red)
      deserialized.show(examples.Green) ==> original.show(examples.Green)
    }

    test("Eq for sealed trait survives serialization") {
      val original = examples.Color.eqColor
      val deserialized =
        roundTripSerialize(original.asInstanceOf[Serializable]).asInstanceOf[cats.kernel.Eq[examples.Color]]
      deserialized.eqv(examples.Red, examples.Red) ==> true
      deserialized.eqv(examples.Red, examples.Blue) ==> false
    }
  }
}
