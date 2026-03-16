package hearth.kindlings.integrationtests

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.any.*
import io.github.iltotore.iron.constraint.numeric.*
import io.github.iltotore.iron.constraint.string.*
import hearth.MacroSuite
import hearth.kindlings.ubjsonderivation.UBJsonValueCodec
import hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils

final class IronUBJsonSpec extends MacroSuite {

  private def roundTrip[A](value: A)(implicit codec: hearth.kindlings.ubjsonderivation.UBJsonValueCodec[A]): A =
    UBJsonDerivationUtils.readFromBytes[A](UBJsonDerivationUtils.writeToBytes[A](value)(codec))(codec)

  group("Iron + UBJson") {

    group("encoding") {

      test("case class with iron fields encodes correctly") {
        val codec = UBJsonValueCodec.derived[IronPerson]
        val person = IronPerson("Alice", 30)
        val bytes = UBJsonDerivationUtils.writeToBytes(person)(codec)
        assert(bytes.nonEmpty)
        val decoded = UBJsonDerivationUtils.readFromBytes[IronPerson](bytes)(codec)
        assertEquals(decoded.name: String, "Alice")
        assertEquals(decoded.age: Int, 30)
      }
    }

    group("decoding valid") {

      test("case class with iron fields decodes via round-trip") {
        val codec = UBJsonValueCodec.derived[IronPerson]
        val person = IronPerson("Alice", 30)
        val bytes = UBJsonDerivationUtils.writeToBytes(person)(codec)
        val result = UBJsonDerivationUtils.readFromBytes[IronPerson](bytes)(codec)
        assertEquals(result.name: String, "Alice")
        assertEquals(result.age: Int, 30)
      }
    }

    group("decoding invalid") {

      test("iron positive rejects negative with error") {
        val validCodec = UBJsonValueCodec.derived[IronPerson]
        val plainCodec = UBJsonValueCodec.derived[PlainPerson]
        val bytes = UBJsonDerivationUtils.writeToBytes(PlainPerson("Alice", -1))(plainCodec)
        intercept[hearth.kindlings.ubjsonderivation.UBJsonReaderException] {
          UBJsonDerivationUtils.readFromBytes[IronPerson](bytes)(validCodec)
        }
      }
    }

    group("round-trip") {

      test("encode then decode preserves value") {
        implicit val codec: hearth.kindlings.ubjsonderivation.UBJsonValueCodec[IronPerson] =
          UBJsonValueCodec.derived[IronPerson]
        val person = IronPerson("Alice", 30)
        val decoded = roundTrip(person)
        assertEquals(decoded.name: String, "Alice")
        assertEquals(decoded.age: Int, 30)
      }
    }
  }
}
