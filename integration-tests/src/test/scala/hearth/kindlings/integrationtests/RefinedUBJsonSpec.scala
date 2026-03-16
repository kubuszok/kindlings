package hearth.kindlings.integrationtests

import eu.timepit.refined.refineV
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Positive
import hearth.MacroSuite
import hearth.kindlings.ubjsonderivation.UBJsonValueCodec
import hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils

final class RefinedUBJsonSpec extends MacroSuite {

  private val alice = refineV[NonEmpty]("Alice").toOption.get
  private val thirty = refineV[Positive](30).toOption.get

  private def roundTrip[A](value: A)(implicit codec: hearth.kindlings.ubjsonderivation.UBJsonValueCodec[A]): A =
    UBJsonDerivationUtils.readFromBytes[A](UBJsonDerivationUtils.writeToBytes[A](value)(codec))(codec)

  group("Refined + UBJson") {

    group("encoding") {

      test("case class with refined fields encodes correctly") {
        val codec = UBJsonValueCodec.derived[RefinedPerson]
        val person = RefinedPerson(alice, thirty)
        val bytes = UBJsonDerivationUtils.writeToBytes(person)(codec)
        assert(bytes.nonEmpty)
        // Round-trip to verify encoding is correct
        val decoded = UBJsonDerivationUtils.readFromBytes[RefinedPerson](bytes)(codec)
        assertEquals(decoded.name.value, "Alice")
        assertEquals(decoded.age.value, 30)
      }
    }

    group("decoding valid") {

      test("case class with refined fields decodes via round-trip") {
        val codec = UBJsonValueCodec.derived[RefinedPerson]
        val person = RefinedPerson(alice, thirty)
        val bytes = UBJsonDerivationUtils.writeToBytes(person)(codec)
        val result = UBJsonDerivationUtils.readFromBytes[RefinedPerson](bytes)(codec)
        assertEquals(result.name.value, "Alice")
        assertEquals(result.age.value, 30)
      }
    }

    group("decoding invalid") {

      test("refined positive rejects negative with error") {
        val validCodec = UBJsonValueCodec.derived[RefinedPerson]
        // Encode a plain case class with invalid refined values by using a surrogate type
        val plainCodec = UBJsonValueCodec.derived[PlainPerson]
        val bytes = UBJsonDerivationUtils.writeToBytes(PlainPerson("Alice", -1))(plainCodec)
        intercept[hearth.kindlings.ubjsonderivation.UBJsonReaderException] {
          UBJsonDerivationUtils.readFromBytes[RefinedPerson](bytes)(validCodec)
        }
      }

      test("refined non-empty rejects empty string with error") {
        val validCodec = UBJsonValueCodec.derived[RefinedPerson]
        val plainCodec = UBJsonValueCodec.derived[PlainPerson]
        val bytes = UBJsonDerivationUtils.writeToBytes(PlainPerson("", 30))(plainCodec)
        intercept[hearth.kindlings.ubjsonderivation.UBJsonReaderException] {
          UBJsonDerivationUtils.readFromBytes[RefinedPerson](bytes)(validCodec)
        }
      }
    }

    group("round-trip") {

      test("encode then decode preserves value") {
        implicit val codec: hearth.kindlings.ubjsonderivation.UBJsonValueCodec[RefinedPerson] =
          UBJsonValueCodec.derived[RefinedPerson]
        val person = RefinedPerson(alice, thirty)
        val decoded = roundTrip(person)
        assertEquals(decoded.name.value, "Alice")
        assertEquals(decoded.age.value, 30)
      }
    }
  }
}
