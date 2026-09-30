package hearth.kindlings.integrationtests

import hearth.MacroSuite
import hearth.kindlings.ubjsonderivation.UBJsonValueCodec
import hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils

final class NeotypeUBJsonSpec extends MacroSuite {

  group("Neotype + UBJson") {

    test("encode then decode preserves value") {
      val codec = UBJsonValueCodec.derived[NeotypePerson]
      val person = NeotypePerson(NeotypeEmail("alice@example.com"), NeotypeAge(30), NeotypeNickname("ally"))
      val bytes = UBJsonDerivationUtils.writeToBytes(person)(codec)
      UBJsonDerivationUtils.readFromBytes[NeotypePerson](bytes)(codec) ==> person
    }

    test("neotype validation rejects invalid value with error") {
      // Encode invalid values through a plain surrogate with the same shape
      val plainCodec = UBJsonValueCodec.derived[PlainNeotypePerson]
      val bytes = UBJsonDerivationUtils.writeToBytes(PlainNeotypePerson("alice@example.com", -1, "ally"))(plainCodec)
      intercept[hearth.kindlings.ubjsonderivation.UBJsonReaderException] {
        UBJsonDerivationUtils.readFromBytes[NeotypePerson](bytes)(UBJsonValueCodec.derived[NeotypePerson])
      }
    }
  }
}
