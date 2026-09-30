package hearth.kindlings.integrationtests

import hearth.MacroSuite
import hearth.kindlings.ubjsonderivation.UBJsonValueCodec
import hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils

final class NewtypeUBJsonSpec extends MacroSuite {

  group("scala-newtype + UBJson") {

    test("encode then decode preserves value") {
      val codec = UBJsonValueCodec.derived[NewtypeUser]
      val bytes = UBJsonDerivationUtils.writeToBytes(NewtypeUser.example)(codec)
      UBJsonDerivationUtils.readFromBytes[NewtypeUser](bytes)(codec) ==> NewtypeUser.example
    }
  }
}
