package hearth.kindlings.ubjsonderivation

import hearth.MacroSuite
import hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils

final case class UbConsBox(xs: ::[Int])
final case class UbListBox(xs: List[Int])

/** `::` (a non-empty `List`) is a collection (Hearth's `IsCollection` provider): an array, rejected when empty. */
final class ListConsSpec extends MacroSuite {

  group("::") {

    test("is written as the List it is and read back") {
      val consCodec = UBJsonValueCodec.derived[UbConsBox]
      val listCodec = UBJsonValueCodec.derived[UbListBox]
      val value = UbConsBox(::(1, List(2, 3)))
      val bytes = UBJsonDerivationUtils.writeToBytes[UbConsBox](value)(consCodec)
      bytes.toList ==> UBJsonDerivationUtils.writeToBytes[UbListBox](UbListBox(List(1, 2, 3)))(listCodec).toList
      UBJsonDerivationUtils.readFromBytes[UbConsBox](bytes)(consCodec) ==> value
    }

    test("an empty array is rejected") {
      val consCodec = UBJsonValueCodec.derived[UbConsBox]
      val listCodec = UBJsonValueCodec.derived[UbListBox]
      val bytes = UBJsonDerivationUtils.writeToBytes[UbListBox](UbListBox(Nil))(listCodec)
      assert(scala.util.Try(UBJsonDerivationUtils.readFromBytes[UbConsBox](bytes)(consCodec)).isFailure)
    }
  }
}
