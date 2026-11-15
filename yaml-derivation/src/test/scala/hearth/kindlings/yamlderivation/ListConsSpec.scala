package hearth.kindlings.yamlderivation

import hearth.MacroSuite

final case class YConsBox(xs: ::[Int])
final case class YListBox(xs: List[Int])

/** `::` (a non-empty `List`) is a collection (Hearth's `IsCollection` provider): a YAML sequence, rejected when empty.
  */
final class ListConsSpec extends MacroSuite {

  group("::") {

    test("is encoded as a sequence and decoded back") {
      val value = YConsBox(::(1, List(2, 3)))
      val node = KindlingsYamlEncoder.encode(value)
      node ==> KindlingsYamlEncoder.encode(YListBox(List(1, 2, 3)))
      KindlingsYamlDecoder.decode[YConsBox](node) ==> Right(value)
    }

    test("an empty sequence is rejected") {
      assert(KindlingsYamlDecoder.decode[YConsBox](KindlingsYamlEncoder.encode(YListBox(Nil))).isLeft)
    }
  }
}
