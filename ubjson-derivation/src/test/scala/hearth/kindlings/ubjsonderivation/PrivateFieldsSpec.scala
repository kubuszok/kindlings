package hearth.kindlings.ubjsonderivation

import hearth.MacroSuite

final case class UbPrivate(a: Int, private val b: String, c: Boolean) {
  def bValue: String = b
}

/** As jsoniter-scala (and jsoniter-derivation): a constructor parameter without an accessor cannot be written, so it is
  * a compile-time error rather than a field silently left out.
  */
final class PrivateFieldsSpec extends MacroSuite {

  test("private val constructor fields are a compile-time error, as in jsoniter-scala") {
    compileErrors(
      "hearth.kindlings.ubjsonderivation.UBJsonValueCodec.derived[hearth.kindlings.ubjsonderivation.UbPrivate]"
    ).check(
      "'b' parameter of 'hearth.kindlings.ubjsonderivation.UbPrivate' should be defined as 'val' or 'var' in the primary constructor."
    )
  }
}
