package hearth.kindlings.pureconfigderivation

import com.typesafe.config.ConfigRenderOptions
import hearth.MacroSuite
import pureconfig.ConfigCursor

final case class PcPrivate(a: Int, private val b: String, c: Boolean) {
  def bValue: String = b
}

// Hearth's `caseFieldValuesAt` has no `private val` constructor parameters (they have no accessor the generated code could
// call): derivations which read fields include them anyway (through `Product.productElement`), matched by name, so the
// fields after them are not shifted.
final class PrivateFieldsSpec extends MacroSuite {

  test("private val constructor fields are written and read, as pureconfig-generic does") {
    val value = PcPrivate(1, "two", c = true)
    val written = KindlingsConfigWriter.derived[PcPrivate].to(value)
    written.render(ConfigRenderOptions.concise) ==> """{"a":1,"b":"two","c":true}"""
    KindlingsConfigReader.derived[PcPrivate].from(ConfigCursor(written, Nil)) ==> Right(value)
  }
}
