package hearth.kindlings.sconfigderivation

import hearth.MacroSuite
import org.ekrich.config.ConfigRenderOptions

final case class ScPrivate(a: Int, private val b: String, c: Boolean) {
  def bValue: String = b
}

// Hearth's `caseFieldValuesAt` has no `private val` constructor parameters (they have no accessor the generated code could
// call): derivations which read fields include them anyway (through `Product.productElement`), matched by name, so the
// fields after them are not shifted.
final class PrivateFieldsSpec extends MacroSuite {

  test("private val constructor fields are written and read") {
    val value = ScPrivate(1, "two", c = true)
    val written = ConfigWriter.derived[ScPrivate].to(value)
    val rendered = written.render(ConfigRenderOptions.concise)
    assert(rendered.contains("\"b\":\"two\""), rendered)
    assert(rendered.contains("\"c\":true"), rendered)
    ConfigReader.derived[ScPrivate].from(written) ==> Right(value)
  }
}
