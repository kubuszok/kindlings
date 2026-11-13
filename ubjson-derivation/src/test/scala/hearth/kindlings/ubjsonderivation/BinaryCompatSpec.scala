package hearth.kindlings.ubjsonderivation

import hearth.MacroSuite
import hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils

import scala.annotation.nowarn

/** The members kept (deprecated) for binary compatibility with Kindlings 0.3.x keep their 0.3.2 behaviour. */
@nowarn("cat=deprecation")
final class BinaryCompatSpec extends MacroSuite {

  group("UBJsonConfig of 0.3.2") {

    test("the old-arity constructor and apply fill the new fields with their defaults") {
      val f: String => String = _.toUpperCase
      val viaApply = UBJsonConfig(f, f, Some("t"), false, true, false, false, false, false, false, true, 10, 11, 12,
        13, 14)
      val viaNew = new UBJsonConfig(f, f, Some("t"), false, true, false, false, false, false, false, true, 10, 11, 12,
        13, 14)
      for (c <- List(viaApply, viaNew)) {
        c.discriminatorFieldName ==> Some("t")
        c.checkFieldDuplication ==> true
        c.setMaxInsertNumber ==> 14
        c.circeLikeObjectEncoding ==> false
        c.javaEnumValueNameMapper("a") ==> "a"
        c.bigIntDigitsLimit ==> 308
        c.requireDiscriminatorFirst ==> true
        c.transientNull ==> false
      }
    }

    test("the old-arity copy keeps the new fields") {
      val base = UBJsonConfig.default.withRequireDiscriminatorFirst(false).withBigIntDigitsLimit(5)
      val c = base.copy(identity, identity, None, true, false, false, false, false, false, false, false, 34, 6178, 308,
        1, 2)
      c.mapMaxInsertNumber ==> 1
      c.requireDiscriminatorFirst ==> false
      c.bigIntDigitsLimit ==> 5
    }

    test("named copy still uses the full-arity copy") {
      UBJsonConfig.default.copy(transientNull = true) ==> UBJsonConfig(transientNull = true)
    }

    test("the 0.3.2 name mappers forward to the jsoniter-scala ones") {
      UBJsonConfig.snakeCase("fooBar") ==> "foo_bar"
      UBJsonConfig.kebabCase("fooBar") ==> "foo-bar"
      UBJsonConfig.pascalCase("fooBar") ==> "FooBar"
    }
  }

  test("UBJsonDerivationUtils.throwMissingField of 0.3.2") {
    intercept[IllegalArgumentException](UBJsonDerivationUtils.throwMissingField("a")).getMessage ==>
      "missing required field: a"
  }
}
