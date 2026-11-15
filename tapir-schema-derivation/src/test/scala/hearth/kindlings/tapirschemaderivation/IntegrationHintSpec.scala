package hearth.kindlings.tapirschemaderivation

import eu.timepit.refined.api.Refined
import hearth.MacroSuite
import hearth.kindlings.circederivation.Configuration

final class HintNonEmpty

// The encoding of a scala-newtype `@newtype case class HintAge(value: Int)` (see kindlings-newtype-integration).
object HintAge {
  type Repr = Int
  type Base = Any
  trait Tag extends Any
  type Type <: Base with Tag
  def unsafeUnwrap(value: Type): Int = value.asInstanceOf[Int]
}

final case class HintRefinedKey(byKey: Map[Refined[String, HintNonEmpty], Int])
final case class HintRefinedField(name: Refined[String, HintNonEmpty])
final case class HintNewtypeKey(byKey: Map[HintAge.Type, Int])
final case class HintNewtypeField(age: HintAge.Type)

/** Without the integration module making a refined/newtype type a value type, the errors say which module it is. */
final class IntegrationHintSpec extends MacroSuite {

  implicit val config: Configuration = Configuration.default
  implicit val jsoniterConfig: hearth.kindlings.jsoniterderivation.JsoniterConfig =
    hearth.kindlings.jsoniterderivation.JsoniterConfig.default

  group("errors for refined's Refined point at kindlings-refined-integration") {

    test("tapir schema: map key") {
      compileErrors("KindlingsSchema.derived[HintRefinedKey]").check(
        "Cannot derive tapir Schema for Map with key type",
        "looks like a type of refined: add kindlings-refined-integration to the dependencies"
      )
    }

    test("tapir schema: field") {
      compileErrors("KindlingsSchema.derived[HintRefinedField]").check(
        "looks like a type of refined: add kindlings-refined-integration to the dependencies"
      )
    }

    test("jsoniter codec: map key and field") {
      compileErrors("hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[HintRefinedKey]").check(
        "kindlings-refined-integration"
      )
      compileErrors("hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[HintRefinedField]").check(
        "kindlings-refined-integration"
      )
    }

    test("circe encoder/decoder: map key and field") {
      compileErrors("hearth.kindlings.circederivation.KindlingsEncoder.derived[HintRefinedKey]").check(
        "kindlings-refined-integration"
      )
      compileErrors("hearth.kindlings.circederivation.KindlingsDecoder.derived[HintRefinedField]").check(
        "kindlings-refined-integration"
      )
    }
  }

  group("errors for scala-newtype's @newtype point at kindlings-newtype-integration") {

    test("tapir schema: map key and field") {
      compileErrors("KindlingsSchema.derived[HintNewtypeKey]").check(
        "looks like a type of scala-newtype: add kindlings-newtype-integration to the dependencies"
      )
      compileErrors("KindlingsSchema.derived[HintNewtypeField]").check(
        "looks like a type of scala-newtype: add kindlings-newtype-integration to the dependencies"
      )
    }

    test("jsoniter codec and circe encoder: map key") {
      compileErrors("hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[HintNewtypeKey]").check(
        "kindlings-newtype-integration"
      )
      compileErrors("hearth.kindlings.circederivation.KindlingsEncoder.derived[HintNewtypeKey]").check(
        "kindlings-newtype-integration"
      )
    }
  }

  test("other unsupported types get no hint") {
    val errors = compileErrors("KindlingsSchema.derived[hearth.kindlings.tapirschemaderivation.WithProductKeyMap]")
    errors.check("Cannot derive tapir Schema for Map with key type")
    errors.checkNot("looks like a type of")
  }
}
