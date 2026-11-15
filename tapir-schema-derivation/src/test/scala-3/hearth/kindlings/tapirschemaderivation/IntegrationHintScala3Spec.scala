package hearth.kindlings.tapirschemaderivation

import hearth.MacroSuite
import hearth.kindlings.circederivation.Configuration

object HintIronKey extends io.github.iltotore.iron.RefinedType[String, HintNonEmpty]
type HintIronKey = HintIronKey.T

object HintNeoName extends neotype.Newtype[String]
type HintNeoName = HintNeoName.Type

final case class HintIronKeyed(byKey: Map[HintIronKey, Int])
final case class HintIronField(key: HintIronKey)
final case class HintNeoKeyed(byKey: Map[HintNeoName, Int])
final case class HintNeoField(name: HintNeoName)

/** Iron `RefinedType` and neotype `Newtype` (Scala 3 only) without their integration modules: the errors say which. */
final class IntegrationHintScala3Spec extends MacroSuite {

  given Configuration = Configuration.default
  given hearth.kindlings.jsoniterderivation.JsoniterConfig = hearth.kindlings.jsoniterderivation.JsoniterConfig.default

  test("Iron RefinedType: tapir schema, jsoniter codec and circe encoder point at kindlings-iron-integration") {
    compileErrors("KindlingsSchema.derived[HintIronKeyed]").check(
      "Cannot derive tapir Schema for Map with key type",
      "looks like a type of Iron: add kindlings-iron-integration to the dependencies"
    )
    compileErrors("KindlingsSchema.derived[HintIronField]").check(
      "looks like a type of Iron: add kindlings-iron-integration to the dependencies"
    )
    compileErrors("hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[HintIronKeyed]").check(
      "kindlings-iron-integration"
    )
    compileErrors("hearth.kindlings.circederivation.KindlingsEncoder.derived[HintIronKeyed]").check(
      "kindlings-iron-integration"
    )
  }

  test("neotype Newtype: tapir schema, jsoniter codec and circe encoder point at kindlings-neotype-integration") {
    compileErrors("KindlingsSchema.derived[HintNeoKeyed]").check(
      "looks like a type of neotype: add kindlings-neotype-integration to the dependencies"
    )
    compileErrors("KindlingsSchema.derived[HintNeoField]").check(
      "looks like a type of neotype: add kindlings-neotype-integration to the dependencies"
    )
    compileErrors("hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[HintNeoField]").check(
      "kindlings-neotype-integration"
    )
    compileErrors("hearth.kindlings.circederivation.KindlingsDecoder.derived[HintNeoKeyed]").check(
      "kindlings-neotype-integration"
    )
  }
}
