package hearth.kindlings.jsoniterderivation

/** Configs defined in the test module itself (see `Scala2InlineConfigSpec`): never evaluated at compile time. */
object SameModuleConfigs {
  val decodingAndEncodingOnly: JsoniterConfig = JsoniterConfig.default.withDecodingOnly.withEncodingOnly
}
