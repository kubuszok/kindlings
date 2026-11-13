package hearth.kindlings.ubjsonderivation

/** Codecs whose config is passed explicitly, so the macro evaluates it at compile time (optimized codegen). Per Scala
  * version only because passing the config explicitly differs (`derived[A](config)` vs `derived[A](using config)`).
  */
object UbPortOptimizedCodecs {
  val scalaTransient: UBJsonValueCodec[UbScalaTransient] =
    UBJsonValueCodec.derived[UbScalaTransient](using UBJsonConfig.default.withScalaTransientSupport)
  val enumIds: UBJsonValueCodec[UbEnums] =
    UBJsonValueCodec.derived[UbEnums](using UBJsonConfig.default.withUseScalaEnumValueId)
  val skipNestedOptions: UBJsonValueCodec[List[UbNestedOptions]] =
    UBJsonValueCodec.derived[List[UbNestedOptions]](using
      UBJsonConfig.default.withSkipNestedOptionValues.withTransientNone
    )
  val nestedOptions3Discriminator: UBJsonValueCodec[UbNestedOptions3] =
    UBJsonValueCodec.derived[UbNestedOptions3](using UBJsonConfig.default.withDiscriminator("type").withTransientNone)
  val nestedOptions3: UBJsonValueCodec[UbNestedOptions3] =
    UBJsonValueCodec.derived[UbNestedOptions3](using UBJsonConfig.default)
  val nestedOptions3CirceLike: UBJsonValueCodec[UbNestedOptions3] =
    UBJsonValueCodec.derived[UbNestedOptions3](using UBJsonConfig.default.withCirceLikeObjectEncoding)
  val vehicles: UBJsonValueCodec[List[UbVehicle]] =
    UBJsonValueCodec.derived[List[UbVehicle]](using UBJsonConfig.default)
  val vehiclesDiscriminator: UBJsonValueCodec[List[UbVehicle]] =
    UBJsonValueCodec.derived[List[UbVehicle]](using UBJsonConfig.default.withDiscriminator("type"))
  val plainAdt: UBJsonValueCodec[List[UbPlainAdt]] =
    UBJsonValueCodec.derived[List[UbPlainAdt]](using UBJsonConfig.default)
  val plainAdtDiscriminator: UBJsonValueCodec[List[UbPlainAdt]] =
    UBJsonValueCodec.derived[List[UbPlainAdt]](using UBJsonConfig.default.withDiscriminator("type"))
  val sealedBase: UBJsonValueCodec[List[UbSealedBase]] =
    UBJsonValueCodec.derived[List[UbSealedBase]](using UBJsonConfig.default.withDiscriminator("type"))
  val multiList: UBJsonValueCodec[UbMultiListDependent] =
    UBJsonValueCodec.derived[UbMultiListDependent](using UBJsonConfig.default.withTransientDefault)
  val multiListAdt: UBJsonValueCodec[List[UbMultiListAdt]] =
    UBJsonValueCodec.derived[List[UbMultiListAdt]](using UBJsonConfig.default.withDiscriminator("type"))
  val transientNull: UBJsonValueCodec[UbNotNullable] =
    UBJsonValueCodec.derived[UbNotNullable](using UBJsonConfig.default.withTransientNull)
}
