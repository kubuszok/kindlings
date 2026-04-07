package hearth.kindlings.sconfigderivation

private[sconfigderivation] trait ConfigCodecCompanionCompat { this: ConfigCodec.type =>

  // On Scala 3 we cannot have a single macro that combines Reader and Writer derivation
  // because the two derivations each have their own outer Quotes, and weaving the
  // resulting Exprs into a wrapping Expr.quote { … } trips Scala 3's sibling-splice
  // isolation. Instead, the inline composition runs each derivation as its own top-level
  // macro expansion, then combines the results at runtime via a small helper.
  inline def derive[A](using config: SConfig): ConfigCodec[A] =
    internal.runtime.SConfigDerivationUtils.configCodec[A](
      ConfigReader.derive[A],
      ConfigWriter.derive[A]
    )

  inline given derived[A](using config: SConfig): ConfigCodec[A] =
    internal.runtime.SConfigDerivationUtils.configCodec[A](
      ConfigReader.derive[A],
      ConfigWriter.derive[A]
    )
}
