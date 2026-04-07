package hearth.kindlings.pureconfigderivation

import pureconfig.ConfigConvert

private[pureconfigderivation] trait KindlingsConfigConvertCompanionCompat { this: KindlingsConfigConvert.type =>

  // On Scala 3 we cannot have a single macro that combines Reader and Writer derivation
  // because the two derivations each have their own outer Quotes, and weaving the
  // resulting Exprs into a wrapping Expr.quote { … } trips Scala 3's sibling-splice
  // isolation. Instead, the inline composition runs each derivation as its own top-level
  // macro expansion, then combines the results at runtime via a small helper.
  inline def derive[A](using config: PureConfig): ConfigConvert[A] =
    internal.runtime.PureConfigDerivationUtils.configConvert[A](
      KindlingsConfigReader.derive[A],
      KindlingsConfigWriter.derive[A]
    )

  inline given derived[A](using config: PureConfig): KindlingsConfigConvert[A] =
    internal.runtime.PureConfigDerivationUtils.configConvert[A](
      KindlingsConfigReader.derive[A],
      KindlingsConfigWriter.derive[A]
    )
}
