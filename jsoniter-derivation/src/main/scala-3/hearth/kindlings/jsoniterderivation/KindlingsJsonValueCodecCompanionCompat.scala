package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec

private[jsoniterderivation] trait KindlingsJsonValueCodecCompanionCompat {
  this: KindlingsJsonValueCodec.type =>

  inline def derive[A](using config: JsoniterConfig): JsonValueCodec[A] = ${
    internal.compiletime.CodecMacros.deriveCodecImpl[A]('config)
  }

  inline given derived[A](using config: JsoniterConfig): KindlingsJsonValueCodec[A] = ${
    internal.compiletime.CodecMacros.deriveKindlingsCodecImpl[A]('config)
  }
}
