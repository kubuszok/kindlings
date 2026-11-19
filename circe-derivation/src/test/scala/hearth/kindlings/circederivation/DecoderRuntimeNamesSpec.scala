package hearth.kindlings.circederivation

import cats.data.Validated
import hearth.MacroSuite
import io.circe.{Decoder, Json}

final case class DecNamed(firstField: Int, secondField: String)

sealed trait DecNamedSum
object DecNamedSum {
  final case class FirstCase(firstField: Int) extends DecNamedSum
  final case class SecondCase(secondField: String) extends DecNamedSum
  case object ThirdCase extends DecNamedSum
}

sealed trait DecNamedKey
object DecNamedKey {
  case object KeyOne extends DecNamedKey
  case object KeyTwo extends DecNamedKey
}
final case class DecWithEnumKeys(byKey: Map[DecNamedKey, Int])

sealed trait DecOuter
object DecOuter {
  final case class OuterLeaf(outerField: Int) extends DecOuter
  sealed trait DecInner extends DecOuter
  final case class InnerLeaf(innerField: Int) extends DecInner
}

/** Instances derived with a configuration only known at runtime (a parameter the macro cannot evaluate). */
object DecRuntimeConfigInstances {
  def decoder(config: Configuration): KindlingsDecoder[DecNamed] = {
    implicit val runtimeConfig: Configuration = config
    KindlingsDecoder.derived[DecNamed]
  }
  def sumDecoder(config: Configuration): KindlingsDecoder[DecNamedSum] = {
    implicit val runtimeConfig: Configuration = config
    KindlingsDecoder.derived[DecNamedSum]
  }
  def enumDecoder(config: Configuration): KindlingsDecoder[DecNamedKey] = {
    implicit val runtimeConfig: Configuration = config
    KindlingsDecoder.derived[DecNamedKey]
  }
  def enumKeysDecoder(config: Configuration): KindlingsDecoder[DecWithEnumKeys] = {
    implicit val runtimeConfig: Configuration = config
    KindlingsDecoder.derived[DecWithEnumKeys]
  }
  def nestedDecoder(config: Configuration): KindlingsDecoder[DecOuter] = {
    implicit val runtimeConfig: Configuration = config
    // an intermediate sum with an instance of its own: the outer decoder delegates its subtypes' names to it
    implicit val innerDecoder: KindlingsDecoder[DecOuter.DecInner] = KindlingsDecoder.derived[DecOuter.DecInner]
    KindlingsDecoder.derived[DecOuter]
  }
  def codec(config: Configuration): KindlingsCodecAsObject[DecNamed] = {
    implicit val runtimeConfig: Configuration = config
    KindlingsCodecAsObject.derived[DecNamed]
  }
}

/** With a configuration only known at runtime, derived decoders transform the field and constructor names once per
  * instance, not on every decode (or, for the keys of a map, on every key).
  */
final class DecoderRuntimeNamesSpec extends MacroSuite {

  /** A configuration counting the calls of its name transformations. */
  final private class Counting(base: Configuration) {
    var calls: Int = 0
    val config: Configuration = base
      .withTransformMemberNames { name => calls += 1; name.toUpperCase }
      .withTransformConstructorNames { name => calls += 1; name.toLowerCase }
  }

  // Runs `op` once (an instance may compute its names on first use), then twice more, and returns how many name
  // transformations the 2nd and 3rd run made.
  private def transformsOfRepeatedCalls(counting: Counting)(op: () => Any): Int = {
    op()
    val before = counting.calls
    op()
    op()
    counting.calls - before
  }

  private val namedJson = Json.obj("FIRSTFIELD" -> Json.fromInt(1), "SECONDFIELD" -> Json.fromString("s"))

  test("a case class: field names (incl. strict decoding's expected fields)") {
    List(Configuration.default, Configuration.default.withStrictDecoding).foreach { base =>
      val counting = new Counting(base)
      val decoder: Decoder[DecNamed] = DecRuntimeConfigInstances.decoder(counting.config)
      decoder.decodeJson(namedJson) ==> Right(DecNamed(1, "s"))
      transformsOfRepeatedCalls(counting)(() => decoder.decodeJson(namedJson)) ==> 0
      transformsOfRepeatedCalls(counting)(() => decoder.decodeAccumulating(namedJson.hcursor)) ==> 0
      decoder.decodeAccumulating(namedJson.hcursor) ==> Validated.valid(DecNamed(1, "s"))
    }
  }

  test("a case class decoded by a codec") {
    val counting = new Counting(Configuration.default)
    val codec = DecRuntimeConfigInstances.codec(counting.config)
    codec.decodeJson(namedJson) ==> Right(DecNamed(1, "s"))
    transformsOfRepeatedCalls(counting)(() => codec.decodeJson(namedJson)) ==> 0
    transformsOfRepeatedCalls(counting)(() => codec(DecNamed(1, "s"))) ==> 0
  }

  test("a sealed trait: constructor names, wrapped and with a discriminator") {
    val wrapped = Json.obj("secondcase" -> Json.obj("SECONDFIELD" -> Json.fromString("s")))
    val withDiscriminator = Json.obj("SECONDFIELD" -> Json.fromString("s"), "type" -> Json.fromString("secondcase"))
    List(
      Configuration.default -> wrapped,
      Configuration.default.withStrictDecoding -> wrapped,
      Configuration.default.withDiscriminator("type") -> withDiscriminator
    ).foreach { case (base, json) =>
      val counting = new Counting(base)
      val decoder: Decoder[DecNamedSum] = DecRuntimeConfigInstances.sumDecoder(counting.config)
      decoder.decodeJson(json) ==> Right(DecNamedSum.SecondCase("s"))
      transformsOfRepeatedCalls(counting)(() => decoder.decodeJson(json)) ==> 0
      transformsOfRepeatedCalls(counting)(() => decoder.decodeAccumulating(json.hcursor)) ==> 0
    }
  }

  test("a sealed trait delegating to the instance of an intermediate sealed trait") {
    List(Configuration.default, Configuration.default.withDiscriminator("type")).foreach { base =>
      val counting = new Counting(base)
      val decoder: Decoder[DecOuter] = DecRuntimeConfigInstances.nestedDecoder(counting.config)
      val json =
        if (base.discriminator.isDefined)
          Json.obj("INNERFIELD" -> Json.fromInt(1), "type" -> Json.fromString("innerleaf"))
        else Json.obj("innerleaf" -> Json.obj("INNERFIELD" -> Json.fromInt(1)))
      decoder.decodeJson(json) ==> Right(DecOuter.InnerLeaf(1))
      transformsOfRepeatedCalls(counting)(() => decoder.decodeJson(json)) ==> 0
    }
  }

  test("an enum decoded from strings") {
    val counting = new Counting(Configuration.default.withEnumAsStrings)
    val decoder: Decoder[DecNamedKey] = DecRuntimeConfigInstances.enumDecoder(counting.config)
    val json = Json.fromString("keytwo")
    decoder.decodeJson(json) ==> Right(DecNamedKey.KeyTwo)
    transformsOfRepeatedCalls(counting)(() => decoder.decodeJson(json)) ==> 0
  }

  test("enum map keys: no name transformation per key") {
    val counting = new Counting(Configuration.default)
    val decoder: Decoder[DecWithEnumKeys] = DecRuntimeConfigInstances.enumKeysDecoder(counting.config)
    val json = Json.obj("BYKEY" -> Json.obj("keyone" -> Json.fromInt(1), "keytwo" -> Json.fromInt(2)))
    decoder.decodeJson(json) ==> Right(DecWithEnumKeys(Map(DecNamedKey.KeyOne -> 1, DecNamedKey.KeyTwo -> 2)))
    transformsOfRepeatedCalls(counting)(() => decoder.decodeJson(json)) ==> 0
    // an unknown key still fails
    assert(decoder.decodeJson(Json.obj("BYKEY" -> Json.obj("KeyOne" -> Json.fromInt(1)))).isLeft)
  }
}
