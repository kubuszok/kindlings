package hearth.kindlings.yamlderivation

import hearth.MacroSuite
import org.virtuslab.yaml.YamlDecoder

/** Decoders for a 100-field case class with defaults, derived in a class of their own so their bytecode can be
  * measured.
  */
object YamlWideInstances {
  // the default configuration (evaluated at compile time): defaults are used
  val decoder: KindlingsYamlDecoder[WideDefaults] = KindlingsYamlDecoder.derived[WideDefaults]

  // `config` is a parameter: the macro cannot evaluate it and reads it (incl. `useDefaults`) at runtime
  def decoderWith(config: YamlConfig): KindlingsYamlDecoder[WideDefaults] = {
    implicit val runtimeConfig: YamlConfig = config
    KindlingsYamlDecoder.derived[WideDefaults]
  }

  val encoder: KindlingsYamlEncoder[WideDefaults] = KindlingsYamlEncoder.derived[WideDefaults]

  def encoderWith(config: YamlConfig): KindlingsYamlEncoder[WideDefaults] = {
    implicit val runtimeConfig: YamlConfig = config
    KindlingsYamlEncoder.derived[WideDefaults]
  }
}

/** The same with a configuration only known at runtime: the subtype names are computed once per instance. */
object YamlWideSumRuntimeInstances {
  def encoder(config: YamlConfig): KindlingsYamlEncoder[WideSum] = {
    implicit val runtimeConfig: YamlConfig = config
    KindlingsYamlEncoder.derived[WideSum]
  }
  def decoder(config: YamlConfig): KindlingsYamlDecoder[WideSum] = {
    implicit val runtimeConfig: YamlConfig = config
    KindlingsYamlDecoder.derived[WideSum]
  }
}

/** Guards against generated methods too large to be JIT-compiled (see `docs/research/generated-code-audit-2026-11.md`).
  * JVM only: it reads the class files.
  */
final class YamlBytecodeSizeSpec extends MacroSuite {

  test("no method of the decoders derived for a 100-field case class exceeds HotSpot's JIT limit") {
    val sizes = BytecodeSizes.methodSizes(YamlWideInstances.getClass)
    assert(sizes.map(_._3).sum > 5000, s"expected the derived decoders' code, got: $sizes")
    val huge = sizes.filter(_._3 > BytecodeSizes.HugeMethodLimit)
    assert(huge.isEmpty, s"methods larger than ${BytecodeSizes.HugeMethodLimit} B: ${huge.mkString(", ")}")
  }

  test("no method of the instances derived for a sealed trait with 50 subtypes exceeds HotSpot's JIT limit") {
    val sizes = BytecodeSizes.methodSizes(YamlWideSumStaticInstances.getClass) ++
      BytecodeSizes.methodSizes(YamlWideSumRuntimeInstances.getClass)
    assert(sizes.map(_._3).sum > 5000, s"expected the derived instances' code, got: $sizes")
    val huge = sizes.filter(_._3 > BytecodeSizes.HugeMethodLimit)
    assert(huge.isEmpty, s"methods larger than ${BytecodeSizes.HugeMethodLimit} B: ${huge.mkString(", ")}")
  }

  test("no method of the map-key codecs derived for an enum of 100 case objects exceeds HotSpot's JIT limit") {
    val sizes = BytecodeSizes.methodSizes(YamlWideKeyInstances.getClass)
    assert(sizes.map(_._3).sum > 3000, s"expected the derived instances' code, got: $sizes")
    val huge = sizes.filter(_._3 > BytecodeSizes.HugeMethodLimit)
    assert(huge.isEmpty, s"methods larger than ${BytecodeSizes.HugeMethodLimit} B: ${huge.mkString(", ")}")
  }

  test("enum map keys round-trip, with compile-time and runtime configurations") {
    val value = WideKeyMap(Map(WideKey.K00 -> 0, WideKey.K57 -> 57, WideKey.K99 -> 99))
    val node = YamlWideKeyInstances.encoder.asNode(value)
    YamlWideKeyInstances.encoderWith(YamlConfig()).asNode(value) ==> node
    YamlWideKeyInstances.decoder.construct(node) ==> Right(value)
    YamlWideKeyInstances.decoderWith(YamlConfig()).construct(node) ==> Right(value)
    val kebab = YamlConfig().withKebabCaseConstructorNames
    YamlWideKeyInstances.decoderWith(kebab).construct(YamlWideKeyInstances.encoderWith(kebab).asNode(value)) ==>
      Right(value)
  }

  test("a configuration known at compile time leaves no runtime branches on the configuration in sum dispatch") {
    // positive control: with a runtime configuration the dispatch reads it
    assert(BytecodeSizes.refersTo(YamlWideSumRuntimeInstances.getClass, "transformConstructorNames"))
    assert(BytecodeSizes.refersTo(YamlWideSumRuntimeInstances.getClass, "wrappedSubtypes"))
    List("transformConstructorNames", "wrappedSubtypes", "enumAsStrings", "discriminator").foreach { member =>
      assert(!BytecodeSizes.refersTo(YamlWideSumStaticInstances.getClass, member), member)
    }
  }

  test("sum instances work, with compile-time and runtime configurations") {
    val values: List[WideSum] = List(WideSum.Case00(0), WideSum.Case47(47), WideSum.Object48, WideSum.Object49)
    import YamlWideSumStaticInstances.*
    List(
      (Discriminator.encoder, Discriminator.decoder, YamlConfig(discriminator = Some("type"))),
      (Wrapped.encoder, Wrapped.decoder, YamlConfig(wrappedSubtypes = true))
    ).foreach { case (encoder, decoder, config) =>
      val runtimeEncoder = YamlWideSumRuntimeInstances.encoder(config)
      val runtimeDecoder = YamlWideSumRuntimeInstances.decoder(config)
      values.foreach { value =>
        val node = encoder.asNode(value)
        runtimeEncoder.asNode(value) ==> node
        decoder.construct(node) ==> Right(value)
        runtimeDecoder.construct(node) ==> Right(value)
      }
    }
    val untaggedNode = Untagged.encoder.asNode(WideSum.Case47(47))
    YamlWideSumRuntimeInstances.encoder(YamlConfig()).asNode(WideSum.Case47(47)) ==> untaggedNode
    // untagged: the first subtype that decodes wins (as in scala-yaml)
    Untagged.decoder
      .construct(untaggedNode) ==> YamlWideSumRuntimeInstances.decoder(YamlConfig()).construct(untaggedNode)
  }

  test("the encoders work") {
    val value = WideDefaults(f0 = Some("x"), f1 = 1, f99 = 5L)
    val node = YamlWideInstances.encoder.asNode(value)
    YamlWideInstances.encoderWith(YamlConfig()).asNode(value) ==> node
    YamlWideInstances.decoder.construct(node) ==> Right(value)
  }

  test("the decoders work") {
    val value = WideDefaults(f0 = Some("x"), f1 = 1, f99 = 5L)
    val node = KindlingsYamlEncoder.encode[WideDefaults](value)
    val runtime: YamlDecoder[WideDefaults] = YamlWideInstances.decoderWith(YamlConfig())
    YamlWideInstances.decoder.construct(node) ==> Right(value)
    runtime.construct(node) ==> Right(value)
    val empty = KindlingsYamlEncoder.encode[Map[String, Int]](Map.empty)
    YamlWideInstances.decoder.construct(empty) ==> Right(WideDefaults())
    runtime.construct(empty) ==> Right(WideDefaults())
    // the first failing field is reported
    val broken = KindlingsYamlEncoder.encode[Map[String, String]](Map("f1" -> "x", "f3" -> "y"))
    val failure = YamlWideInstances.decoder.construct(broken)
    val runtimeFailure = runtime.construct(broken)
    assert(failure.isLeft, failure.toString)
    runtimeFailure ==> failure
  }
}
