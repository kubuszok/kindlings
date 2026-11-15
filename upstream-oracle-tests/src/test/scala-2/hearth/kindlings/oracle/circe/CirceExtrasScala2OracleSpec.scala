package hearth.kindlings.oracle.circe

import hearth.MacroSuite
import hearth.kindlings.circederivation.{Configuration, ExtrasDecoder, KindlingsCodecAsObject, KindlingsDecoder}
import hearth.kindlings.circederivation.annotations.fieldName
import io.circe.Decoder
import io.circe.generic.extras.{Configuration as ExtrasConfiguration, JsonKey}

/** Upstream's renamed field (`@JsonKey`) and Kindlings' (`@fieldName`) on the same field. */
final case class CiKeyed(@JsonKey("full name") @fieldName("full name") fullName: String, otherField: Int)

/** Kindlings vs Scala 2-only circe-generic / generic-extras features: `@JsonKey` (Kindlings: `@fieldName`),
  * `ExtrasDecoder.decodeStrict`, `deriveFor[A].patch` and `deriveFor[F].incomplete`.
  */
final class CirceExtrasScala2OracleSpec extends MacroSuite {

  private def check(differences: List[CiDiff]): Unit =
    assert(differences.isEmpty, s"Kindlings vs upstream circe differences:\n${differences.mkString("\n")}")

  test("@JsonKey vs @fieldName (the annotation wins over transformMemberNames)") {
    import io.circe.generic.extras.semiauto.deriveConfiguredCodec
    val values = List(CiKeyed("Ann", 1))
    val plain = {
      implicit val k: Configuration = Configuration.default
      implicit val u: ExtrasConfiguration = ExtrasConfiguration.default
      CirceOracle.compare(
        "keyed",
        CiSamples.pair(KindlingsCodecAsObject.derived[CiKeyed], deriveConfiguredCodec[CiKeyed]),
        values
      )
    }
    val snakeStrict = {
      implicit val k: Configuration = Configuration.default.withSnakeCaseMemberNames.withStrictDecoding
      implicit val u: ExtrasConfiguration = ExtrasConfiguration.default.withSnakeCaseMemberNames.withStrictDecoding
      CirceOracle.compare(
        "keyed snake strict",
        CiSamples.pair(KindlingsCodecAsObject.derived[CiKeyed], deriveConfiguredCodec[CiKeyed]),
        values,
        List("""{"full name":"a","other_field":1,"fullName":"b"}""")
      )
    }
    check(plain ++ snakeStrict)
  }

  test("ExtrasDecoder: isStrict and decodeStrict (the extraneous fields)") {
    import io.circe.generic.extras.semiauto.deriveExtrasCodec
    val inputs = List(
      """{"firstName":"a","age":1}""",
      """{"firstName":"a","age":1,"x":1,"y":2}""",
      """{"firstName":"a","age":"wrong","x":1}""",
      """{"age":1,"zzz":null}""",
      """[]"""
    ).map(CirceOracle.parse)
    def results(strict: Boolean): List[String] = {
      implicit val k: Configuration = Configuration(strictDecoding = strict)
      implicit val u: ExtrasConfiguration = ExtrasConfiguration.default.copy(strictDecoding = strict)
      val kd: ExtrasDecoder[CiPerson] = KindlingsCodecAsObject.derived[CiPerson]
      val ud = deriveExtrasCodec[CiPerson]
      def show(r: Either[(io.circe.DecodingFailure, List[String]), CiPerson]): String =
        r.fold({ case (df, extra) => s"fail ${CirceOracle.render(df)} extraneous=$extra" }, v => s"ok $v")
      s"isStrict: kindlings=${kd.isStrict} upstream=${ud.isStrict}" :: inputs.map { j =>
        s"${j.noSpaces}: kindlings=${show(kd.decodeStrict(j.hcursor))} upstream=${show(ud.decodeStrict(j.hcursor))}"
      }
    }
    val differences = (results(strict = false) ++ results(strict = true)).filter { line =>
      val i = line.indexOf("kindlings=")
      val j = line.indexOf(" upstream=")
      line.substring(i + "kindlings=".length, j) != line.substring(j + " upstream=".length)
    }
    // (failure messages are not compared on Scala 2, see `CirceOracle.compareFailureMessages`)
    assertEquals(differences, Nil)
  }

  test("patch decoders: KindlingsDecoder.patch vs deriveFor[A].patch") {
    val base = CiPerson("Ann", 30, Some("An"))
    val inputs = List(
      """{}""",
      """{"firstName":"Bob"}""",
      """{"age":null,"nickName":null}""",
      """{"nickName":"B","unknown":1}""",
      """{"age":"wrong"}""",
      """[]""",
      """null"""
    )
    val generic = {
      implicit val k: Configuration = Configuration.default
      val u = io.circe.generic.semiauto.deriveFor[CiPerson].patch
      val kd = KindlingsDecoder.patch[CiPerson]
      CirceOracle.compare("patch", CirceOracle.decoders(kd.map(_(base)), u.map(_(base))), Nil, inputs)
    }
    def snake(defaults: Boolean): List[CiDiff] = {
      implicit val k: Configuration = Configuration(useDefaults = defaults).withSnakeCaseMemberNames
      implicit val u: ExtrasConfiguration =
        ExtrasConfiguration.default.copy(useDefaults = defaults).withSnakeCaseMemberNames
      val ud = io.circe.generic.extras.semiauto.deriveConfiguredFor[CiDefaults].patch
      val kd = KindlingsDecoder.patch[CiDefaults]
      val baseD = CiDefaults("a", 1, Nil, None)
      CirceOracle.compare(
        s"patch snake defaults=$defaults",
        CirceOracle.decoders(kd.map(_(baseD)), ud.map(_(baseD))),
        Nil,
        List("""{}""", """{"count":null}""", """{"name":"b","tags":["t"]}""", """{"opt":5}""", """{"count":"x"}""")
      )
    }
    check(generic ++ snake(defaults = false))
    // generic-extras' patch decoder with `useDefaults` throws a `ClassCastException` (an upstream bug: it puts the
    // default values, not `Option`s, in the patch record) for every valid patch; Kindlings' patch decoders ignore
    // `useDefaults` (documented: feature-parity.md circe "Patch decoders", circe guide "Patch decoders").
    val withDefaults = snake(defaults = true)
    assert(
      withDefaults.nonEmpty && withDefaults.forall { d =>
        d.kindlings.startsWith("ok") && d.upstream.startsWith("threw ClassCastException")
      },
      withDefaults.mkString("\n")
    )
  }

  test("incomplete decoders: KindlingsDecoder.incomplete vs deriveFor[F].incomplete") {
    val inputs = List(
      """{"firstName":"Bob","nickName":"B"}""",
      """{"firstName":"Bob"}""",
      """{"firstName":"Bob","age":5}""",
      """{"nickName":"B"}""",
      """{"firstName":1}""",
      """null"""
    )
    val generic = {
      implicit val k: Configuration = Configuration.default
      val u: Decoder[Int => CiPerson] = io.circe.generic.semiauto.deriveFor[Int => CiPerson].incomplete
      val kd: Decoder[Int => CiPerson] = KindlingsDecoder.incomplete[Int => CiPerson]
      CirceOracle.compare("incomplete", CirceOracle.decoders(kd.map(_(7)), u.map(_(7))), Nil, inputs)
    }
    val extras = {
      implicit val k: Configuration = Configuration.default.withSnakeCaseMemberNames.withDefaults
      implicit val u: ExtrasConfiguration = ExtrasConfiguration.default.withSnakeCaseMemberNames.withDefaults
      val ud: Decoder[String => CiDefaults] =
        io.circe.generic.extras.semiauto.deriveConfiguredFor[String => CiDefaults].incomplete
      val kd: Decoder[String => CiDefaults] = KindlingsDecoder.incomplete[String => CiDefaults]
      CirceOracle.compare(
        "incomplete snake defaults",
        CirceOracle.decoders(kd.map(_("n")), ud.map(_("n"))),
        Nil,
        List("""{}""", """{"count":2,"tags":[],"opt":null}""", """{"count":"x"}""", """{"name":"ignored"}""")
      )
    }
    check(generic ++ extras)
  }
}
