package hearth.kindlings.oracle.circe

import io.circe.{Decoder, DecodingFailure, Encoder, Json}

/** A pair of instances of the same type: Kindlings-derived and upstream-derived. */
final case class CiPair[A](
    kindlingsEncoder: Encoder[A],
    kindlingsDecoder: Decoder[A],
    upstreamEncoder: Encoder[A],
    upstreamDecoder: Decoder[A]
)

/** One difference: `operation` (`encode`, `decode`, `decodeAccumulating`) of `subject` (a value or a JSON input). */
final case class CiDiff(
    label: String,
    operation: String,
    subject: String,
    input: Option[Json],
    kindlings: String,
    upstream: String
) {
  override def toString: String = s"$label: $operation $subject: kindlings=$kindlings upstream=$upstream"
}

/** An expected (documented) difference: `reason` says why and where it is documented, `matches` selects its diffs. */
final case class CiExpected(reason: String)(val matches: CiDiff => Boolean)
object CiExpected {

  def onScala2(expected: CiExpected*): List[CiExpected] = if (CiVersionSpecific.isScala3) Nil else expected.toList
  def onScala3(expected: CiExpected*): List[CiExpected] = if (CiVersionSpecific.isScala3) expected.toList else Nil

  // Hearth bug (docs/research/hearth-bug-children-order-from-classpath.md): on Scala 3 Hearth lists the subtypes of a
  // sealed type from the classpath alphabetically, so the strict wrapper failure lists circe's `constructorNames` in
  // another order than circe (declaration order). Remove once Hearth is fixed.
  val hearthChildrenOrder: CiExpected =
    CiExpected("Hearth bug: subtypes of a sealed type from the classpath listed alphabetically on Scala 3") { d =>
      val marker = "expected a single key json object with one of: "
      def names(s: String): Option[List[String]] =
        if (!s.contains(marker)) None
        else Some(s.substring(s.indexOf(marker) + marker.length).takeWhile(_ != '.').split(", ").toList)
      (names(d.kindlings), names(d.upstream)) match {
        case (Some(k), Some(u)) =>
          k != u && k.sorted == u.sorted && d.kindlings.replace(k.mkString(", "), "") ==
            d.upstream.replace(u.mkString(", "), "")
        case _ => false
      }
    }
}

/** Differential checks of circe codecs: encoded JSON (key order included) and decoding (`decodeJson` and
  * `decodeAccumulating`) of valid inputs and of systematically broken ones (each key removed, set to `null`, set to a
  * value of the wrong type, an unknown key added — at every object of the input).
  */
object CirceOracle {

  /** Whether failures are compared by message and cursor history, or only as "failed". Kindlings follows Scala 3 core
    * circe's wording on both Scala versions, so vs Scala 2's shapeless-based derivation (whose wording differs, e.g.
    * `JSON decoding to CNil should never happen`, a missing first field for a non-object input) only the outcome and
    * the decoded values are compared. (Documented: feature-parity.md circe "Error messages and cursor history".)
    */
  val compareFailureMessages: Boolean = CiVersionSpecific.isScala3

  /** Every difference between the two sides. */
  def compare[A](label: String, pair: CiPair[A], values: List[A], extraInputs: List[String] = Nil): List[CiDiff] = {
    val encodingDiffs = values.flatMap { value =>
      val k = encode(pair.kindlingsEncoder, value)
      val u = encode(pair.upstreamEncoder, value)
      if (k == u) Nil else List(CiDiff(label, "encode", value.toString, None, k, u))
    }
    // Inputs: what upstream writes, mutated; plus the hand-written ones.
    val written = values.flatMap(v => scala.util.Try(pair.upstreamEncoder(v)).toOption)
    // (deduplicated by their text: `Json` equality ignores the order of keys, which matters here)
    val inputs =
      (written.flatMap(j => j :: mutations(j)) ++ rootMutations ++ extraInputs.map(parse)).distinctBy(_.noSpaces)
    val decodingDiffs = inputs.flatMap { json =>
      val k = decode(pair.kindlingsDecoder, json)
      val u = decode(pair.upstreamDecoder, json)
      val ka = decodeAccumulating(pair.kindlingsDecoder, json)
      val ua = decodeAccumulating(pair.upstreamDecoder, json)
      (if (k == u) Nil else List(CiDiff(label, "decode", json.noSpaces, Some(json), k, u))) ++
        (if (ka == ua) Nil else List(CiDiff(label, "decodeAccumulating", json.noSpaces, Some(json), ka, ua)))
    }
    encodingDiffs ++ decodingDiffs
  }

  /** The differences no expected difference explains, and the expected differences that explain none (a documented
    * difference that disappeared: update the docs and the test). Empty when the differences are exactly the expected
    * ones.
    */
  def unexplained(differences: List[CiDiff], expected: List[CiExpected]): List[String] =
    differences.filterNot(d => expected.exists(_.matches(d))).map(d => s"unexpected: $d") ++
      expected.filterNot(e => differences.exists(e.matches)).map(e => s"no longer differs: ${e.reason}")

  /** A pair of decoders only (to compare with no values to encode). */
  def decoders[A](kindlings: Decoder[A], upstream: Decoder[A]): CiPair[A] = {
    val none: Encoder[A] = Encoder.instance(_ => Json.Null)
    CiPair(none, kindlings, none, upstream)
  }

  /** Every JSON object in `json` (itself included), depth first. */
  def objects(json: Json): List[io.circe.JsonObject] = json.fold(
    Nil,
    _ => Nil,
    _ => Nil,
    _ => Nil,
    _.toList.flatMap(objects),
    obj => obj :: obj.values.toList.flatMap(objects)
  )

  def parse(s: String): Json =
    io.circe.parser.parse(s).fold(e => throw new IllegalArgumentException(s"$s: $e"), identity)

  private def encode[A](encoder: Encoder[A], value: A): String =
    try encoder(value).noSpaces
    catch { case e: Throwable => s"threw ${e.getClass.getSimpleName}: ${e.getMessage}" }

  def render(df: DecodingFailure): String =
    if (compareFailureMessages) s"${df.message} @ ${df.history.mkString("[", ",", "]")}" else "<failure>"

  private def decode[A](decoder: Decoder[A], json: Json): String =
    try
      decoder.decodeJson(json) match {
        case Right(value) => s"ok $value"
        case Left(df)     => s"fail ${render(df)}"
      }
    catch { case e: Throwable => s"threw ${e.getClass.getSimpleName}: ${e.getMessage}" }

  private def decodeAccumulating[A](decoder: Decoder[A], json: Json): String =
    try
      decoder
        .decodeAccumulating(json.hcursor)
        .fold(
          errors => if (compareFailureMessages) s"fail ${errors.toList.map(render).mkString(" | ")}" else "fail",
          value => s"ok $value"
        )
    catch { case e: Throwable => s"threw ${e.getClass.getSimpleName}: ${e.getMessage}" }

  private val rootMutations: List[Json] =
    List(Json.Null, Json.fromString("wrong"), Json.fromInt(1), Json.arr(), Json.obj())

  /** The input with one change at one place, for every object key and array element (recursively). */
  def mutations(json: Json): List[Json] = json.fold(
    Nil,
    _ => Nil,
    _ => Nil,
    _ => Nil,
    values =>
      values.indices.toList.flatMap { i =>
        (wrongType(values(i)) :: mutations(values(i))).map(changed => Json.fromValues(values.updated(i, changed)))
      },
    obj => {
      val fields = obj.toList
      val perKey = fields.indices.toList.flatMap { i =>
        val (key, value) = fields(i)
        val removed = Json.fromFields(fields.patch(i, Nil, 1))
        val renamed = Json.fromFields(fields.updated(i, (key + "x") -> value)) // also a bad key of a `Map[Int, _]`
        val fractional = if (value.isNumber) List(Json.fromDoubleOrNull(1.5)) else Nil
        val replaced = (Json.Null :: wrongType(value) :: fractional ++ mutations(value)).map { changed =>
          Json.fromFields(fields.updated(i, key -> changed))
        }
        removed :: renamed :: replaced
      }
      Json.fromFields(fields :+ ("unknownKey" -> Json.fromInt(1))) :: perKey
    }
  )

  private def wrongType(json: Json): Json =
    if (json.isString) Json.fromInt(42) else Json.fromString("wrong")
}
