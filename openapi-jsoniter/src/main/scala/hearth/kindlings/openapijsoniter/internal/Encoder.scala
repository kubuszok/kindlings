package hearth.kindlings.openapijsoniter
package internal

import hearth.kindlings.jsoniterjson.{Json, JsonNumber}

/** Minimal circe-style `Encoder` for the kindlings [[Json]] AST.
  *
  * Mirrors the subset of circe's `Encoder` / `KeyEncoder` API that the sttp-apispec codecs rely on. Encoders are
  * plain `A => Json` functions; the `:=` syntax builds ordered `(String, Json)` object entries exactly like circe.
  */
private[openapijsoniter] trait Encoder[A] { self =>
  def apply(a: A): Json
  final def contramap[B](f: B => A): Encoder[B] = (b: B) => self.apply(f(b))
}

private[openapijsoniter] object Encoder {
  def apply[A](implicit e: Encoder[A]): Encoder[A] = e
  def instance[A](f: A => Json): Encoder[A] = (a: A) => f(a)

  implicit val encodeString: Encoder[String] = Json.fromString(_)
  implicit val encodeBoolean: Encoder[Boolean] = Json.fromBoolean(_)
  implicit val encodeInt: Encoder[Int] = Json.fromInt(_)
  implicit val encodeLong: Encoder[Long] = Json.fromLong(_)
  implicit val encodeBigDecimal: Encoder[BigDecimal] = Json.fromBigDecimal(_)
  implicit val encodeBigInt: Encoder[BigInt] = Json.fromBigInt(_)
  implicit val encodeJson: Encoder[Json] = identity(_)

  implicit def encodeOption[A](implicit e: Encoder[A]): Encoder[Option[A]] = {
    case Some(a) => e(a)
    case None    => Json.Null
  }

  /** Empty `List`s encode as `null` (then dropped by `dropNulls`), matching circe's `encodeList` in the apispec
    * codecs.
    */
  implicit def encodeList[A](implicit e: Encoder[A]): Encoder[List[A]] = {
    case Nil => Json.Null
    case l   => Json.fromValues(l.map(e.apply))
  }

  implicit def encodeVector[A](implicit e: Encoder[A]): Encoder[Vector[A]] =
    (v: Vector[A]) => Json.fromValues(v.map(e.apply))
}

/** circe-style `KeyEncoder` (object keys). */
private[openapijsoniter] trait KeyEncoder[A] { self =>
  def apply(a: A): String
  final def contramap[B](f: B => A): KeyEncoder[B] = (b: B) => self.apply(f(b))
}

private[openapijsoniter] object KeyEncoder {
  def apply[A](implicit e: KeyEncoder[A]): KeyEncoder[A] = e
  implicit val encodeKeyString: KeyEncoder[String] = identity(_)
}

/** Number helpers mirroring circe's `Json.fromFloatOrString` / `fromDoubleOrString` over the kindlings AST. */
private[openapijsoniter] object JsonNumbers {
  def fromFloatOrString(value: Float): Json =
    JsonNumber.fromFloat(value).map(Json.Num(_)).getOrElse(Json.fromString(value.toString))
  def fromDoubleOrString(value: Double): Json =
    JsonNumber.fromDouble(value).map(Json.Num(_)).getOrElse(Json.fromString(value.toString))
}
