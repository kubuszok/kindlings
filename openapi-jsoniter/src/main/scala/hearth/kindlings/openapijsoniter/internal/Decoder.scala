package hearth.kindlings.openapijsoniter
package internal

import hearth.kindlings.jsoniterjson.Json

import scala.collection.immutable.ListMap

/** Decoding failure carrying a human-readable message. */
final private[openapijsoniter] case class DecodingFailure(message: String)

/** Minimal circe-style `Decoder` for the kindlings [[Json]] AST.
  *
  * A `Decoder[A]` is `Json => Either[DecodingFailure, A]`. It exposes the subset of circe's combinators that the
  * sttp-apispec codecs use: `map`, `emap`, `flatMap`, `or`, `widen`, plus object-cursor helpers via [[Cursor]].
  */
private[openapijsoniter] trait Decoder[A] { self =>
  def apply(json: Json): Either[DecodingFailure, A]

  final def map[B](f: A => B): Decoder[B] = (json: Json) => self.apply(json).map(f)

  final def emap[B](f: A => Either[String, B]): Decoder[B] = (json: Json) =>
    self.apply(json).flatMap(a => f(a).left.map(DecodingFailure(_)))

  final def flatMap[B](f: A => Decoder[B]): Decoder[B] = (json: Json) => self.apply(json).flatMap(a => f(a).apply(json))

  /** Tries `self`; on failure falls back to `other`. Mirrors circe's `Decoder#or`. */
  final def or[AA >: A](other: => Decoder[AA]): Decoder[AA] = (json: Json) =>
    self.apply(json) match {
      case r @ Right(_) => r.asInstanceOf[Either[DecodingFailure, AA]]
      case Left(_)      => other.apply(json)
    }

  final def widen[B >: A]: Decoder[B] = self.asInstanceOf[Decoder[B]]
}

private[openapijsoniter] object Decoder {
  def apply[A](implicit d: Decoder[A]): Decoder[A] = d
  def instance[A](f: Json => Either[DecodingFailure, A]): Decoder[A] = (json: Json) => f(json)

  /** Decoder that operates on an object-cursor (used heavily by the apispec decoders). */
  def fromCursor[A](f: Cursor => Either[DecodingFailure, A]): Decoder[A] =
    (json: Json) => f(new Cursor(json))

  private def fail[A](msg: String): Either[DecodingFailure, A] = Left(DecodingFailure(msg))

  implicit val decodeJson: Decoder[Json] = instance(Right(_))

  implicit val decodeString: Decoder[String] = instance {
    case Json.Str(s) => Right(s)
    case other       => fail(s"expected string, got $other")
  }
  implicit val decodeBoolean: Decoder[Boolean] = instance {
    case Json.Bool(b) => Right(b)
    case other        => fail(s"expected boolean, got $other")
  }
  implicit val decodeInt: Decoder[Int] = instance {
    case Json.Num(n) => n.toInt.toRight(DecodingFailure(s"expected Int, got $n"))
    case other       => fail(s"expected number, got $other")
  }
  implicit val decodeLong: Decoder[Long] = instance {
    case Json.Num(n) => n.toLong.toRight(DecodingFailure(s"expected Long, got $n"))
    case other       => fail(s"expected number, got $other")
  }
  implicit val decodeBigDecimal: Decoder[BigDecimal] = instance {
    case Json.Num(n) => n.toBigDecimal.toRight(DecodingFailure(s"expected number, got $n"))
    case other       => fail(s"expected number, got $other")
  }

  implicit def decodeOption[A](implicit d: Decoder[A]): Decoder[Option[A]] = instance {
    case Json.Null => Right(None)
    case other     => d(other).map(Some(_))
  }

  implicit def decodeList[A](implicit d: Decoder[A]): Decoder[List[A]] = instance {
    case Json.Arr(vs) =>
      vs.foldRight[Either[DecodingFailure, List[A]]](Right(Nil)) { (j, acc) =>
        for { tail <- acc; head <- d(j) } yield head :: tail
      }
    case other => fail(s"expected array, got $other")
  }

  implicit def decodeVector[A](implicit d: Decoder[A]): Decoder[Vector[A]] =
    decodeList[A].map(_.toVector)

  /** Generic implicit `ListMap` decoder (preserving insertion order) for any key/value with the right type classes. */
  implicit def decodeListMapImplicit[K, V](implicit k: KeyDecoder[K], v: Decoder[V]): Decoder[ListMap[K, V]] =
    decodeListMap[K, V]

  /** circe's `decodeMapLike[K, V, ListMap]` preserving insertion order. */
  def decodeListMap[K, V](implicit k: KeyDecoder[K], v: Decoder[V]): Decoder[ListMap[K, V]] = instance {
    case Json.Obj(obj) =>
      obj.fields.foldLeft[Either[DecodingFailure, ListMap[K, V]]](Right(ListMap.empty)) { case (acc, (key, value)) =>
        for {
          m <- acc
          dk <- k(key).toRight(DecodingFailure(s"could not decode key '$key'"))
          dv <- v(value)
        } yield m.updated(dk, dv)
      }
    case other => fail(s"expected object, got $other")
  }
}

/** circe-style `KeyDecoder` (object keys). */
private[openapijsoniter] trait KeyDecoder[A] { self =>
  def apply(key: String): Option[A]
  final def map[B](f: A => B): KeyDecoder[B] = (key: String) => self.apply(key).map(f)
}

private[openapijsoniter] object KeyDecoder {
  def apply[A](implicit d: KeyDecoder[A]): KeyDecoder[A] = d
  def instance[A](f: String => Option[A]): KeyDecoder[A] = (key: String) => f(key)
  implicit val decodeKeyString: KeyDecoder[String] = Some(_)
}

/** A focused JSON cursor mirroring the bits of circe's `HCursor` the apispec decoders use: `get`, `getOrElse`, `as`,
  * `focus`, `withFocus`/`mapObject`.
  */
final private[openapijsoniter] class Cursor(val focus: Json) {

  def as[A](implicit d: Decoder[A]): Either[DecodingFailure, A] = d(focus)

  private def field(name: String): Option[Json] = focus match {
    case Json.Obj(obj) => obj(name)
    case _             => None
  }

  /** `c.get[A](name)` — fails if the field is missing (unless `A` is `Option`, handled by callers via `Option`). */
  def get[A](name: String)(implicit d: Decoder[A]): Either[DecodingFailure, A] =
    field(name) match {
      case Some(j) => d(j)
      case None    => d(Json.Null) // matches circe: missing key decodes like `null` for Option, fails otherwise
    }

  def getOrElse[A](name: String)(default: => A)(implicit d: Decoder[A]): Either[DecodingFailure, A] =
    field(name) match {
      case Some(j) => d(j)
      case None    => Right(default)
    }

  def withFocus(f: Json => Json): Cursor = new Cursor(f(focus))
}
