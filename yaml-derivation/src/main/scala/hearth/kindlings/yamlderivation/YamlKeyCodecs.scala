package hearth.kindlings.yamlderivation

import org.virtuslab.yaml.{ConstructError, LoadSettings, Node, YamlDecoder}

/** How a map key is written: as the scalar string this returns (the key of the YAML mapping).
  *
  * Maps derived by Kindlings write their keys with the `YamlKeyEncoder[K]` in scope, or with a built-in / derived one:
  * `String`, `Char`, `Boolean`, the numeric primitives, `BigInt`, `BigDecimal`, `UUID`, enums of case objects (Scala 3
  * enums, Java enums, `scala.Enumeration`, sealed traits of case objects: by their configured names) and value types
  * (`AnyVal`, opaque types, refined / Iron / neotype wrappers) over a type with a key encoder. Unlike scala-yaml, whose
  * `YamlEncoder.forMap` writes keys with the ordinary `YamlEncoder[K]`, a `YamlEncoder[K]` is used for keys only when
  * `K` has no key encoder (a complex, non-scalar key).
  */
trait YamlKeyEncoder[K] { self =>

  def encodeKey(key: K): String

  final def contramap[B](f: B => K): YamlKeyEncoder[B] = YamlKeyEncoder.instance(b => self.encodeKey(f(b)))
}
object YamlKeyEncoder {

  def apply[K](implicit encoder: YamlKeyEncoder[K]): YamlKeyEncoder[K] = encoder

  def instance[K](f: K => String): YamlKeyEncoder[K] = new YamlKeyEncoder[K] {
    override def encodeKey(key: K): String = f(key)
  }

  // The built-in key encoders: the macro writes these key types directly; the instances are there to build on.
  implicit val forString: YamlKeyEncoder[String] = instance(identity)
  implicit val forChar: YamlKeyEncoder[Char] = instance(_.toString)
  implicit val forBoolean: YamlKeyEncoder[Boolean] = instance(_.toString)
  implicit val forByte: YamlKeyEncoder[Byte] = instance(_.toString)
  implicit val forShort: YamlKeyEncoder[Short] = instance(_.toString)
  implicit val forInt: YamlKeyEncoder[Int] = instance(_.toString)
  implicit val forLong: YamlKeyEncoder[Long] = instance(_.toString)
  implicit val forFloat: YamlKeyEncoder[Float] = instance(_.toString)
  implicit val forDouble: YamlKeyEncoder[Double] = instance(_.toString)
  implicit val forBigInt: YamlKeyEncoder[BigInt] = instance(_.toString)
  implicit val forBigDecimal: YamlKeyEncoder[BigDecimal] = instance(_.toString)
  implicit val forUUID: YamlKeyEncoder[java.util.UUID] = instance(_.toString)
}

/** How a map key is read from the scalar string of the YAML mapping's key. A failure without a position is reported at
  * the key's node.
  *
  * Maps derived by Kindlings read their keys with the `YamlKeyDecoder[K]` in scope, or with a built-in / derived one
  * (the types listed at [[YamlKeyEncoder]]; a value type's smart constructor validates the key, and a rejected key is a
  * decoding failure). Unlike scala-yaml, whose `YamlDecoder.forMap` reads keys with the ordinary `YamlDecoder[K]`, a
  * `YamlDecoder[K]` is used for keys only when `K` has no key decoder (a complex, non-scalar key).
  */
trait YamlKeyDecoder[K] { self =>

  def decodeKey(key: String): Either[ConstructError, K]

  final def map[B](f: K => B): YamlKeyDecoder[B] = YamlKeyDecoder.instance(s => self.decodeKey(s).map(f))

  /** A key decoder that can reject the decoded value (the `Left` message becomes the failure). */
  final def emap[B](f: K => Either[String, B]): YamlKeyDecoder[B] =
    YamlKeyDecoder.instance(s => self.decodeKey(s).flatMap(k => f(k).left.map(ConstructError.from(_))))
}
object YamlKeyDecoder {

  def apply[K](implicit decoder: YamlKeyDecoder[K]): YamlKeyDecoder[K] = decoder

  def instance[K](f: String => Either[ConstructError, K]): YamlKeyDecoder[K] = new YamlKeyDecoder[K] {
    override def decodeKey(key: String): Either[ConstructError, K] = f(key)
  }

  /** A key decoder reading the key as scala-yaml's `decoder` reads a scalar with this value. */
  def fromScalarDecoder[K](decoder: YamlDecoder[K]): YamlKeyDecoder[K] =
    instance(s => decoder.construct(Node.ScalarNode(s))(LoadSettings.empty))

  // The built-in key decoders: the macro reads these key types directly (as scala-yaml's decoders read them); the
  // instances are there to build on.
  implicit val forString: YamlKeyDecoder[String] = instance(Right(_))
  implicit val forChar: YamlKeyDecoder[Char] = instance { s =>
    if (s.length == 1) Right(s.charAt(0)) else Left(ConstructError.from(s"Cannot parse $s as Char", "Char"))
  }
  implicit val forBoolean: YamlKeyDecoder[Boolean] = fromScalarDecoder(YamlDecoder.forBoolean)
  implicit val forByte: YamlKeyDecoder[Byte] = fromScalarDecoder(YamlDecoder.forByte)
  implicit val forShort: YamlKeyDecoder[Short] = fromScalarDecoder(YamlDecoder.forShort)
  implicit val forInt: YamlKeyDecoder[Int] = fromScalarDecoder(YamlDecoder.forInt)
  implicit val forLong: YamlKeyDecoder[Long] = fromScalarDecoder(YamlDecoder.forLong)
  implicit val forFloat: YamlKeyDecoder[Float] = fromScalarDecoder(YamlDecoder.forFloat)
  implicit val forDouble: YamlKeyDecoder[Double] = fromScalarDecoder(YamlDecoder.forDouble)
  implicit val forBigInt: YamlKeyDecoder[BigInt] = fromScalarDecoder(YamlDecoder.forBigInt)
  implicit val forBigDecimal: YamlKeyDecoder[BigDecimal] = fromScalarDecoder(YamlDecoder.forBigDecimal)
  implicit val forUUID: YamlKeyDecoder[java.util.UUID] = instance { s =>
    try Right(java.util.UUID.fromString(s))
    catch { case e: IllegalArgumentException => Left(ConstructError.from(e, "UUID")) }
  }
}
