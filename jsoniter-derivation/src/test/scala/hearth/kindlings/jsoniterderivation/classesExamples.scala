package hearth.kindlings.jsoniterderivation

import hearth.kindlings.jsoniterderivation.annotations.transientField

import java.util.Objects

// Plain (non-case) classes (Spec ~612)
class ParityNonCaseClass(val id: Int, var name: String) {
  override def hashCode(): Int = id * 31 + Objects.hashCode(name)
  override def equals(obj: Any): Boolean = obj match {
    case c: ParityNonCaseClass => id == c.id && Objects.equals(name, c.name)
    case _                     => false
  }
  override def toString: String = s"ParityNonCaseClass(id=$id,name=$name)"
}

class ParityNonCaseWithDefaults(val id: Int, val tags: List[String] = List("a"), val note: Option[String] = None) {
  override def hashCode(): Int = Objects.hash(Int.box(id), tags, note)
  override def equals(obj: Any): Boolean = obj match {
    case c: ParityNonCaseWithDefaults => id == c.id && tags == c.tags && note == c.note
    case _                            => false
  }
  override def toString: String = s"ParityNonCaseWithDefaults($id,$tags,$note)"
}

case class ParityHasNonCaseClass(inner: ParityNonCaseClass, list: List[ParityNonCaseClass])

// ADT leaves which are not case classes or case objects (Spec ~2839)
sealed trait ParityPlainAdt
class ParityPlainLeaf() extends ParityPlainAdt {
  override def hashCode(): Int = 1
  override def equals(obj: Any): Boolean = obj.isInstanceOf[ParityPlainLeaf]
  override def toString: String = "ParityPlainLeaf()"
}
class ParityPlainLeafWithFields(val x: Int, var y: String) extends ParityPlainAdt {
  override def hashCode(): Int = x
  override def equals(obj: Any): Boolean = obj match {
    case c: ParityPlainLeafWithFields => x == c.x && y == c.y
    case _                            => false
  }
  override def toString: String = s"ParityPlainLeafWithFields($x,$y)"
}
object ParityPlainObject extends ParityPlainAdt {
  override def toString: String = "ParityPlainObject"
}

// ADTs with a non-abstract sealed base (Spec ~2856)
// (no equals/toString overrides: case class subclasses would inherit them instead of generating their own)
sealed class ParitySealedBase
case class ParitySealedB(n: Int) extends ParitySealedBase
case class ParitySealedC(s: String) extends ParitySealedBase

// Multiple parameter lists (Spec ~3518-3586)
case class ParityMultiList(i: Int)(val l: Long)(var s: String)
case class ParityMultiListDefault(i: Int = 1)(val l: Long)(var s: String)
case class ParityMultiListDependent(i: Int = 1)(val l: Long = i - 1L)(var s: String = s"$i-$l")
case class ParityGenericMultiList[A](a: A)(@transientField val n: Int = 2)(val as: List[A] = List.fill(n)(a))(
    val m: Int = as.size * n
)
sealed trait ParityMultiListAdt
case class ParityMultiData1(i: Int)(val pos: Int) extends ParityMultiListAdt
case class ParityMultiData2(i: Int)(val pos: Int) extends ParityMultiListAdt

object ClassesRuntimeConfigs {
  @volatile var default: JsoniterConfig = JsoniterConfig.default
  @volatile var discriminator: JsoniterConfig = JsoniterConfig.default.withDiscriminator("type")
  @volatile var jsoniterScalaDefaults: JsoniterConfig = JsoniterConfig.jsoniterScalaDefaults
  @volatile var requireDefaults: JsoniterConfig = JsoniterConfig.default.withRequireDefaultFields
}

// Nested options without skipNestedOptionValues (Spec ~1345)
case class ParityNestedOptions3(y: Option[String], x: Option[Option[Option[String]]] = None)
object NestedOptionsRuntimeConfigs {
  @volatile var default: JsoniterConfig = JsoniterConfig.default
  @volatile var jsoniterScalaDefaults: JsoniterConfig = JsoniterConfig.jsoniterScalaDefaults
  @volatile var circeLike: JsoniterConfig = JsoniterConfig.default.withCirceLikeObjectEncoding
}

// User-provided codecs of ADT leaves with a discriminator (scala-3 JsonCodecMaker ~2767, ~4091)
sealed trait ParityCustomAdt
case class ParityCustomLeaf(v: Int) extends ParityCustomAdt
object ParityCustomLeaf {
  implicit val codec: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[ParityCustomLeaf] =
    new com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[ParityCustomLeaf] {
      def nullValue: ParityCustomLeaf = null
      def encodeValue(x: ParityCustomLeaf, out: com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter): Unit = {
        out.writeObjectStart()
        out.writeKey("type")
        out.writeVal("ParityCustomLeaf")
        out.writeKey("custom")
        out.writeVal(x.v)
        out.writeObjectEnd()
      }
      def decodeValue(
          in: com.github.plokhotnyuk.jsoniter_scala.core.JsonReader,
          default: ParityCustomLeaf
      ): ParityCustomLeaf = {
        if (!in.isNextToken('{'.toByte)) in.decodeError("expected '{'")
        var v = 0
        var l = -1
        while (l < 0 || in.isNextToken(','.toByte)) {
          l = in.readKeyAsCharBuf()
          if (in.isCharBufEqualsTo(l, "custom")) v = in.readInt() else in.skip()
        }
        ParityCustomLeaf(v)
      }
    }
}
case class ParityRegularLeaf(a: Int) extends ParityCustomAdt

// Iterator fields with transientEmpty (upstream ~3622)
case class ParityIteratorField(it: Iterator[Int], n: Int)

// User-provided JsonKeyCodecs take precedence over built-in key types (scala-3 JsonCodecMaker ~2340)
object ParityKeyCodecs {
  implicit val upperStringKeys: com.github.plokhotnyuk.jsoniter_scala.core.JsonKeyCodec[String] =
    new com.github.plokhotnyuk.jsoniter_scala.core.JsonKeyCodec[String] {
      def decodeKey(in: com.github.plokhotnyuk.jsoniter_scala.core.JsonReader): String = in.readKeyAsString().toLowerCase
      def encodeKey(x: String, out: com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter): Unit = out.writeKey(x.toUpperCase)
    }
  implicit val hexIntKeys: com.github.plokhotnyuk.jsoniter_scala.core.JsonKeyCodec[Int] =
    new com.github.plokhotnyuk.jsoniter_scala.core.JsonKeyCodec[Int] {
      def decodeKey(in: com.github.plokhotnyuk.jsoniter_scala.core.JsonReader): Int = Integer.parseInt(in.readKeyAsString(), 16)
      def encodeKey(x: Int, out: com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter): Unit = out.writeKey(Integer.toHexString(x))
    }
}

// inlineOneValueClasses: map keys and transient parameters (upstream genWriteKey/genReadKey)
case class ParityUserKey(v: String)
case class ParityTaggedId(id: Int, @transientField tag: String = "t")
object OneValueRuntimeConfigs {
  @volatile var inline: JsoniterConfig = JsoniterConfig.default.withInlineOneValueClasses
}

object ContradictoryRuntimeConfigs {
  @volatile var bothOnly: JsoniterConfig = JsoniterConfig.default.withDecodingOnly.withEncodingOnly
  @volatile var requireAndTransientEmpty: JsoniterConfig =
    JsoniterConfig.default.withRequireCollectionFields.withTransientEmpty
  @volatile var alwaysEmitWithoutDiscriminator: JsoniterConfig = JsoniterConfig.default.withAlwaysEmitDiscriminator
  @volatile var encodingOnly: JsoniterConfig = JsoniterConfig.default.withEncodingOnly
}

// @named on fields (jsoniter-scala's field rename annotation)
case class ParityNamedField(@hearth.kindlings.jsoniterderivation.annotations.named("renamed") original: Int, other: Int)
case class ParityPresetRecord(l: List[Int], o: Option[Int] = None, d: Int = 1)

// null / default semantics (Spec ~674, ~2530, ~3673)
case class ParityDefaults2(
    st: String = "VVV", bi: BigInt = -1, oc: Option[Char] = Some('X'), l: List[Int] = List(0),
    ab: scala.collection.mutable.ArrayBuffer[Int] = scala.collection.mutable.ArrayBuffer(1, 2),
    m: Map[Int, Boolean] = Map(1 -> true),
    s: Set[String] = Set("VVV"),
    v: Vector[Int] = Vector.empty
)

// A user-provided codec receives the field's default as `default` (genReadVal: `codec.decodeValue(in, <field var>)`)
final case class ParityTag(value: String)
object ParityTag {
  import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
  // "keep" (or null) decodes as the `default` argument
  implicit val codec: JsonValueCodec[ParityTag] = new JsonValueCodec[ParityTag] {
    def nullValue: ParityTag = ParityTag("null-value")
    def decodeValue(in: JsonReader, default: ParityTag): ParityTag =
      if (in.isNextToken('n'.toByte)) in.readNullOrError(default, "expected null")
      else {
        in.rollbackToken()
        val s = in.readString(null)
        if (s == "keep") default else ParityTag(s)
      }
    def encodeValue(x: ParityTag, out: JsonWriter): Unit = out.writeVal(x.value)
  }
}
case class ParityTagged(a: ParityTag = ParityTag("dflt"), b: ParityTag)
sealed trait ParityDefaultsAdt
case class ParityDefaultsLeaf(l: List[Int] = List(0), t: ParityTag = ParityTag("dflt"), n: Int = 0) extends ParityDefaultsAdt

// A custom Option codec: null is None, a missing field is the default (Spec ~674)
case class ParityCustomOptionEx(opt: Option[String] = Some("hiya"), level: Option[Int] = Some(10))
object ParityCustomOptionCodecs {
  import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
  implicit val intCodec: JsonValueCodec[Int] = KindlingsJsonValueCodec.derived[Int]
  implicit val stringCodec: JsonValueCodec[String] = KindlingsJsonValueCodec.derived[String]
  implicit def optionCodec[A](implicit aCodec: JsonValueCodec[A]): JsonValueCodec[Option[A]] =
    new JsonValueCodec[Option[A]] {
      override def decodeValue(in: JsonReader, default: Option[A]): Option[A] =
        if (in.isNextToken('n'.toByte)) in.readNullOrError(None, "expected 'null' or JSON value")
        else {
          in.rollbackToken()
          Some(aCodec.decodeValue(in, aCodec.nullValue))
        }
      override def encodeValue(x: Option[A], out: JsonWriter): Unit =
        if (x eq None) out.writeNull() else aCodec.encodeValue(x.get, out)
      override def nullValue: Option[A] = None
    }
}

// User-provided field codecs receive the field's default as `default` (upstream genReadVal with the field default)
case class ParityCounter(n: Int)
object ParityCounter {
  // decodes `{}` as the `default` it receives, to observe it
  implicit val codec: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[ParityCounter] =
    new com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[ParityCounter] {
      def nullValue: ParityCounter = ParityCounter(-1)
      def encodeValue(x: ParityCounter, out: com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter): Unit =
        out.writeVal(x.n)
      def decodeValue(in: com.github.plokhotnyuk.jsoniter_scala.core.JsonReader, default: ParityCounter): ParityCounter =
        if (in.isNextToken('{'.toByte)) {
          if (!in.isNextToken('}'.toByte)) in.decodeError("expected '}'")
          default
        } else {
          in.rollbackToken()
          ParityCounter(in.readInt())
        }
    }
}
case class ParityHasCounter(c: ParityCounter = ParityCounter(7), d: ParityCounter)
