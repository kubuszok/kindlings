package hearth.kindlings.oracle.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.macros.{
  named as uNamed,
  stringified as uStringified,
  transient as uTransient
}
import hearth.kindlings.jsoniterderivation.annotations.{named as kNamed, stringified as kStringified, transientField}

import java.time.*
import java.util.UUID
import scala.annotation.nowarn

// Fixtures of jsoniter-scala's own `JsonCodecMakerSpec` (jsoniter-scala-macros/shared/src/test/scala/.../
// JsonCodecMakerSpec.scala, v2.42.3), under the names they have there prefixed with `JU`, for
// `JsoniterUpstreamSpecOracleSpec`. Annotated fixtures carry both libraries' annotations (each library ignores the
// other's). Fixtures with arrays render them in `toString` (the oracle compares rendered values).

private[jsoniter] object JuRender {
  def apply(value: Any): String = value match {
    case null        => "null"
    case a: Array[?] => a.map(apply).mkString("Array(", ", ", ")")
    case other       => other.toString
  }
}

final case class JUUserId(id: String) extends AnyVal
final case class JUOrderId(value: Int) extends AnyVal
final case class JUUserIds(l: List[String]) extends AnyVal

object JUAlias {
  type I = Int
  type S = String
  final case class UserId(id: S) extends AnyVal
  final case class OrderId(value: I) extends AnyVal
}

object JUGeneric {
  final case class UserId[S](id: S) extends AnyVal
  final case class OrderId[I](value: I) extends AnyVal
}

final case class JUPrimitives(b: Byte, s: Short, i: Int, l: Long, bl: Boolean, ch: Char, dbl: Double, f: Float)

final case class JUBoxedPrimitives(
    b: java.lang.Byte,
    s: java.lang.Short,
    i: java.lang.Integer,
    l: java.lang.Long,
    bl: java.lang.Boolean,
    ch: java.lang.Character,
    dbl: java.lang.Double,
    f: java.lang.Float
)

final case class JUStandardTypes(s: String, bi: BigInt, bd: BigDecimal)

final case class JUJavaEnums(l: java.util.concurrent.TimeUnit, il: java.math.RoundingMode)

final case class JUJavaTypes(uuid: UUID)

object JULocationType extends Enumeration {
  type LocationType = Value
  val IP, GPS: LocationType = Value
  def extra(name: String): LocationType = Value(nextId, name)
}

@nowarn
object JUCelestialBody extends Enumeration {
  type CelestialBody = Value
  val Sun, Moon, Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, Neptune: CelestialBody = Value
  private val Vesta: CelestialBody = Value("Vesta")
  protected val Pallas: CelestialBody = Value("Pallas")
  private[jsoniter] val Juno: CelestialBody = Value("Juno")
  lazy val Halley: CelestialBody = Value("Halley")
  var Encke: CelestialBody = Value
  def extra(name: String): CelestialBody = Value(nextId, name)
}

object JUCardRank extends Enumeration {
  type CardRank = Value
  val Two: CardRank = Value("2")
  val Ace: CardRank = Value("A")
  val King: CardRank = Value("King")
  val K: CardRank = King
  val Queen: CardRank = Value("Jack")
  val Jack: CardRank = Value("Queen")
}

object JUBird extends Enumeration {
  type Bird = Value
  val `Ñandú`, Emu: Bird = Value
}

object JUSpectralClass extends Enumeration {
  type SpectralClass = Value
  val O: SpectralClass = Value(0, "O")
  val G: SpectralClass = Value(5, "G")
  def extra(id: Int, name: String): SpectralClass = Value(id, name)
}

object JUTemperature extends Enumeration {
  type Temperature = Value
  val Cold: Temperature = Value(-1)
  val Hot: Temperature = Value(1)
}

final case class JUEnums(lt: JULocationType.LocationType)

final case class JUEnums2(@kStringified @uStringified lt: JULocationType.LocationType)

final case class JUOptions(
    os: Option[String],
    obi: Option[BigInt],
    osi: Option[Set[Int]],
    ol: Option[Long],
    ojl: Option[java.lang.Long],
    ost: Option[JUStandardTypes]
)

final case class JUTuples(
    t1: (Int, Double, List[Char]),
    t2: (String, BigInt, Option[JULocationType.LocationType])
)

final case class JUArrays(aa: Array[Array[Int]], a: Array[BigInt]) {
  override def toString: String = s"JUArrays(${JuRender(aa)},${JuRender(a)})"
}

final case class JUIterators(s: Iterator[Iterator[String]], i: Iterator[Int])

@nowarn("cat=deprecation")
final case class JUMutableMaps(
    hm: collection.mutable.HashMap[Boolean, collection.mutable.AnyRefMap[BigDecimal, Int]],
    m: collection.mutable.Map[Float, collection.mutable.ListMap[BigInt, String]],
    ohm: collection.mutable.OpenHashMap[Double, collection.mutable.LinkedHashMap[Short, Double]]
)

final case class JUImmutableMaps(
    m: Map[Int, Double],
    hm: collection.immutable.HashMap[String, collection.immutable.ListMap[Char, BigInt]],
    sm: collection.immutable.SortedMap[Long, collection.immutable.TreeMap[Byte, Float]]
)

final case class JUEmptyMaps(im: collection.immutable.Map[String, Int], mm: collection.mutable.Map[Long, Int])

final case class JUGenericMaps(m: collection.Map[Int, Boolean])

final case class JUIterables(
    s: collection.Set[collection.SortedSet[String]],
    is: collection.IndexedSeq[collection.Seq[Float]],
    i: collection.Iterable[Int]
)

@nowarn("cat=deprecation")
final case class JUMutableIterables(
    ml: collection.mutable.Seq[collection.mutable.SortedSet[String]],
    ab: collection.mutable.ArrayBuffer[collection.mutable.Set[BigInt]],
    as: collection.mutable.ArraySeq[collection.mutable.LinkedHashSet[Int]],
    b: collection.mutable.Buffer[collection.mutable.HashSet[Double]],
    lb: collection.mutable.ListBuffer[collection.mutable.TreeSet[Long]],
    is: collection.mutable.IndexedSeq[collection.mutable.ArrayStack[Float]],
    ub: collection.mutable.UnrolledBuffer[collection.mutable.Iterable[Short]]
)

@nowarn("cat=deprecation")
final case class JUImmutableIterables(
    l: List[collection.immutable.ListSet[String]],
    nl: ::[::[Int]],
    q: collection.immutable.Queue[Set[BigInt]],
    is: IndexedSeq[collection.immutable.SortedSet[Int]],
    s: Stream[collection.immutable.TreeSet[Double]],
    v: Vector[Iterable[Long]]
)

final case class JUEmptyIterables(l: List[String], a: collection.mutable.ArrayBuffer[Int])

final case class JUEmptyIterables2(l: List[String], s: Set[Int], ls: List[Set[Int]])

final case class JUNestedIterables(lessi: List[Either[String, Set[Int]]])

final case class JUMutableLongMaps(lm1: collection.mutable.LongMap[Double], lm2: collection.mutable.LongMap[String])

final case class JUStringifiedMutableLongMaps(@kStringified @uStringified lm: collection.mutable.LongMap[Double])

final case class JUImmutableIntLongMaps(
    im: collection.immutable.IntMap[Double],
    lm: collection.immutable.LongMap[String]
)

final case class JUStringifiedImmutableIntLongMaps(
    @kStringified @uStringified im: collection.immutable.IntMap[Double],
    @kStringified @uStringified lm: collection.immutable.LongMap[Double]
)

final case class JUBitSets(
    bs: collection.BitSet,
    ibs: collection.immutable.BitSet,
    mbs: collection.mutable.BitSet
)

final case class JUCamelPascalSnakeKebabCases(
    camelCase: Int,
    PascalCase: Int,
    snake_case: Int,
    `kebab-case`: Int,
    camel1: Int,
    Pascal1: Int,
    snake_1: Int,
    `kebab-1`: Int
)

final case class JUNameOverridden(@kNamed("new_name") @uNamed("new_name") oldName: String)

final case class JURecursive(s: String, bd: BigDecimal, l: List[Int], m: Map[Char, JURecursive])

final case class JUStringified(
    @kStringified @uStringified i: Int,
    @kStringified @uStringified bi: BigInt,
    @kStringified @uStringified o: Option[Int],
    @kStringified @uStringified l: List[Int]
)

final case class JUDefaults(
    st: String = "VVV",
    i: Int = 1,
    bi: BigInt = -1,
    oc: Option[Char] = Some('X'),
    l: List[Int] = collection.immutable.List(0),
    a: Array[Array[Long]] = Array(Array(1L, 2L), Array(3L, 4L)),
    ab: collection.mutable.ArrayBuffer[Int] = collection.mutable.ArrayBuffer(1, 2),
    m: Map[Int, Boolean] = Map(1 -> true),
    mm: collection.mutable.Map[String, Int] = collection.mutable.Map("VVV" -> 1),
    im: collection.immutable.IntMap[String] = collection.immutable.IntMap(1 -> "VVV"),
    lm: collection.mutable.LongMap[Int] = collection.mutable.LongMap(1L -> 2),
    s: Set[String] = Set("VVV"),
    ms: collection.mutable.Set[Int] = collection.mutable.Set(1),
    bs: collection.immutable.BitSet = collection.immutable.BitSet(1),
    mbs: collection.mutable.BitSet = scala.collection.mutable.BitSet(1)
) {
  override def toString: String =
    s"JUDefaults($st,$i,$bi,$oc,$l,${JuRender(a)},$ab,$m,$mm,$im,$lm,$s,$ms,$bs,$mbs)"
}

final case class JUDefaults2(
    st: String = "VVV",
    i: Int = 1,
    bi: BigInt = -1,
    oc: Option[Char] = Some('X'),
    l: List[Int] = collection.immutable.List(0),
    ab: collection.mutable.ArrayBuffer[Int] = collection.mutable.ArrayBuffer(1, 2),
    m: Map[Int, Boolean] = Map(1 -> true),
    mm: collection.mutable.Map[String, Int] = collection.mutable.Map("VVV" -> 1),
    im: collection.immutable.IntMap[String] = collection.immutable.IntMap(1 -> "VVV"),
    lm: collection.mutable.LongMap[Int] = collection.mutable.LongMap(1L -> 2),
    s: Set[String] = Set("VVV"),
    ms: collection.mutable.Set[Int] = collection.mutable.Set(1),
    bs: collection.immutable.BitSet = collection.immutable.BitSet(1),
    mbs: collection.mutable.BitSet = collection.mutable.BitSet(1)
)

object JUSharedEmptyMutables {
  val ab: collection.mutable.ArrayBuffer[Int] = collection.mutable.ArrayBuffer.empty
  val lb: collection.mutable.ListBuffer[Int] = collection.mutable.ListBuffer.empty
  val mm: collection.mutable.Map[String, Int] = collection.mutable.Map.empty
  val lm: collection.mutable.LongMap[Int] = collection.mutable.LongMap.empty
  val ms: collection.mutable.Set[Int] = collection.mutable.Set.empty
}

final case class JUSharedEmptyMutableDefaults(
    ab: collection.mutable.ArrayBuffer[Int] = JUSharedEmptyMutables.ab,
    lb: collection.mutable.ListBuffer[Int] = JUSharedEmptyMutables.lb,
    mm: collection.mutable.Map[String, Int] = JUSharedEmptyMutables.mm,
    lm: collection.mutable.LongMap[Int] = JUSharedEmptyMutables.lm,
    ms: collection.mutable.Set[Int] = JUSharedEmptyMutables.ms
)

final case class JUPolymorphicDefaults[A, B](i: A = 1, cs: List[B] = Nil)

final case class JUIterablesWithDefaults(l: List[Int] = Nil, s: Set[Option[String]] = Set())

trait JUNestedDefaults { // jsoniter-scala issue 1245
  case class User(name: String, age: Int = 42)
  case class Box[A](a: A, tags: List[String] = List("x"))
}
object JUNestedDefaults extends JUNestedDefaults

final case class JUTransient(@transientField @uTransient transient: String = "default", required: String) {
  val ignored: String = s"$required-$transient" // always transient
}

final case class JUScalaTransient(@scala.transient transient: String = "default", required: String) {
  val ignored: String = s"$required-$transient" // always transient
}

final case class JUNoneValues(opt: Option[String])

final case class JUSkipUnknown()

final case class JURequired32(
    r00: Int = 0,
    r01: Int,
    r02: Int,
    r03: Int,
    r04: Int,
    r05: Int,
    r06: Int,
    r07: Int,
    r08: Int,
    r09: Int,
    r10: Int = 10,
    r11: Int,
    r12: Int,
    r13: Int,
    r14: Int,
    r15: Int,
    r16: Int,
    r17: Int,
    r18: Int,
    r19: Int,
    r20: Int = 20,
    r21: Int,
    r22: Int,
    r23: Int,
    r24: Int,
    r25: Int,
    r26: Int,
    r27: Int,
    r28: Int,
    r29: Int,
    r30: Int = 30,
    r31: Int
)

final case class JURequiredAfterOptionalFields(f1: Option[Long], f2: Long, f3: Option[Long], f4: Long)

final case class JUValueClassTypes(uid: JUUserId, oid: JUOrderId)

// One-value (not `AnyVal`) classes for `inlineOneValueClasses`.
final case class JUOneUserId(value: String)
final case class JUOneOrderId(value: Int)
final case class JUOneValueClassTypes(uid: JUOneUserId, oid: JUOneOrderId)
final case class JUTransientUserId(@transientField @uTransient source: String = "unknown", value: String)
final case class JUTransientOrderId(@transientField @uTransient source: Int = -1, value: Int)
final case class JUTransientOneValueClassTypes(uid: JUTransientUserId, oid: JUTransientOrderId)
final case class JUDependentTransientUserId(value: String)(@transientField @uTransient val length: Int = value.length) {
  override def toString: String = s"JUDependentTransientUserId($value)($length)"
}
final case class JUOneUserIds(l: List[String])
final case class JUOneUserIdsT[T](l: T)
final case class JUOneUserIdsF[F[_], A](l: F[A])

final case class JUNestedOptions(y: Option[String], x: Option[Option[Option[String]]] = None)

final case class JUModel(field1: Option[String], field2: Option[Option[String]] = None)

final case class JUEitherFoo(value: Either[Int, Int])

final case class JUTupleAliases(t1: (JUAlias.S, JUAlias.I), t2: (JUAlias.I, JUAlias.S))

final case class JUUTF8KeysAndValues(გასაღები: String)

final case class JUOperators(` =<>!#%^&|*/\\~+-:$ `: Int)

final case class JUOptionInner(a: Int, b: Boolean, c: String)

// ADTs

sealed trait JUAdtBase extends Product with Serializable
sealed abstract class JUInner extends JUAdtBase {
  def a: Int
}
final case class JUAAA(a: Int) extends JUInner
final case class JUBBB(a: BigInt) extends JUAdtBase
final case class JUCCC(a: Int, b: String) extends JUInner
case object JUDDD extends JUAdtBase

object JUNestedAdts {
  sealed trait Base extends Product with Serializable
  object Leafs {
    final case class Leaf(a: Int) extends Base
    case object LeafObj extends Base
  }
}

sealed trait JUGADT2[A] extends Product with Serializable
object JUGADT2 {
  final case class IsDir(path: String) extends JUGADT2[Boolean]
  final case class Exists(path: String) extends JUGADT2[Boolean]
  final case class ReadBytes(path: String) extends JUGADT2[Array[Byte]]
  final case class CopyOver(src: Seq[Byte], path: String) extends JUGADT2[Int]
}

sealed trait JUFruit[T <: JUFruit[T]] extends Product with Serializable
final case class JUApple(family: String) extends JUFruit[JUApple]
final case class JUOrange(color: Int) extends JUFruit[JUOrange]

object JUKingDom {
  sealed trait Human
  object Human {
    final case class Subject(name: String) extends Human
    final case class King(name: String, reignOver: Iterable[Human]) extends Human
  }
}

sealed abstract class JUVersion(val value: String)
object JUVersion {
  case object Current extends JUVersion("8.10")
  @kNamed("8.09") @uNamed("8.09") case object `8.09` extends JUVersion("8.9")
}

sealed trait JUWeapon extends Product with Serializable
object JUWeapon {
  case object Axe extends JUWeapon
  case object Sword extends JUWeapon
}

final case class JUJavaTimeTypes(
    dow: DayOfWeek,
    d: Duration,
    i: Instant,
    ld: LocalDate,
    ldt: LocalDateTime,
    lt: LocalTime,
    m: Month,
    md: MonthDay,
    odt: OffsetDateTime,
    ot: OffsetTime,
    p: Period,
    y: Year,
    ym: YearMonth,
    zdt: ZonedDateTime,
    zi: ZoneId,
    zo: ZoneOffset
)

final case class JUJavaTimeMaps(
    dow: Map[DayOfWeek, Int],
    d: Map[Duration, Int],
    i: Map[Instant, Int],
    ld: Map[LocalDate, Int],
    ldt: Map[LocalDateTime, Int],
    lt: Map[LocalTime, Int],
    m: Map[Month, Int],
    md: Map[MonthDay, Int],
    odt: Map[OffsetDateTime, Int],
    ot: Map[OffsetTime, Int],
    p: Map[Period, Int],
    y: Map[Year, Int],
    ym: Map[YearMonth, Int],
    zdt: Map[ZonedDateTime, Int],
    zi: Map[ZoneId, Int],
    zo: Map[ZoneOffset, Int]
)
