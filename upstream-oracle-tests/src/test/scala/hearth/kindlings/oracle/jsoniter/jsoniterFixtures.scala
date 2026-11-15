package hearth.kindlings.oracle.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.macros.{
  named as uNamed,
  stringified as uStringified,
  transient as uTransient
}
import hearth.kindlings.jsoniterderivation.annotations.{named as kNamed, stringified as kStringified, transientField}

import java.time.*
import java.util.UUID
import scala.collection.immutable.BitSet

// Fixtures shared by both sides of `JsoniterOracleSpec`. Annotated fixtures carry both libraries' annotations (each
// library ignores the other's).

final case class JOPerson(name: String, age: Int, email: Option[String], tags: List[String])

final case class JODefaults(
    i: Int = 1,
    s: String = "d",
    l: List[Int] = List(1, 2),
    o: Option[Int] = Some(3),
    m: Map[String, Int] = Map("k" -> 1)
)

final case class JOEmptyable(l: List[Int], v: Vector[String], m: Map[String, Int], s: Set[Int], o: Option[Int])

final case class JONestedOption(oo: Option[Option[Int]], o: Option[String])

final case class JOPrimitives(
    b: Byte,
    s: Short,
    i: Int,
    l: Long,
    f: Float,
    d: Double,
    c: Char,
    z: Boolean,
    str: String
)

final case class JOBoxed(i: java.lang.Integer, l: java.lang.Long, d: java.lang.Double, z: java.lang.Boolean)

final case class JOBig(bd: BigDecimal, bi: BigInt)

final case class JOId(value: Long) extends AnyVal

final case class JOWrapped(id: JOId, ids: List[JOId], maybe: Option[JOId])

final case class JOMaps(
    byInt: Map[Int, String],
    byLong: Map[Long, Boolean],
    byUuid: Map[UUID, Int],
    byId: Map[JOId, String],
    byString: Map[String, List[Int]]
)

final case class JOCollections(
    seq: Seq[Int],
    vector: Vector[String],
    set: Set[Int],
    list: List[Option[Int]],
    nested: List[List[Int]],
    bits: BitSet
)

final case class JONonEmpty(head: ::[Int], rest: List[Int], withDefault: ::[Int] = ::(0, Nil))

final case class JOTuples(pair: (Int, String), triple: (Boolean, Double, List[Int]))

final case class JOTime(
    instant: Instant,
    date: LocalDate,
    time: LocalTime,
    dateTime: LocalDateTime,
    offset: OffsetDateTime,
    zoned: ZonedDateTime,
    duration: Duration,
    period: Period,
    uuid: UUID
)

final case class JOEither(e: Either[String, Int])

sealed trait JOShape
final case class JOCircle(radius: Double) extends JOShape
final case class JORect(width: Double, height: Double) extends JOShape
case object JOEmpty extends JOShape
@kNamed("sq") @uNamed("sq") final case class JOSquare(side: Double) extends JOShape

final case class JODrawing(shapes: List[JOShape], main: JOShape)

final case class JOShapeHolder(
    shape: JOShape = JOEmpty,
    colour: JOColour = JORed,
    either: Either[String, Int] = Right(0)
)

sealed trait JOColour
case object JORed extends JOColour
case object JOGreen extends JOColour
@kNamed("blue") @uNamed("blue") case object JOBlue extends JOColour

object JOLevel extends Enumeration {
  val Low, High = Value
}

final case class JOEnums(level: JOLevel.Value, rounding: java.math.RoundingMode, colour: JOColour)

final case class JOTree(value: Int, children: List[JOTree])

sealed trait JOExpr
final case class JOLit(value: Int) extends JOExpr
final case class JOAdd(left: JOExpr, right: JOExpr) extends JOExpr

final case class JOAnnotated(
    @kNamed("full_name") @uNamed("full_name") name: String,
    @transientField @uTransient cache: Int = 0,
    @kStringified @uStringified count: Int,
    @kStringified @uStringified maybe: Option[Long],
    @kStringified @uStringified values: List[BigDecimal]
)

final case class JOCamel(firstName: String, lastName: String, httpServer: String, field1: Int)

final case class JOName(value: String)
final case class JOUser(name: JOName, alias: Option[JOName])
final case class JONameMap(byName: Map[JOName, Int])

final case class JOScalaTransient(name: String, @transient cache: Int = 7)

final case class JOStringy(i: Int, l: List[Long])

final case class JOLiteral(s: "VVV", i: 2, b: true)
