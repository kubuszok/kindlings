package hearth.kindlings.oracle.avro

import com.sksamuel.avro4s.{
  AvroAlias,
  AvroDoc,
  AvroEnumDefault,
  AvroErasedName,
  AvroFixed,
  AvroName,
  AvroNamespace,
  AvroNoDefault,
  AvroProp,
  AvroSortPriority,
  AvroTransient
}
import hearth.kindlings.avroderivation.annotations.{
  avroAlias,
  avroDoc,
  avroEnumDefault,
  avroErasedName,
  avroFixed,
  avroName,
  avroNamespace,
  avroNoDefault,
  avroProp,
  avroSortPriority,
  fieldName,
  transientField
}

// Fixtures shared by both sides of the avro oracle. Annotated fixtures carry both avro4s' annotation and Kindlings'
// equivalent (each library reads only its own).

case class AvPerson(name: String, age: Int)

case class AvPrimitives(b: Boolean, by: Byte, sh: Short, i: Int, l: Long, f: Float, d: Double, s: String)

case class AvOptions(a: Option[String], b: Option[Int], p: Option[AvPerson])

case class AvCollections(l: List[Int], v: Vector[String], s: Set[Long], seq: Seq[Double], nested: List[List[Int]])

case class AvMaps(m: Map[String, Int], nested: Map[String, List[String]], people: Map[String, AvPerson])

case class AvNested(owner: AvPerson, members: List[AvPerson], lead: Option[AvPerson])

sealed trait AvShape
case class AvCircle(radius: Double) extends AvShape
case class AvRect(width: Double, height: Double) extends AvShape
case class AvDrawing(shape: AvShape, shapes: List[AvShape], maybe: Option[AvShape])

sealed trait AvColor
case object AvRed extends AvColor
case object AvGreen extends AvColor
case object AvBlue extends AvColor
case class AvPalette(main: AvColor, others: List[AvColor])

case class AvEithers(e: Either[String, Int], rec: Either[AvPerson, AvRect])

case class AvTuples(t2: (String, Int), t3: (Int, String, Boolean))

case class AvTree(value: Int, children: List[AvTree])

case class AvId(value: String) extends AnyVal
case class AvUsesId(id: AvId, ids: List[AvId], maybe: Option[AvId])

case class AvBytes(arr: Array[Byte], vec: Vector[Byte])

case class AvMoney(amount: BigDecimal)

case class AvTimes(
    instant: java.time.Instant,
    date: java.time.LocalDate,
    time: java.time.LocalTime,
    dateTime: java.time.LocalDateTime
)

case class AvOffset(at: java.time.OffsetDateTime)

case class AvUuid(id: java.util.UUID)

case class AvDefaults(
    s: String = "x",
    i: Int = 7,
    o: Option[String] = None,
    os: Option[Int] = Some(3),
    l: List[Int] = List(1, 2),
    p: AvPerson = AvPerson("n", 1)
)

case class AvPair[A, B](a: A, b: B)
case class AvBox[A](value: A)
case class AvGenerics(pair: AvPair[String, Int], box: AvBox[AvPair[String, Int]], boxed: AvBox[Int])

@AvroErasedName
@avroErasedName
case class AvErased[A](value: A)
case class AvUsesErased(e: AvErased[Int])

@AvroName("AvRenamedRecord")
@avroName("AvRenamedRecord")
@AvroNamespace("custom.ns")
@avroNamespace("custom.ns")
@AvroDoc("An annotated record")
@avroDoc("An annotated record")
@AvroAlias("AvOldName")
@avroAlias("AvOldName")
@AvroProp("team", "data")
@avroProp("team", "data")
case class AvAnnotated(
    @AvroName("full_name") @fieldName("full_name") name: String,
    @AvroDoc("Age in years") @avroDoc("Age in years") age: Int,
    @AvroAlias("old_tag") @avroAlias("old_tag") tag: String,
    @AvroProp("format", "short") @avroProp("format", "short") code: String,
    @AvroTransient() @transientField cache: Option[String] = None
)

case class AvFixed(@AvroFixed(4) @avroFixed(4) token: Array[Byte])

case class AvNoDefault(@AvroNoDefault() @avroNoDefault count: Int = 5, name: String = "a")

sealed trait AvPriority
@AvroSortPriority(1)
@avroSortPriority(1)
case object AvLow extends AvPriority
@AvroSortPriority(3)
@avroSortPriority(3)
case object AvHigh extends AvPriority
@AvroSortPriority(2)
@avroSortPriority(2)
case object AvMid extends AvPriority

@AvroEnumDefault(AvSouth)
@avroEnumDefault("AvSouth")
sealed trait AvDirection
case object AvNorth extends AvDirection
case object AvSouth extends AvDirection

case class AvWidenedLong(i: Long)
case class AvWidenedDouble(d: Double)

case class AvTwoTuples(a: (String, Int), b: (Int, String))

case class AvLegacyTimes(date: java.sql.Date, timestamp: java.sql.Timestamp)

case class AvInstants(instant: java.time.Instant, timestamp: java.sql.Timestamp)

object AvWeekday extends Enumeration {
  val Mon, Tue, Wed = Value
}
case class AvUsesWeekday(day: AvWeekday.Value, days: List[AvWeekday.Value])

sealed trait AvMixed
case object AvNone extends AvMixed
case class AvSome(value: Int) extends AvMixed
case class AvUsesMixed(m: AvMixed)

// declaration order differs from the alphabetical one
sealed trait AvAnimal
case class AvZebra(stripes: Int) extends AvAnimal
case class AvAnt(legs: Int) extends AvAnimal
sealed trait AvLetter
case object AvZ extends AvLetter
case object AvA extends AvLetter
case class AvUsesOrder(animal: AvAnimal, letter: AvLetter)

case class AvOptionalSums(shape: Option[AvShape], either: Option[Either[String, Int]], color: Option[AvColor])

case class AvFieldNs(@AvroNamespace("other.ns") @avroNamespace("other.ns") person: AvPerson)

case class AvCamel(firstName: String, lastNameOfPerson: Int)

case class AvEvolV1(name: String)
case class AvEvolV2(name: String, nickname: Option[String] = None, score: Int = 10)
