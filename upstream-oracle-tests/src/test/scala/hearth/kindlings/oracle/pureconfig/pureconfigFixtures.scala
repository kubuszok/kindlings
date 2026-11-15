package hearth.kindlings.oracle.pureconfig

// Shared fixtures for the pureconfig oracle: each is derived both by Kindlings and by pureconfig's own derivation
// (`pureconfig.generic.semiauto`: Shapeless on Scala 2, the hint-aware `pureconfig-generic-scala3` on Scala 3).

final case class PcPerson(name: String, age: Int, nickname: Option[String])

final case class PcCamel(firstName: String, maxRetryCount: Int, httpURLValue: String, a1b2: Int)

final case class PcDefaults(host: String = "localhost", port: Int = 8080, tags: List[String] = Nil)

final case class PcCollections(
    list: List[Int],
    set: Set[String],
    vector: Vector[Double],
    map: Map[String, Int],
    maybeList: Option[List[Int]]
)

final case class PcNested(owner: PcPerson, others: List[PcPerson], byName: Map[String, PcPerson])

final case class PcWrapper(value: String) extends AnyVal
final case class PcUsesWrapper(id: PcWrapper, maybeId: Option[PcWrapper])

final case class PcStrict(name: String, port: Int)

final case class PcNoDefaults(host: String = "localhost", port: Int = 8080)

sealed trait PcShape
final case class PcCircle(radius: Double) extends PcShape
final case class PcRectangle(width: Double, height: Double) extends PcShape
case object PcEmpty extends PcShape

final case class PcDrawing(name: String, shapes: List[PcShape])

sealed trait PcColor
case object PcRed extends PcColor
case object PcGreen extends PcColor
case object PcDarkBlue extends PcColor

final case class PcPalette(primary: PcColor, all: List[PcColor])

sealed trait PcEvent
final case class PcClicked(x: Int, y: Int) extends PcEvent
final case class PcTyped(text: String) extends PcEvent

// Subtypes whose shapes overlap: under FirstSuccessCoproductHint the order in which subtypes are tried decides the
// result. pureconfig tries them in the alphabetical order of their names (not the declaration order).
sealed trait PcSource
final case class PcUrlSource(url: String) extends PcSource
final case class PcFileSource(path: String) extends PcSource
final case class PcAnySource(url: String, path: Option[String]) extends PcSource
final case class PcInlineSource(content: String, encoding: String = "utf-8") extends PcSource

// A subtype with a field named like the discriminator key.
sealed trait PcTagged
final case class PcWithType(`type`: String, value: Int) extends PcTagged
final case class PcPlain(value: Int) extends PcTagged
