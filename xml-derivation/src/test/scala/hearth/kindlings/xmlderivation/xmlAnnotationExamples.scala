package hearth.kindlings.xmlderivation

import hearth.kindlings.xmlderivation.annotations.{
  xmlAttribute,
  xmlContent,
  xmlElement,
  xmlName,
  xmlUnwrapped,
  xmlWrapper
}

// Enums: sealed traits of case objects, a `scala.Enumeration`
sealed trait XmlColor
case object XmlRed extends XmlColor
@xmlName("verde") case object XmlGreen extends XmlColor
case object XmlDarkBlue extends XmlColor

object XmlWeekday extends Enumeration {
  val Mon, Tue = Value
}

case class XmlPaint(name: String, color: XmlColor, accent: Option[XmlColor])
case class XmlPaintAttr(@xmlAttribute color: XmlColor, @xmlAttribute accent: Option[XmlColor], name: String)
case class XmlPaintContent(@xmlAttribute id: Int, @xmlContent color: XmlColor)
case class XmlScheduleElements(day: XmlWeekday.Value, others: List[XmlWeekday.Value])
case class XmlPetOwner(name: String, pet: MixedPet, mood: XmlColor)
case class XmlPalette(colors: List[XmlColor], byName: Map[String, XmlColor], @xmlWrapper("extra") more: List[XmlColor])
case class XmlSchedule(day: XmlWeekday.Value, @xmlAttribute first: XmlWeekday.Value, others: List[XmlWeekday.Value])

// A subtype renamed with @xmlName; a nested sealed hierarchy with a case object
sealed trait XmlRenamedShape
@xmlName("circ") case class XmlRenamedCircle(r: Int) extends XmlRenamedShape
case class XmlRenamedSquare(side: Int) extends XmlRenamedShape

sealed trait XmlTransport
sealed trait XmlEngineTransport extends XmlTransport
case class XmlTransportCar(brand: String) extends XmlEngineTransport
case object XmlTransportTank extends XmlEngineTransport
case class XmlTransportBike(gears: Int) extends XmlTransport
case class XmlFleet(lead: XmlTransport, rest: List[XmlTransport])

// A type with user-provided instances, as an attribute and as content
final case class XmlVersion(major: Int, minor: Int)
object XmlVersion {
  implicit val encoder: XmlEncoder[XmlVersion] = new XmlEncoder[XmlVersion] {
    def encode(value: XmlVersion, elementName: String): scala.xml.Elem =
      scala.xml.Elem(
        null,
        elementName,
        scala.xml.Null,
        scala.xml.TopScope,
        false,
        scala.xml.Text(s"${value.major}.${value.minor}")
      )
  }
  implicit val decoder: XmlDecoder[XmlVersion] = new XmlDecoder[XmlVersion] {
    def decode(elem: scala.xml.Elem): Either[XmlDecodingError, XmlVersion] = elem.text.trim.split('.') match {
      case Array(major, minor) if major.forall(_.isDigit) && minor.forall(_.isDigit) =>
        Right(XmlVersion(major.toInt, minor.toInt))
      case _ => Left(XmlDecodingError.InvalidValue(elem.text, "XmlVersion", elem.label))
    }
  }
}
case class XmlRelease(@xmlAttribute version: XmlVersion, @xmlAttribute previous: Option[XmlVersion], name: String)
case class XmlReleaseContent(@xmlAttribute name: String, @xmlContent version: XmlVersion)
case class XmlReleaseElement(version: XmlVersion, name: String)

// @xmlContent: the other fields are attributes
case class XmlContentPlain(id: Int, lang: Option[String], @xmlContent body: String)
case class XmlContentOption(@xmlAttribute id: Int, @xmlContent body: Option[String])
case class XmlContentOptionInt(@xmlAttribute id: Int, @xmlContent count: Option[Int])
case class XmlContentInt(@xmlAttribute id: Int, @xmlContent count: Int)

// Invalid annotations (only used in `compileErrors`)
case class XmlBadUnwrappedString(@xmlUnwrapped name: String)
case class XmlBadUnwrappedOption(@xmlUnwrapped name: Option[List[Int]])
case class XmlBadWrappedAttribute(@xmlAttribute @xmlWrapper("w") name: String)
case class XmlBadWrappedUnwrapped(@xmlUnwrapped @xmlWrapper("w") names: List[String])
case class XmlBadWrapperTwice(@xmlWrapper("w") a: List[String], @xmlWrapper("w") b: List[Int])
case class XmlBadWrappedMap(@xmlWrapper("w") byId: Map[Int, String])
case class XmlBadAttributeCaseClass(@xmlAttribute address: Address, name: String)
case class XmlBadAttributeList(@xmlAttribute tags: List[String])
case class XmlBadAttributeOptionCaseClass(@xmlAttribute address: Option[Address])
case class XmlBadAttributeSealed(@xmlAttribute shape: Shape)
case class XmlBadAttributeElement(@xmlAttribute @xmlElement name: String)
case class XmlBadContentCaseClass(@xmlContent address: Address)
case class XmlBadContentTwice(@xmlContent a: String, @xmlContent b: String)
case class XmlBadContentWithElement(@xmlContent body: String, @xmlElement title: String)
case class XmlBadContentWithChild(@xmlContent body: String, tags: List[String])
