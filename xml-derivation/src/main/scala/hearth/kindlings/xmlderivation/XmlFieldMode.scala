package hearth.kindlings.xmlderivation

sealed trait XmlFieldMode
object XmlFieldMode {
  case object Attribute extends XmlFieldMode
  case object Element extends XmlFieldMode
}
