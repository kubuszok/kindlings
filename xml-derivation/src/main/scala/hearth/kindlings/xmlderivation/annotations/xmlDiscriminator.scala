package hearth.kindlings.xmlderivation.annotations

import scala.annotation.StaticAnnotation

/** On a sealed trait / Scala 3 enum: the name of the discriminator attribute of that type, whatever the `XmlConfig`
  * says (`withDiscriminator(name)`, `withNoDiscriminator`).
  */
final class xmlDiscriminator(val name: String) extends StaticAnnotation
