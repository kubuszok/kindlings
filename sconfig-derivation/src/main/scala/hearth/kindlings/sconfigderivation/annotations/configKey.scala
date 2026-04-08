package hearth.kindlings.sconfigderivation.annotations

import scala.annotation.StaticAnnotation

/** Override the HOCON key used for a case-class field or sealed-trait subtype. */
final class configKey(val name: String) extends StaticAnnotation
