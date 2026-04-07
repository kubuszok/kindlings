package hearth.kindlings.pureconfigderivation.annotations

import scala.annotation.StaticAnnotation

/** Override the HOCON key used for a case-class field or sealed-trait subtype.
  *
  * Takes precedence over [[PureConfig.transformMemberNames]] / [[PureConfig.transformConstructorNames]].
  */
final class configKey(val name: String) extends StaticAnnotation
