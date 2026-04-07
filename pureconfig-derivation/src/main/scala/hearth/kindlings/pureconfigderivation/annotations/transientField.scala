package hearth.kindlings.pureconfigderivation.annotations

import scala.annotation.StaticAnnotation

/** Skip a field during reading and writing. The field must have a default value, which is used as the placeholder when
  * reading.
  */
final class transientField extends StaticAnnotation
