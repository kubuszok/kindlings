package hearth.kindlings.sconfigderivation.annotations

import scala.annotation.StaticAnnotation

/** Skip a field during reading and writing. The field must have a default value. */
final class transientField extends StaticAnnotation
