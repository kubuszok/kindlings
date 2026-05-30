package hearth.kindlings.catsderivation.annotations

import scala.annotation.StaticAnnotation

final class sensitiveData(val reason: String) extends StaticAnnotation {
  def this() = this("")
}
