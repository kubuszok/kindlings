package hearth.kindlings.fastshowpretty.annotations

import scala.annotation.StaticAnnotation

final class sensitiveData(val reason: String) extends StaticAnnotation {
  def this() = this("")
}
