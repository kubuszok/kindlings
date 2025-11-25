package hearth.kindlings.fastshowpretty

trait FastShowPretty[A] {
  
  def render(sb: StringBuilder)(value: A): StringBuilder
}
object FastShowPretty {}

