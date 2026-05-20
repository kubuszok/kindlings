package hearth.kindlings.catsderivation

trait ShowPretty[A] extends cats.Show[A] {
  def showLines(a: A): List[String]
  override def show(a: A): String = showLines(a).mkString(System.lineSeparator)
}
object ShowPretty
