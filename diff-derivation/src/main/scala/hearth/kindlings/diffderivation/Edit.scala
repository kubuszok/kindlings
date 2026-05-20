package hearth.kindlings.diffderivation

sealed trait Edit[+A] extends Product with Serializable
object Edit {
  final case class Equal[+A](value: A) extends Edit[A]
  final case class Insert[+A](value: A) extends Edit[A]
  final case class Delete[+A](value: A) extends Edit[A]
}
