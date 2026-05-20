package hearth.kindlings.diffderivation

trait Diff[A] {
  def prettyName: String
  def plainName: String
  def simpleName: String
  def shortName: String
  def diff(left: A, right: A): DiffResult
  def snapshot(value: A): DiffResult
}
object Diff extends DiffCompanionCompat {

  def apply[A](implicit ev: Diff[A]): Diff[A] = ev

  sealed trait LogDerivation
  object LogDerivation extends LogDerivation
}
