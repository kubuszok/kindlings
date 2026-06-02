package hearth.kindlings.diffderivation

trait ObjectMatcher[A] {
  def key(a: A): Any
}
object ObjectMatcher {

  def by[A, K](f: A => K): ObjectMatcher[A] = new ObjectMatcher[A] {
    def key(a: A): Any = f(a)
  }
}
