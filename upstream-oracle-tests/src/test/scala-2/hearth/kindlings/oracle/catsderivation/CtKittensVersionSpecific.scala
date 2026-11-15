package hearth.kindlings.oracle.catsderivation

import _root_.cats.{Foldable, Functor, Traverse}
import _root_.cats.derived.semiauto
import _root_.cats.kernel.Hash

/** kittens instances whose availability depends on the Scala version. */
trait CtKittensVersionSpecific {

  /** kittens' `MkHash` derives the fields of a product eagerly, so recursive types diverge. */
  def hashTree: Option[Hash[CtTree]] = None

  // As in kittens' Scala 2 tests, recursive `F[_]` instances are declared implicit so the recursive field finds them
  // (deriving them without the instance in scope crashes scalac inside shapeless' `Lazy`).
  def functorIList: Functor[CtIList] = CtKittensScala2Recursive.functorIList
  def foldableIList: Foldable[CtIList] = CtKittensScala2Recursive.foldableIList
  def traverseIList: Traverse[CtIList] = CtKittensScala2Recursive.traverseIList
}

object CtKittensScala2Recursive {
  implicit val functorIList: Functor[CtIList] = semiauto.functor[CtIList]
  implicit val foldableIList: Foldable[CtIList] = semiauto.foldable[CtIList]
  implicit val traverseIList: Traverse[CtIList] = semiauto.traverse[CtIList]
}
