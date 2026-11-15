package hearth.kindlings.oracle.catsderivation

import _root_.cats.{Foldable, Functor, NonEmptyTraverse, Reducible, Traverse}
import _root_.cats.derived.semiauto
import _root_.cats.kernel.Hash

/** kittens instances whose availability depends on the Scala version. */
trait CtKittensVersionSpecific {
  def hashTree: Option[Hash[CtTree]] = Some(semiauto.hash[CtTree])
  def functorIList: Functor[CtIList] = semiauto.functor[CtIList]
  def foldableIList: Foldable[CtIList] = semiauto.foldable[CtIList]
  def traverseIList: Traverse[CtIList] = semiauto.traverse[CtIList]
  def reducibleRose: Reducible[CtRose] = semiauto.reducible[CtRose]
  def nonEmptyTraverseRose: NonEmptyTraverse[CtRose] = semiauto.nonEmptyTraverse[CtRose]
}
