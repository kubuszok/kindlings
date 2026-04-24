package hearth.kindlings.scalacheckderivation.internal.runtime

import org.scalacheck.Shrink

@scala.annotation.nowarn("msg=deprecated")
object ShrinkUtils {

  /** Shrinks an Option[A] by first trying None, then shrinking the inner value. */
  def shrinkOption[A](innerShrink: Shrink[A]): Shrink[Option[A]] =
    Shrink {
      case None    => Stream.empty
      case Some(x) => None #:: innerShrink.shrink(x).map(Some(_))
    }

  /** Shrinks a collection by trying progressively smaller sublists and shrinking individual elements. Uses
    * Iterable[Any] at runtime to avoid higher-kinded type issues in macro-generated code.
    */
  def shrinkCollection(
      elemShrink: Shrink[Any],
      factory: Any // scala.collection.IterableFactory[CC] — erased
  ): Shrink[Any] =
    Shrink { value =>
      val coll = value.asInstanceOf[Iterable[Any]]
      val elems = coll.toList
      val f = factory.asInstanceOf[scala.collection.IterableFactory[Iterable]]
      if (elems.isEmpty) Stream.empty
      else {
        // Try removing elements (halving strategy)
        val removeStreams = removeChunks(elems).map(smaller => f.from(smaller))
        // Try shrinking individual elements
        val shrinkElemStreams = shrinkOne(elems, elemShrink).map(smaller => f.from(smaller))
        (removeStreams #::: shrinkElemStreams).asInstanceOf[Stream[Any]]
      }
    }

  /** Shrinks a case class by shrinking one field at a time. */
  def shrinkCaseClass[A](
      fieldShrinks: List[Shrink[Any]],
      extract: A => Array[Any],
      reconstruct: Array[Any] => A
  ): Shrink[A] =
    Shrink { value =>
      val fields = extract(value)
      // For each field, try shrinking it while keeping others constant
      fieldShrinks.zipWithIndex.toStream.flatMap { case (shrink, idx) =>
        shrink.shrink(fields(idx)).map { shrunkField =>
          val newFields = fields.clone()
          newFields(idx) = shrunkField
          reconstruct(newFields)
        }
      }
    }

  /** Shrinks an enum/sealed trait value by delegating to the Shrink for the actual runtime type. The list of Shrink
    * instances corresponds to the enum cases in declaration order. We try each one (catching ClassCastException) to
    * find the right variant.
    */
  def shrinkEnum[A](caseShrinks: List[Shrink[A]]): Shrink[A] =
    Shrink { value =>
      // Try each case's shrink — the matching one will succeed,
      // non-matching ones will produce empty streams or throw
      caseShrinks.toStream.flatMap { caseShrink =>
        try caseShrink.shrink(value)
        catch { case _: ClassCastException => Stream.empty }
      }
    }

  // --- Internal helpers ---

  /** Remove chunks of elements from a list (halving strategy from ScalaCheck). */
  private def removeChunks[A](xs: List[A]): Stream[List[A]] = {
    val n = xs.length
    if (n == 0) Stream.empty
    else if (n == 1) Stream(Nil)
    else {
      val half = n / 2
      val (left, right) = xs.splitAt(half)
      right #:: left #:: removeChunks(left).map(_ ++ right) #::: removeChunks(right).map(left ++ _)
    }
  }

  /** Shrink one element at a time in a list. */
  private def shrinkOne[A](xs: List[A], shrink: Shrink[A]): Stream[List[A]] = xs match {
    case Nil          => Stream.empty
    case head :: tail =>
      shrink.shrink(head).map(_ :: tail) #::: shrinkOne(tail, shrink).map(head :: _)
  }
}
