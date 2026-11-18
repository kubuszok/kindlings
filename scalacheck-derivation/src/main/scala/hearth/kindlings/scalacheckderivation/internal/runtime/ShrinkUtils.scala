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

  /** Shrinks a value type built with a smart constructor (`wrap`, e.g. a refined type's `refineV`): shrinks the
    * underlying value and drops the candidates the constructor rejects (a `Left`, or an exception thrown by `wrap`).
    */
  def shrinkValueType[Inner, A](
      innerShrink: Shrink[Inner],
      unwrap: A => Inner,
      wrap: Inner => Either[Any, A]
  ): Shrink[A] =
    Shrink { value =>
      innerShrink.shrink(unwrap(value)).flatMap { candidate =>
        val wrapped =
          try wrap(candidate)
          catch { case scala.util.control.NonFatal(e) => Left(e) }
        wrapped match {
          case Right(shrunk) => Stream(shrunk)
          case Left(_)       => Stream.empty
        }
      }
    }

  /** Shrinks a collection by trying progressively smaller sublists and shrinking individual elements.
    *
    * Reads the value through the provider's `asIterable` and rebuilds every candidate through the provider's smart
    * constructor (`build`), so containers that are not Scala `Iterable`s (e.g. `cats.data.NonEmptyList`) work, and
    * candidates the constructor rejects (e.g. an empty list for a non-empty container) are simply dropped.
    */
  def shrinkCollectionWithBuild[Item, A](
      elemShrink: Shrink[Item],
      toIterable: A => Iterable[Item],
      build: List[Item] => Either[Any, A]
  ): Shrink[A] =
    Shrink { value =>
      val elems = toIterable(value).toList
      if (elems.isEmpty) Stream.empty
      else
        // Try removing elements (halving strategy), then try shrinking individual elements
        (removeChunks(elems) #::: shrinkOne(elems, elemShrink)).flatMap { candidate =>
          ScalaCheckUtils.safeBuild(build, candidate) match {
            case Right(shrunk) => Stream(shrunk)
            case Left(_)       => Stream.empty
          }
        }
    }

  /** Shrinks a map entry by shrinking its key, then its value. Pair access is provided by the macro, so this works for
    * any pair representation (not only `Tuple2`).
    */
  def shrinkPair[Pair, K, V](
      keyShrink: Shrink[K],
      valueShrink: Shrink[V],
      key: Pair => K,
      value: Pair => V,
      pair: (K, V) => Pair
  ): Shrink[Pair] =
    Shrink { p =>
      val k = key(p)
      val v = value(p)
      keyShrink.shrink(k).map(pair(_, v)) #::: valueShrink.shrink(v).map(pair(k, _))
    }

  /** Shrinks a case class by shrinking one field at a time. */
  def shrinkCaseClass[A](
      fieldShrinks: List[Shrink[Any]],
      extract: A => Array[Any],
      reconstruct: Array[Any] => A
  ): Shrink[A] =
    Shrink { value =>
      // Guard: a Shrink of the wrong case must not crash.
      // On Scala Native, calling productElement beyond a Product's arity causes SIGSEGV
      // (not a catchable exception). Check arity matches before extracting.
      val product = value.asInstanceOf[Product]
      if (product.productArity < fieldShrinks.size) Stream.empty
      else {
        val fields = extract(value)
        fieldShrinks.zipWithIndex.toStream.flatMap { case (shrink, idx) =>
          shrink.shrink(fields(idx)).map { shrunkField =>
            val newFields = fields.clone()
            newFields(idx) = shrunkField
            reconstruct(newFields)
          }
        }
      }
    }

  /** Shrinks an enum/sealed trait value with the Shrink of its case, selected by the macro-generated `ordinal` (a real
    * pattern match). Unlike trying every case's Shrink, a value is never shrunk by another case's instance (which could
    * turn e.g. `B(1)` into a same-arity `C(0)`).
    */
  def shrinkEnumByOrdinal[A](caseShrinks: List[Shrink[A]], ordinal: A => Int): Shrink[A] = {
    lazy val shrinks = caseShrinks.toArray
    Shrink { value =>
      val index = ordinal(value)
      if (index < 0 || index >= shrinks.length) Stream.empty
      else shrinks(index).shrink(value)
    }
  }

  // --- Internal helpers ---

  /** Remove chunks of elements from a list - ScalaCheck's own halving strategy (`Shrink.shrinkContainer`), so derived
    * shrinkers produce the same candidates, in the same order, as ScalaCheck's instances.
    */
  private def removeChunks[A](xs: List[A]): Stream[List[A]] = removeChunks(xs.length, xs.toStream).map(_.toList)

  private def removeChunks[A](n: Int, xs: Stream[A]): Stream[Stream[A]] =
    if (xs.isEmpty) Stream.empty
    else if (xs.tail.isEmpty) Stream(Stream.empty)
    else {
      val n1 = n / 2
      val n2 = n - n1
      lazy val xs1 = xs.take(n1)
      lazy val xs2 = xs.drop(n1)
      lazy val xs3 = for (ys1 <- removeChunks(n1, xs1) if ys1.nonEmpty) yield ys1 #::: xs2
      lazy val xs4 = for (ys2 <- removeChunks(n2, xs2) if ys2.nonEmpty) yield xs1 #::: ys2
      xs1 #:: xs2 #:: interleave(xs3, xs4)
    }

  private def interleave[A](xs: Stream[A], ys: Stream[A]): Stream[A] =
    if (xs.isEmpty) ys
    else if (ys.isEmpty) xs
    else xs.head #:: ys.head #:: interleave(xs.tail, ys.tail)

  /** Shrink one element at a time in a list. */
  private def shrinkOne[A](xs: List[A], shrink: Shrink[A]): Stream[List[A]] = xs match {
    case Nil          => Stream.empty
    case head :: tail =>
      shrink.shrink(head).map(_ :: tail) #::: shrinkOne(tail, shrink).map(head :: _)
  }

  /** Lazy Shrink - defers (and memoizes) evaluation of the underlying Shrink until the first `shrink` call. Breaks
    * infinite recursion for recursive types, whose cached shrinker would otherwise be built while building itself.
    */
  def shrinkLazy[A](shrink: => Shrink[A]): Shrink[A] = {
    lazy val underlying = shrink
    Shrink(value => underlying.shrink(value))
  }

  /** Shrinks a value by unwrapping, shrinking the inner value, and re-wrapping. */
  def shrinkMapped(innerShrink: Shrink[Any], unwrap: Any => Any, wrap: Any => Any): Shrink[Any] =
    Shrink { value =>
      innerShrink.shrink(unwrap(value)).map(wrap)
    }
}
