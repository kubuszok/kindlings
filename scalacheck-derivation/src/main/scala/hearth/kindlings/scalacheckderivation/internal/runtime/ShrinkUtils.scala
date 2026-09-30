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

  /** Shrinks a collection by trying progressively smaller sublists and shrinking individual elements. Uses
    * Iterable[Any] at runtime to avoid higher-kinded type issues in macro-generated code.
    */
  @deprecated(
    "Kept for binary compatibility of code compiled against kindlings 0.3.x; the macro no longer emits it",
    "0.3.3"
  )
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
      // Guard: when used inside shrinkEnum, the wrong case's shrink may be called.
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

  /** Shrinks an enum/sealed trait value by delegating to the Shrink for the actual runtime type. The list of Shrink
    * instances corresponds to the enum cases in declaration order. We try each one (catching ClassCastException) to
    * find the right variant.
    */
  def shrinkEnum[A](caseShrinks: List[Shrink[A]]): Shrink[A] =
    Shrink { value =>
      // Evaluate try/catch eagerly (strict List.map) to avoid try/catch inside lazy
      // Stream.flatMap — that combination overflows Scala Native's stack.
      val results: List[Stream[A]] = caseShrinks.map { caseShrink =>
        try caseShrink.shrink(value)
        catch { case _: Throwable => Stream.empty[A] }
      }
      results.foldRight(Stream.empty[A])(_ #::: _)
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

  /** Shrinks a Map by shrinking its entries as a list of pairs. */
  @deprecated(
    "Kept for binary compatibility of code compiled against kindlings 0.3.x; the macro no longer emits it",
    "0.3.3"
  )
  def shrinkMap(keyShrink: Shrink[Any], valueShrink: Shrink[Any]): Shrink[Any] =
    Shrink { value =>
      val entries = value.asInstanceOf[Map[Any, Any]].toList
      if (entries.isEmpty) Stream.empty
      else {
        val pairShrink: Shrink[(Any, Any)] = Shrink { case (k, v) =>
          keyShrink.shrink(k).map((_, v)) #::: valueShrink.shrink(v).map((k, _))
        }
        val removeStreams = removeChunks(entries).map(_.toMap)
        val shrinkEntryStreams = shrinkOne(entries, pairShrink).map(_.toMap)
        (removeStreams #::: shrinkEntryStreams).asInstanceOf[Stream[Any]]
      }
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
