package hearth.kindlings.catsintegration.internal.runtime

/** Runtime helpers for cats data type conversions.
  *
  * These exist to avoid referencing cats Newtype type aliases (`NonEmptyChain`, `NonEmptyMap`, `NonEmptySet`) inside
  * cross-quotes (`Expr.quote`), which fails on Scala 2 with "not found: value data" due to reification issues with
  * `cats.data.*Impl.Type` aliases.
  *
  * The macro-generated code calls these helpers instead, keeping the quote bodies free of problematic type references.
  */
object CatsConversions {

  def nonEmptyChainToIterable[A](a: Any): Iterable[A] =
    a.asInstanceOf[cats.data.NonEmptyChain[A]].toChain.toList

  def buildNonEmptyChain(list: List[Any]): Either[String, Any] =
    if (list.nonEmpty) {
      val chain = cats.data.Chain.fromSeq(list)
      Right(cats.data.NonEmptyChain.fromChainUnsafe(chain))
    } else Left("Cannot create NonEmptyChain from empty collection")

  def nonEmptyMapToIterable(a: Any): Iterable[(Any, Any)] =
    a.asInstanceOf[cats.data.NonEmptyMap[Any, Any]].toSortedMap.toList

  def buildNonEmptyMap(pairs: List[(Any, Any)], ordering: Ordering[Any]): Either[String, Any] =
    pairs match {
      case head :: tail =>
        implicit val ord: Ordering[Any] = ordering
        val sortedMap = scala.collection.immutable.SortedMap.from(head :: tail)
        Right(cats.data.NonEmptyMap.fromMapUnsafe(sortedMap))
      case Nil =>
        Left("Cannot create NonEmptyMap from empty collection")
    }

  def nonEmptySetToIterable[A](a: Any): Iterable[A] =
    a.asInstanceOf[cats.data.NonEmptySet[A]].toSortedSet.toList

  def buildNonEmptySet(list: List[Any], ordering: Ordering[Any]): Either[String, Any] =
    list match {
      case head :: tail =>
        implicit val ord: Ordering[Any] = ordering
        val sortedSet = scala.collection.immutable.SortedSet.from(head :: tail)
        Right(cats.data.NonEmptySet.fromSetUnsafe(sortedSet))
      case Nil =>
        Left("Cannot create NonEmptySet from empty collection")
    }
}
