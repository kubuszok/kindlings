package hearth.kindlings.optics

import _root_.cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyVector}

/** [[QuicklensFunctor]] instances for cats' non-empty (and `Chain`) collections, so kindlings-optics' `.each` step
  * works over them exactly as it does over `List`/`Vector`/`Set`:
  *
  * {{{
  * import hearth.kindlings.optics._
  * import hearth.kindlings.optics.CatsQuicklensFunctors._
  *
  * case class Team(members: NonEmptyList[String])
  * Team(NonEmptyList.of("ann", "bob")).modify(_.members.each).using(_.toUpperCase)
  * }}}
  *
  * The optics macro is container-agnostic: it summons a `QuicklensFunctor[F]` for the focused container constructor `F`
  * and the call-site `.each` evidence ([[IsElementOf]]) is derived from the same given. Providing these instances is
  * therefore all that is needed — there is no macro or compile-time component in this module, only runtime givens.
  * Bring them into scope with `import hearth.kindlings.optics.CatsQuicklensFunctors._`.
  *
  * Note: `NonEmptyMap`/`NonEmptySet` are not covered — the macro's map/value traversal is pinned to
  * `scala.collection.immutable.Map`, and `NonEmptySet` cannot be rebuilt by `map` without an `Order`. Use `.when`/field
  * descent for those.
  */
object CatsQuicklensFunctors {

  implicit val nonEmptyListQuicklensFunctor: QuicklensFunctor[NonEmptyList] =
    new QuicklensFunctor[NonEmptyList] {
      def each[A, B](fa: NonEmptyList[A])(f: A => B): NonEmptyList[B] = fa.map(f)
    }

  implicit val nonEmptyVectorQuicklensFunctor: QuicklensFunctor[NonEmptyVector] =
    new QuicklensFunctor[NonEmptyVector] {
      def each[A, B](fa: NonEmptyVector[A])(f: A => B): NonEmptyVector[B] = fa.map(f)
    }

  implicit val nonEmptyChainQuicklensFunctor: QuicklensFunctor[NonEmptyChain] =
    new QuicklensFunctor[NonEmptyChain] {
      def each[A, B](fa: NonEmptyChain[A])(f: A => B): NonEmptyChain[B] = fa.map(f)
    }

  implicit val chainQuicklensFunctor: QuicklensFunctor[Chain] =
    new QuicklensFunctor[Chain] {
      def each[A, B](fa: Chain[A])(f: A => B): Chain[B] = fa.map(f)
    }
}
