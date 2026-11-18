package hearth.kindlings.derivation.compiletime

/** The full names of the owners enclosing the current macro expansion, innermost first, computed WITHOUT forcing any
  * type: unlike `Environment.enclosingScope` (which lists the members of the enclosing classes/objects and the types of
  * the local values), it works while the type of the `val` holding the expansion is still being inferred (an untyped
  * `val e = Codec.derived[A]`), where reading those types fails with "illegal cyclic reference".
  */
private[compiletime] object EnclosingOwnerNames {

  private val MaxDepth = 64

  def apply(commons: hearth.MacroCommons): Option[List[String]] = commons match {
    case scala2: hearth.MacroCommonsScala2 =>
      val c = scala2.c
      val owners = Iterator
        .iterate(c.internal.enclosingOwner)(_.owner)
        .takeWhile(_ != c.universe.NoSymbol)
        .take(MaxDepth) // the root owner may be its own owner
        .map(_.fullName)
        .filter(DerivationPolicy.isUserVisibleOwnerName)
        .toList
      Some(owners.distinct)
    case _ => None
  }
}
