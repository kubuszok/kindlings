package hearth.kindlings.derivation.compiletime

/** The constants of a Java-defined enum, in declaration order, completed so that the compiler registers them as the
  * enum's children.
  *
  * Scala 3 registers a constant of a Java enum read from a class file as a child of the enum only when the constant's
  * info is completed (`ClassfileParser` adds the `@Child` annotation then), so:
  *   - an enum none of whose constants is referred to by the code being compiled has no children at all (Hearth's
  *     `Enum.parse` fails: "is not sealed, not an enumeration, and not a union type"),
  *   - the children of an enum some of whose constants are referred to come in the order in which the compiler happened
  *     to complete them (the referred ones first), not in declaration order.
  *
  * See docs/research/hearth-bug-java-enum-unreferenced-constants.md.
  */
private[compiletime] object JavaEnumConstants {

  def apply(commons: hearth.MacroCommons)(tpe: commons.UntypedType): Option[List[String]] = commons match {
    case scala3: hearth.MacroCommonsScala3 =>
      import scala3.quotes.reflect.*
      given scala.quoted.Quotes = scala3.quotes
      val repr = tpe.asInstanceOf[TypeRepr]
      val symbol = repr.typeSymbol
      if !symbol.isNoSymbol && symbol.isClassDef && symbol.flags.is(Flags.JavaDefined) &&
      symbol.flags.is(Flags.Enum) && repr <:< TypeRepr.of[java.lang.Enum[?]]
      then {
        // the constants are static fields: members of the companion module
        val module = symbol.companionModule
        if module.isTerm then {
          val fields = module.declaredFields
          fields.foreach { field =>
            val _ = module.termRef.memberType(field)
          }
          Some(fields.filter(_.flags.is(Flags.Enum)).map(_.name))
        } else None
      } else None
    case _ => None
  }
}
