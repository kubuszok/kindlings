package hearth.kindlings.derivation.compiletime

import hearth.{MacroCommons, MacroCommonsScala3}

/** The annotations of a Scala 3 enum case singleton type (`Color.Red.type`, which Hearth's `directChildren` returns for
  * parameterless enum cases), with their types.
  *
  * Hearth's `Type#annotations` reads the annotations of the type's `typeSymbol`, which for such a `TermRef` is the enum
  * class, while the annotations (e.g. `@named("0") case Red`) are on the case's term symbol. See
  * docs/research/hearth-gap-enum-case-annotations.md.
  */
object EnumCaseAnnotations {

  def of(ctx: MacroCommons)(tpe: ctx.UntypedType): List[(ctx.UntypedExpr, ctx.UntypedType)] = ctx match {
    case MacroCommonsScala3(platform) =>
      import platform.quotes.reflect.*
      val term = tpe.asInstanceOf[TypeRepr].termSymbol
      if term.isNoSymbol || !term.flags.is(Flags.Enum) then Nil
      else term.annotations.map(a => (a.asInstanceOf[ctx.UntypedExpr], a.tpe.asInstanceOf[ctx.UntypedType]))
    case _ => Nil
  }
}
