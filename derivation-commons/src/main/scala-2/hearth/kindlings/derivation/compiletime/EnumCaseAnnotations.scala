package hearth.kindlings.derivation.compiletime

import hearth.MacroCommons

/** The annotations of a Scala 3 enum case singleton type: always empty on Scala 2 (see the Scala 3 version). */
object EnumCaseAnnotations {

  def of(ctx: MacroCommons)(tpe: ctx.UntypedType): List[(ctx.UntypedExpr, ctx.UntypedType)] = {
    val _ = tpe
    Nil
  }
}
