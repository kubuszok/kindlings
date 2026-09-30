package hearth.kindlings.newtypeintegration.internal.compiletime

import hearth.{MacroCommons, MacroCommonsScala2}

/** Scala 2 structural matcher for the `@newtype` expansion: see [[IsValueTypeProviderForNewtype]]. */
private[compiletime] object NewtypeReprPlatform {

  /** If `tpe` is `Foo.Type[Args]` of a `@newtype`/`@newsubtype` companion `Foo`, returns `Foo.Repr[Args]`. */
  def reprOf(ctx: MacroCommons)(tpe: ctx.UntypedType): Option[ctx.UntypedType] = ctx match {
    case ctx2: MacroCommonsScala2 =>
      import ctx2.c.universe.*

      tpe.asInstanceOf[Type].dealias match {
        case TypeRef(prefix, sym, args)
            if sym.name == TypeName("Type") && sym.isType && sym.asType.isAbstract && prefix.termSymbol.isModule =>
          val repr = prefix.member(TypeName("Repr"))
          val base = prefix.member(TypeName("Base"))
          val tag = prefix.member(TypeName("Tag"))
          if (repr == NoSymbol || base == NoSymbol || tag == NoSymbol || !tag.isClass) None
          else Some(internal.typeRef(prefix, repr, args).dealias.asInstanceOf[ctx.UntypedType])
        case _ => None
      }
    case _ => None
  }
}
