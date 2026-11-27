package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2

/** Scala 2 entry-point glue for `F[_]` given as a type alias (`type OptList[A] = Option[List[A]]`,
  * `type AndChar[A] = (A, Char)`).
  *
  * `Type.Ctor1.of[F]` takes the type constructor of the dealiased `F[Any]` - `Option` and `Tuple2` for the aliases
  * above, i.e. not `F` (and, for `Tuple2`, not even a unary type constructor). When the type argument of the macro
  * application is a type alias, the alias itself is used as the type constructor instead, so that `F[X]` is
  * `OptList[X]` for every `X`.
  */
private[compiletime] trait AliasAwareCtor1 { this: MacroCommonsScala2 & ErasedHktSupport =>

  protected def aliasAwareCtor1[F[_]](tag: c.WeakTypeTag[F[Any]])(ctorOfDealiased: => Type.Ctor1[F]): Type.Ctor1[F] = {
    import c.universe.{TypeApply, TypeApplyTag, TypeRef, TypeRefTag}
    val _ = tag
    val alias = c.macroApplication match {
      case TypeApply(_, List(arg)) =>
        Option(arg.tpe).collect {
          case tpe @ TypeRef(_, sym, Nil) if sym.isType && sym.asType.isAliasType && tpe.typeParams.size == 1 => tpe
        }
      case _ => None
    }
    alias match {
      case Some(tpe) =>
        // Code quoting `F` would name the dealiased type constructor: only the general derivation (which does not).
        typeConstructorIsAlias = true
        Type.Ctor1.fromUntyped[F](tpe.asInstanceOf[UntypedType])
      case None => ctorOfDealiased
    }
  }
}
