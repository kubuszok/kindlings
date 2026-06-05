package hearth.kindlings.catstaglessderivation
package internal.compiletime

import hearth.MacroCommonsScala2

private[compiletime] trait TaglessKBridgeScala2 { this: MacroCommonsScala2 & InvariantKMacrosImpl =>

  protected def constructTypeClassTypeForField(
      fieldType: Type[Any],
      typeClassAnchorType: Type[Any]
  ): Option[NestedFieldInfo] = {
    val tpe = fieldType.asInstanceOf[c.WeakTypeTag[Any]].tpe
    val ctor = tpe.typeConstructor
    if (ctor == tpe) return None
    val anchorTpe = typeClassAnchorType.asInstanceOf[c.WeakTypeTag[Any]].tpe
    val typeClassCtor = anchorTpe.typeConstructor
    val typeClassType = c.universe.appliedType(typeClassCtor, List(ctor))
    Some(
      NestedFieldInfo(
        typeClassType = c.WeakTypeTag[Any](typeClassType).asInstanceOf[Type[Any]],
        ctorK1Untyped = ctor.asInstanceOf[UntypedType]
      )
    )
  }
}
