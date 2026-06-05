package hearth.kindlings.catstaglessderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

private[compiletime] trait TaglessKBridgeScala3 { this: MacroCommonsScala3 & InvariantKMacrosImpl =>

  protected val q: Quotes // override with the constructor param in concrete classes

  protected def constructTypeClassTypeForField(
      fieldType: Type[Any],
      typeClassAnchorType: Type[Any]
  ): Option[NestedFieldInfo] = {
    import q.reflect.*
    val repr = TypeRepr.of(using fieldType.asInstanceOf[scala.quoted.Type[Any]])
    repr.dealias match {
      case AppliedType(fieldHKTCtor, _ :: Nil) =>
        val anchorRepr = TypeRepr.of(using typeClassAnchorType.asInstanceOf[scala.quoted.Type[Any]])
        anchorRepr match {
          case AppliedType(typeClassCtor, _) =>
            val typeClassType = typeClassCtor.appliedTo(fieldHKTCtor)
            Some(
              NestedFieldInfo(
                typeClassType = typeClassType.asType.asInstanceOf[Type[Any]],
                ctorK1Untyped = fieldHKTCtor.asInstanceOf[UntypedType]
              )
            )
          case _ => None
        }
      case _ => None
    }
  }
}
