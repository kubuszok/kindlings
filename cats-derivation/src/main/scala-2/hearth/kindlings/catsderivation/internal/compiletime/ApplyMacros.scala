package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class ApplyMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with ApplyMacrosImpl {

  def deriveApplyImpl[F[_]](implicit ft: c.WeakTypeTag[F[Any]]): c.Expr[cats.Apply[F]] = {
    val untypedF: UntypedType = ft.tpe.typeConstructor
    val fCtor: Type.Ctor1[F] = Type.Ctor1.fromUntyped[F](untypedF)

    val applyCtor = c.universe.weakTypeOf[cats.Apply[Any]].typeConstructor
    val applyFTpe = c.universe.appliedType(applyCtor, List(ft.tpe.typeConstructor))
    val applyFType = c.WeakTypeTag[cats.Apply[F]](applyFTpe).asInstanceOf[Type[cats.Apply[F]]]

    deriveApply[F](fCtor, applyFType)
  }
}
