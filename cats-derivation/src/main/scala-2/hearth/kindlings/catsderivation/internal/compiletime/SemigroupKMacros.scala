package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class SemigroupKMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with SemigroupKMacrosImpl {

  def deriveSemigroupKImpl[F[_]](implicit ft: c.WeakTypeTag[F[Any]]): c.Expr[cats.SemigroupK[F]] = {
    val untypedF: UntypedType = ft.tpe.typeConstructor
    val fCtor: Type.Ctor1[F] = Type.Ctor1.fromUntyped[F](untypedF)

    val semigroupKCtor = c.universe.weakTypeOf[cats.SemigroupK[Any]].typeConstructor
    val semigroupKFTpe = c.universe.appliedType(semigroupKCtor, List(ft.tpe.typeConstructor))
    val semigroupKFType =
      c.WeakTypeTag[cats.SemigroupK[F]](semigroupKFTpe).asInstanceOf[Type[cats.SemigroupK[F]]]

    deriveSemigroupK[F](fCtor, semigroupKFType)
  }
}
