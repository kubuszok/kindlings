package hearth.kindlings.dicats
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox
import cats.effect.kernel.Resource

final private[dicats] class ResourceWiringMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with ResourceWiringMacrosImpl {

  def wireResourceImpl[F[_], T](dependencies: c.Tree*)(implicit
      ft: c.WeakTypeTag[F[Any]],
      tt: c.WeakTypeTag[T]
  ): c.Expr[Resource[F, T]] = {
    // Recover the `F` type constructor from the `F[Any]` WeakTypeTag (HKT type params are not directly tagged on 2.13).
    val fCtor: Type.Ctor1[F] = Type.Ctor1.fromUntyped[F](ft.tpe.typeConstructor)
    // `UntypedExpr.as_??(tree)` reads each dependency's PRECISE static type (e.g. `Resource[F, X]`, `Config`), unlike
    // `c.Expr[Any](tree)` which would erase it to `Any` and break classification/resolution.
    // On Scala 2 `Type[A] = c.WeakTypeTag[A]`, so `tt` already is the `Type[T]` we need.
    wireResource[F, T](dependencies.toList.map(t => UntypedExpr.as_??(t)))(tt, fCtor)
  }
}
