package hearth.kindlings.dicats
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*
import cats.effect.kernel.Resource

final private[dicats] class ResourceWiringMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      ResourceWiringMacrosImpl {

  /** Create Type.Ctor1[G] — the cross-quotes plugin rewrites Type.Ctor1.of[G] here because MacroCommons is in scope. */
  def mkCtor1[G[_]](using scala.quoted.Type[G]): Type.Ctor1[G] = Type.Ctor1.of[G]

  /** Split the (already inlined) varargs literal into the individual dependency expressions. */
  def splitDeps(dependencies: Expr[Seq[Any]]): List[Expr_??] = dependencies match {
    case Varargs(exprs) => exprs.toList.map(preciseDependency)
    case _ => Environment.reportErrorAndAbort("wireResource expects a literal varargs list of dependencies.")
  }
}

private[dicats] object ResourceWiringMacros {

  def wireResourceImpl[F[_]: Type, T: Type](
      dependencies: Expr[Seq[Any]]
  )(using q: Quotes): Expr[Resource[F, T]] = {
    val m = new ResourceWiringMacros(q)
    // `summon[scala.quoted.Type[T]]` is exactly `m.Type[T]`, `m.mkCtor1[F]` supplies the F type constructor.
    m.wireResource[F, T](m.splitDeps(dependencies))(summon[Type[T]], m.mkCtor1[F])
  }
}
