package hearth.kindlings.mock
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[mock] class MockMacros(val c: blackbox.Context) extends MacroCommonsScala2 with MockMacrosImpl {

  def mockImpl[A: c.WeakTypeTag](ctx: c.Expr[MockContext]): c.Expr[A] = mockType[A](ctx)

  def stubImpl[A: c.WeakTypeTag](ctx: c.Expr[MockContext]): c.Expr[A] = stubType[A](ctx)

  /** The eta-expanded method reference `f` is the single argument of the `syntax.MockFunctionOps` implicit-class
    * application; recover it from `c.prefix`, alongside the `args` packed into a `Seq[Any]`.
    */
  private def dslOperands(dsl: String, args: Seq[c.Expr[Any]]): (c.Expr[Any], c.Expr[Seq[Any]]) = {
    import c.universe.*
    val fTree: Tree = c.prefix.tree match {
      case Apply(_, List(inner)) => inner
      case other                 =>
        c.abort(c.enclosingPosition, s"`.$dsl` could not extract the method reference from: ${showRaw(other)}")
    }
    (c.Expr[Any](fTree), c.Expr[Seq[Any]](q"_root_.scala.Seq(..${args.map(_.tree)})"))
  }

  def expectsImpl(args: c.Expr[Any]*): c.Expr[CallHandler] = {
    val (f, argsSeq) = dslOperands("expects", args)
    expectsType(f, argsSeq)
  }

  def whenImpl(args: c.Expr[Any]*): c.Expr[CallHandler] = {
    val (f, argsSeq) = dslOperands("when", args)
    whenType(f, argsSeq)
  }

  def verifyImpl(args: c.Expr[Any]*): c.Expr[VerifyTarget] = {
    val (f, argsSeq) = dslOperands("verify", args)
    verifyType(f, argsSeq)
  }
}
