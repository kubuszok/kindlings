package hearth.kindlings.mock
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[mock] class MockMacros(q: Quotes) extends MacroCommonsScala3(using q), MockMacrosImpl

private[mock] object MockMacros {

  def mockImpl[A: Type](ctx: Expr[MockContext])(using q: Quotes): Expr[A] = new MockMacros(q).mockType[A](ctx)

  def stubImpl[A: Type](ctx: Expr[MockContext])(using q: Quotes): Expr[A] = new MockMacros(q).stubType[A](ctx)

  def expectsImpl(f: Expr[Any], args: Expr[Seq[Any]])(using q: Quotes): Expr[CallHandler] =
    new MockMacros(q).expectsType(f, args)

  def whenImpl(f: Expr[Any], args: Expr[Seq[Any]])(using q: Quotes): Expr[CallHandler] =
    new MockMacros(q).whenType(f, args)

  def verifyImpl(f: Expr[Any], args: Expr[Seq[Any]])(using q: Quotes): Expr[VerifyTarget] =
    new MockMacros(q).verifyType(f, args)
}
