package hearth.kindlings.ubjsonderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[ubjsonderivation] class CodecMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AnnotationSupport
    with CodecMacrosImpl {

  /** A config bound by [[withConfigBound]] is evaluated from its original expression. (`semiEval` refuses `var`s and
    * members of the module being compiled itself.)
    */
  override protected def semiEvalConfig(configExpr: Expr[UBJsonConfig]): Option[UBJsonConfig] =
    boundConfig match {
      case Some((ident, original)) if configExpr.tree eq ident => super.semiEvalConfig(original)
      case _                                                   => super.semiEvalConfig(configExpr)
    }

  /** The config bound by [[withConfigBound]]: the reference to its parameter and the original config expression. */
  private var boundConfig: Option[(c.universe.Tree, c.Expr[UBJsonConfig])] = None

  /** A config argument which defines code (a partial-function literal, a lambda) would have its class definitions
    * duplicated by every place of the generated code that reads the config (`double definition` errors). Such a config
    * is evaluated once, as the argument of a function whose body is the generated code:
    * `((config$1: UBJsonConfig) => <generated code>).apply(<config>)`. The config argument stays where it was typed, so
    * its definitions keep their owners. The compile-time evaluation of the config still sees the original expression.
    */
  private def withConfigBound[Out](config: c.Expr[UBJsonConfig])(
      derive: c.Expr[UBJsonConfig] => c.Expr[Out]
  ): c.Expr[Out] = {
    import c.universe.*
    val definesCode = config.tree.exists {
      case _: DefTree | _: Function => true
      case _                        => false
    }
    if (!definesCode) derive(config)
    else {
      val name = TermName(c.freshName("ubjsonConfig"))
      val ident = Ident(name)
      boundConfig = Some((ident, config))
      val out = derive(c.Expr[UBJsonConfig](ident))
      val param = ValDef(Modifiers(Flag.PARAM), name, TypeTree(config.actualType.widen), EmptyTree)
      c.Expr[Out](Apply(Select(Function(List(param), out.tree), TermName("apply")), List(config.tree)))
    }
  }

  def deriveCodecImpl[A: c.WeakTypeTag](
      config: c.Expr[UBJsonConfig]
  ): c.Expr[UBJsonValueCodec[A]] = withConfigBound(config)(deriveCodecTypeClass[A](_))
}
