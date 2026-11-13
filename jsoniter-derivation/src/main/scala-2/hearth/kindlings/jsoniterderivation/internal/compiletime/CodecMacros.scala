package hearth.kindlings.jsoniterderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonCodec, JsonKeyCodec, JsonReaderException, JsonValueCodec}
import scala.reflect.macros.blackbox

final private[jsoniterderivation] class CodecMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AnnotationSupport
    with CodecMacrosImpl {

  /** A config bound by [[withConfigBound]] is evaluated from its original expression. (`semiEval` refuses `var`s and
    * members of the module being compiled itself.)
    */
  override protected def semiEvalConfig(configExpr: Expr[JsoniterConfig]): Option[JsoniterConfig] =
    boundConfig match {
      case Some((ident, original)) if configExpr.tree eq ident => super.semiEvalConfig(original)
      case _                                                   => super.semiEvalConfig(configExpr)
    }

  /** The config bound by [[withConfigBound]]: the reference to its parameter and the original config expression. */
  private var boundConfig: Option[(c.universe.Tree, c.Expr[JsoniterConfig])] = None

  /** A config argument which defines code (a partial-function literal, a lambda) would have its class definitions
    * duplicated by every place of the generated code that reads the config (`double definition` errors). Such a config
    * is evaluated once, as the argument of a function whose body is the generated code:
    * `((config$1: JsoniterConfig) => <generated code>).apply(<config>)`. The config argument stays where it was typed,
    * so its definitions keep their owners. The compile-time evaluation of the config still sees the original
    * expression.
    */
  private def withConfigBound[Out](config: c.Expr[JsoniterConfig])(
      derive: c.Expr[JsoniterConfig] => c.Expr[Out]
  ): c.Expr[Out] = {
    import c.universe.*
    val definesCode = config.tree.exists {
      case _: DefTree | _: Function => true
      case _                        => false
    }
    if (!definesCode) derive(config)
    else {
      val name = TermName(c.freshName("jsoniterConfig"))
      val ident = Ident(name)
      boundConfig = Some((ident, config))
      val out = derive(c.Expr[JsoniterConfig](ident))
      val param = ValDef(Modifiers(Flag.PARAM), name, TypeTree(config.actualType.widen), EmptyTree)
      c.Expr[Out](Apply(Select(Function(List(param), out.tree), TermName("apply")), List(config.tree)))
    }
  }

  /** [[withConfigBound]] for the inline entry points: when the config is bound, the value (or JSON) argument is bound
    * with it (it would otherwise move into the function's body, away from the owner it was typed with).
    */
  private def withConfigAndArgBound[V, Out](config: c.Expr[JsoniterConfig], arg: c.Expr[V])(
      derive: (c.Expr[V], c.Expr[JsoniterConfig]) => c.Expr[Out]
  ): c.Expr[Out] = {
    import c.universe.*
    var argName: Option[TermName] = None
    val out = withConfigBound(config) { cfg =>
      if (cfg eq config) derive(arg, cfg)
      else {
        val name = TermName(c.freshName("jsoniterArg"))
        argName = Some(name)
        derive(c.Expr[V](Ident(name)), cfg)
      }
    }
    argName match {
      case None       => out
      case Some(name) =>
        // ((config$1: Config) => body).apply(config)  ~>  ((config$1: Config, arg$1: V) => body).apply(config, arg)
        out.tree match {
          case Apply(Select(Function(List(cfgParam), body), apply), List(cfgArg)) =>
            val argParam = ValDef(Modifiers(Flag.PARAM), name, TypeTree(arg.actualType.widen), EmptyTree)
            c.Expr[Out](Apply(Select(Function(List(cfgParam, argParam), body), apply), List(cfgArg, arg.tree)))
          case other => c.abort(c.enclosingPosition, s"Unexpected bound config tree: $other")
        }
    }
  }

  def deriveCodecImpl[A: c.WeakTypeTag](
      config: c.Expr[JsoniterConfig]
  ): c.Expr[JsonValueCodec[A]] =
    withConfigBound(config)(deriveCodecTypeClass[A](_)).asInstanceOf[c.Expr[JsonValueCodec[A]]]

  def deriveKindlingsCodecImpl[A: c.WeakTypeTag](
      config: c.Expr[JsoniterConfig]
  ): c.Expr[KindlingsJsonValueCodec[A]] = withConfigBound(config)(deriveCodecTypeClass[A](_))

  def deriveInlineWriteToStringImpl[A: c.WeakTypeTag](
      value: c.Expr[A]
  )(config: c.Expr[JsoniterConfig]): c.Expr[String] =
    withConfigAndArgBound(config, value)(deriveInlineWriteToString[A](_, _))

  def deriveInlineReadFromStringImpl[A: c.WeakTypeTag](
      json: c.Expr[String]
  )(config: c.Expr[JsoniterConfig]): c.Expr[Either[JsonReaderException, A]] =
    withConfigAndArgBound(config, json)(deriveInlineReadFromString[A](_, _))

  def deriveJsonCodecImpl[A: c.WeakTypeTag](
      config: c.Expr[JsoniterConfig]
  ): c.Expr[JsonCodec[A]] =
    withConfigBound(config)(deriveJsonCodecTypeClass[A](_)).asInstanceOf[c.Expr[JsonCodec[A]]]

  def deriveKindlingsJsonCodecImpl[A: c.WeakTypeTag](
      config: c.Expr[JsoniterConfig]
  ): c.Expr[KindlingsJsonCodec[A]] =
    withConfigBound(config)(deriveJsonCodecTypeClass[A](_)).asInstanceOf[c.Expr[KindlingsJsonCodec[A]]]

  def deriveKeyCodecImpl[A: c.WeakTypeTag](
      config: c.Expr[JsoniterConfig]
  ): c.Expr[JsonKeyCodec[A]] =
    withConfigBound(config)(deriveKeyCodecTypeClass[A](_)).asInstanceOf[c.Expr[JsonKeyCodec[A]]]

  @scala.annotation.nowarn("msg=unchecked")
  def deriveInlineWriteToStringOpsImpl[A: c.WeakTypeTag](
      config: c.Expr[JsoniterConfig]
  ): c.Expr[String] = {
    val value = c.Expr[A](c.prefix.tree match {
      case c.universe.Apply(_, List(arg)) => arg
      case other                          => c.abort(c.enclosingPosition, s"Unexpected prefix: $other")
    })
    withConfigAndArgBound(config, value)(deriveInlineWriteToString[A](_, _))
  }

  @scala.annotation.nowarn("msg=unchecked")
  def deriveInlineReadFromStringOpsImpl[A: c.WeakTypeTag](
      config: c.Expr[JsoniterConfig]
  ): c.Expr[Either[JsonReaderException, A]] = {
    val json = c.Expr[String](c.prefix.tree match {
      case c.universe.Apply(_, List(arg)) => arg
      case other                          => c.abort(c.enclosingPosition, s"Unexpected prefix: $other")
    })
    withConfigAndArgBound(config, json)(deriveInlineReadFromString[A](_, _))
  }
}
