package hearth.kindlings.ubjsonderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[ubjsonderivation] class CodecMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AnnotationSupport
    with CodecMacrosImpl {

  /** Hearth's `semiEval` reads `var`s by reflection, from classes compiled earlier (e.g. by a previous incremental
    * run): the value it sees is not the runtime value. Configs referencing a `var` are only known at runtime (as
    * jsoniter-derivation's `CodecMacros`).
    */
  override protected def semiEvalConfig(configExpr: Expr[UBJsonConfig]): Option[UBJsonConfig] =
    boundConfig match {
      case Some((ident, original)) if configExpr.tree eq ident => semiEvalConfigTree(original)
      case _                                                   => semiEvalConfigTree(configExpr)
    }

  @scala.annotation.nowarn("cat=deprecation")
  private def semiEvalConfigTree(configExpr: Expr[UBJsonConfig]): Option[UBJsonConfig] = {
    import c.universe.*
    val readsVar = configExpr.tree.exists { t =>
      val sym = t.symbol
      sym != null && sym != NoSymbol && sym.isTerm && {
        val term = sym.asTerm
        term.isVar || (term.isMethod && term.asMethod.isGetter && term.asMethod.setter != NoSymbol)
      }
    }
    // Members of classes/objects of the module being compiled are never read by reflection: compiled in this run (they
    // have source positions) they cannot be, compiled by an earlier incremental run (their class files are in this
    // compilation's output directory) they could. Refusing both makes clean and incremental builds agree (see
    // docs/research/hearth-gap-semieval-local-vals-and-vars.md).
    val readsCurrentModuleMember = configExpr.tree.exists { t =>
      val sym = t.symbol
      sym != null && sym != NoSymbol && sym.isTerm && sym.owner.isClass &&
      (sym.pos != NoPosition || isInOutputDirectory(sym.associatedFile))
    }
    if (readsVar || readsCurrentModuleMember) None else super.semiEvalConfig(configExpr)
  }

  /** The output directories of this compilation (`Global.settings.outputDirs`, read reflectively: the macro API does
    * not expose them). Classes found there were compiled from this module's sources by an earlier run.
    */
  private lazy val outputDirectories: List[String] = scala.util
    .Try {
      val global: AnyRef = c.universe
      val settings = global.getClass.getMethod("settings").invoke(global)
      val outputDirs = settings.getClass.getMethod("outputDirs").invoke(settings)
      val single = outputDirs.getClass.getMethod("getSingleOutput").invoke(outputDirs) match {
        case Some(dir: scala.reflect.io.AbstractFile) => List(dir)
        case _                                        => Nil
      }
      val multiple = outputDirs.getClass.getMethod("outputs").invoke(outputDirs) match {
        case outputs: List[?] => outputs.collect { case (_, dir: scala.reflect.io.AbstractFile) => dir }
        case _                => Nil
      }
      (single ++ multiple).map(dir => canonicalPath(dir.path)).distinct
    }
    .getOrElse(Nil)

  private def canonicalPath(path: String): String =
    scala.util.Try(new java.io.File(path).getCanonicalPath).getOrElse(path)

  private def isInOutputDirectory(file: scala.reflect.io.AbstractFile): Boolean =
    file != null && file.path.endsWith(".class") && {
      val path = canonicalPath(file.path)
      outputDirectories.exists(dir => path.startsWith(dir + java.io.File.separator))
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
