package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class ShowPrettyMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      ShowPrettyMacrosImpl,
      AnnotationSupportScala3
private[catsderivation] object ShowPrettyMacros {

  def deriveShowPrettyImpl[A: Type](using q: Quotes): Expr[hearth.kindlings.catsderivation.ShowPretty[A]] =
    new ShowPrettyMacros(q).deriveShowPretty[A]
}
