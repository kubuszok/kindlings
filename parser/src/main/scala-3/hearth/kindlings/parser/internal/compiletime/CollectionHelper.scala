package hearth.kindlings.parser
package internal.compiletime

import hearth.MacroCommonsScala3
import hearth.std.StdExtensions

import scala.quoted.*

/** Hearth's macro API for [[GrammarMacros]]: grammar extraction and collection code (kept apart from the
  * raw-compiler-API bridge, whose names it would shadow).
  */
final private[parser] class CollectionHelper(q: Quotes)
    extends MacroCommonsScala3(using q),
      StdExtensions,
      CollectionCodegen,
      GrammarExtractor
