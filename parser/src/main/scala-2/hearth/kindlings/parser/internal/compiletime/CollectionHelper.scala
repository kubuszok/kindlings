package hearth.kindlings.parser
package internal.compiletime

import hearth.MacroCommonsScala2
import hearth.std.StdExtensions

import scala.reflect.macros.blackbox

/** Hearth's macro API for [[GrammarMacros]]: grammar extraction and collection code (kept apart from the
  * raw-compiler-API bridge, whose names it would shadow).
  */
final private[parser] class CollectionHelper(val c: blackbox.Context)
    extends MacroCommonsScala2
    with StdExtensions
    with CollectionCodegen
    with GrammarExtractor
