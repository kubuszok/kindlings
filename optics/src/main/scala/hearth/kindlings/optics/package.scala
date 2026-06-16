package hearth.kindlings

/** Bare, quicklens-style entry point: `import hearth.kindlings.optics.*` then call `obj.modify(_.a.b)` directly (the
  * same DSL also lives under [[hearth.kindlings.optics.syntax]]).
  */
package object optics extends OpticsSyntax
