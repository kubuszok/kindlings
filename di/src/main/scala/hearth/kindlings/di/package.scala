package hearth.kindlings

/** Bare, macwire-style entry points: `import hearth.kindlings.di.*` then call `wire[A]` directly (in addition to the
  * namespaced [[hearth.kindlings.di.DI]] object, which exposes the very same macros).
  */
package object di extends DICompanionCompat
