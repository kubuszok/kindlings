package hearth.kindlings

/** Bare, macwire-style entry point: `import hearth.kindlings.dicats.*` then call `wireResource[F, T](deps*)` directly
  * (in addition to the namespaced [[hearth.kindlings.dicats.DICats]] object, which exposes the very same macro).
  */
package object dicats extends DICatsCompanionCompat
