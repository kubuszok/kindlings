package hearth.kindlings.di

/** Compile-time dependency injection, in the spirit of [[https://github.com/softwaremill/macwire macwire]], but built
  * on Hearth's macro-agnostic API so the very same implementation works on Scala 2.13 and Scala 3, across the JVM,
  * Scala.js and Scala Native.
  *
  * The wiring macros inspect the '''lexical scope''' surrounding the call site (via Hearth's `enclosingScope`) to find
  * already-constructed dependencies — `val`s/`def`s of the enclosing class/object, and (on Scala 2) `val`s local to the
  * enclosing method — and use them to fill the constructor parameters of the wired type.
  *
  * {{{
  * import hearth.kindlings.di.DI
  *
  * class DatabaseAccess()
  * class SecurityFilter()
  * class UserFinder(databaseAccess: DatabaseAccess, securityFilter: SecurityFilter)
  *
  * class UserModule {
  *   lazy val databaseAccess = new DatabaseAccess()
  *   lazy val securityFilter = new SecurityFilter()
  *   // expands to: new UserFinder(databaseAccess, securityFilter)
  *   lazy val userFinder = DI.wire[UserFinder]
  * }
  * }}}
  */
object DI extends DICompanionCompat {

  /** Marker used only inside [[autowire]]: `DI.autowire[A](DI.autowireMembersOf(instance), ...)` seeds the dependency
    * pool with every public, parameterless member (`val` / no-arg `def`) of `instance`, each matched by its declared
    * type — mirroring macwire's `autowireMembersOf`. Outside `autowire` it is the identity; `autowire` rewrites the
    * call at compile time, so this body is never actually evaluated in that position.
    */
  def autowireMembersOf[T](instance: T): T = instance

  /** Special marker type — if its implicit is in scope, the wiring macros log the generated code and the resolution
    * logic that produced it. Import `hearth.kindlings.di.debug.*` to enable, or set
    * `-Xmacro-settings:di.logGeneration=true`. Kept outside auto-summoned scope.
    */
  sealed trait LogGeneration
  object LogGeneration extends LogGeneration
}
