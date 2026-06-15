package hearth.kindlings.di

import scala.annotation.StaticAnnotation

/** Marks a type as a wiring '''module''': when a value whose type is annotated with `@Module` (or inherits such a type)
  * is in scope, `wire`/`wireSet`/`wireList` may also draw on that value's own public, parameterless members as
  * candidates — exactly like macwire's `com.softwaremill.macwire.Module`.
  *
  * {{{
  * @Module
  * class DataModule {
  *   lazy val databaseAccess: DatabaseAccess = new DatabaseAccess
  * }
  *
  * class AppModule(dataModule: DataModule) {
  *   // `databaseAccess` is pulled from `dataModule` because `DataModule` is a `@Module`.
  *   lazy val userFinder: UserFinder = DI.wire[UserFinder]
  * }
  * }}}
  */
final class Module extends StaticAnnotation
