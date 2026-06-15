package hearth.kindlings.di.usage

import hearth.MacroSuite
import hearth.kindlings.di.*

/** Exercises the macwire-style bare `wire` entry point from a package that is NOT `hearth.kindlings.di`, i.e. through
  * `import hearth.kindlings.di.*` exactly as a downstream user would.
  */
final class BareImportSpec extends MacroSuite {

  group("import hearth.kindlings.di.*") {

    test("bare wire and DI.wire are both available and equivalent") {
      val module = new BareImportSpec.Module
      module.viaBare.databaseAccess ==> module.databaseAccess
      module.viaNamespace.securityFilter ==> module.securityFilter
    }
  }
}

object BareImportSpec {

  class DatabaseAccess()
  class SecurityFilter()
  class UserFinder(val databaseAccess: DatabaseAccess, val securityFilter: SecurityFilter)

  class Module {
    lazy val databaseAccess = new DatabaseAccess()
    lazy val securityFilter = new SecurityFilter()
    lazy val viaBare: UserFinder = wire[UserFinder]
    lazy val viaNamespace: UserFinder = DI.wire[UserFinder]
  }
}
