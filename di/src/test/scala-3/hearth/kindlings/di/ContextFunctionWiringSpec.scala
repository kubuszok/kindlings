package hearth.kindlings.di

import hearth.MacroSuite

/** Scala 3 only: a context function (`(A, B) ?=> C`) passed to `autowire` is a factory, its `using` parameters wired
  * from the graph exactly like a plain function's parameters.
  *
  * Only a context function LITERAL reaches the macro as a function value: a reference to a `val f: A ?=> B` (even
  * ascribed) is applied by the compiler to a summoned `A` because the varargs' expected type `Any` is not a context
  * function type.
  */
final class ContextFunctionWiringSpec extends MacroSuite {

  import WiringSpec.*

  group("DI.autowire with context function factories") {

    test("uses a context function literal as a factory") {
      val db = new DatabaseAccess()
      val sf = new SecurityFilter()
      val finder = DI.autowire[UserFinder](
        db,
        sf,
        (d: DatabaseAccess, s: SecurityFilter) ?=> new UserFinder(d, s)
      )
      finder.databaseAccess ==> db
      finder.securityFilter ==> sf
    }
  }
}
