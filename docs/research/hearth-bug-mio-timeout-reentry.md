# Hearth bug: "MIO timeout is already set" when the Scala 2 compiler nests two independent expansions

**Status:** FIXED in Hearth 985e5e6 (next Hearth release after 0.4.5; Kindlings uses snapshot `0.4.5-16-g985e5e6-SNAPSHOT`). Was OPEN (Hearth 0.4.5, Scala 2.13 only). Found 2026-11-18 while writing the Kindlings usage skills (reproduced
with circe-derivation and sconfig-derivation; any Hearth macro that reads the children of a sealed type is affected).

## Symptom

```scala
case class Http(port: Int)
case class Root(http: Http, level: Level)
sealed trait Level
case object Debug extends Level

object Root  { implicit val enc: Encoder[Root] = KindlingsEncoder.derived[Root] }
object Other { val lr = KindlingsEncoder.derived[Http] } // no explicit type
```

fails on Scala 2.13 with

```
exception during macro expansion:
hearth.HearthAssertionError: Hearth assertion failed:
  MIO timeout is already set — you are re-entering `runToExprOrFail`/`withMioTimeout` while one is already running.
  This is a MISUSE of the API, not a bug: ...
	at hearth.Environments$EnvironmentModule.withMioTimeout(Environments.scala:318)
	at hearth.MIOIntegrations$MioExprOps.runToExprOrFail(MIOIntegrations.scala:65)
```

reported at `KindlingsEncoder.derived[Http]`. Writing the type (`val lr: Encoder[Http] = ...`) compiles.

## Cause

Not a misuse by the macro: the second `runToExprOrFail` belongs to a different, independent macro expansion that the
**compiler** starts while the first one is running. Deriving `Root` reads the children of the sealed `Level`
(`Enum.parse` → `knownDirectSubclasses`). To find them, scalac completes the members of the enclosing package, which
includes inferring the type of `Other.lr`, which typechecks its right-hand side, which expands
`KindlingsEncoder.derived[Http]` - inside the first expansion's `withMioTimeout`. Hearth's global
`MIO.timeoutDeadlineNanos` is still set, so the nested (independent) expansion trips the assertion.

The macro cannot avoid it: neither expansion calls the other, and the macro author cannot prevent scalac from completing
other symbols while answering `knownDirectSubclasses`.

## Minimal standalone reproducer (Hearth only, Scala 2.13.18, Hearth 0.4.5)

`macro/Repro.scala`:

```scala
//> using scala 2.13.18
//> using dep com.kubuszok::hearth:0.4.5
//> using options -Xsource:3
package repro

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import hearth.fp.effect.*

object Repro {

  /** Names of the direct children of a sealed `A` (or "-" when `A` is not sealed). */
  def children[A]: String = macro ReproMacros.childrenImpl[A]
}

class ReproMacros(val c: blackbox.Context) extends hearth.MacroCommonsScala2 {

  def childrenImpl[A: c.WeakTypeTag]: c.Expr[String] =
    MIO {
      Enum.parse[A].toOption match {
        case Some(e) => Expr(e.directChildren.keys.mkString(","))
        case None    => Expr("-")
      }
    }.runToExprOrFail("Repro.children") { (_, errors) =>
      errors.toVector.map(_.getMessage).mkString("\n")
    }
}
```

`usage/Usage.scala`:

```scala
//> using scala 2.13.18
//> using options -Xsource:3
package usage

case class Http(port: Int)
sealed trait Level
case object Debug extends Level

object Root { val levels: String = repro.Repro.children[Level] }
object Other { val http = repro.Repro.children[Http] } // `val http: String = ...` compiles

object Main extends App { println((Root.levels, Other.http)) }
```

```bash
scala-cli --power package macro --library -o macro.jar -f
scala-cli run usage --jar macro.jar --dep com.kubuszok::hearth:0.4.5
# [error] ./usage/Usage.scala:10:27
# [error] exception during macro expansion:
# [error] hearth.HearthAssertionError: Hearth assertion failed:
# [error]   MIO timeout is already set — you are re-entering `runToExprOrFail`/`withMioTimeout` ...
```

## Suggested fix (Hearth)

Make `withMioTimeout` (and the other global MIO run state: `_openScopes`, `macroStartTimestamp`, `benchmarkScopes`,
`disableLogging`) save and restore the previous values instead of asserting, so that an independent expansion nested by
the compiler runs with its own deadline and state, and the outer one continues with its own afterwards. Telling a real
misuse (a macro calling `runToExprOrFail` inside its own run) from a compiler-nested expansion is possible by recording
`c.macroApplication` (Scala 2) / the expansion position when the deadline is set; only the same expansion re-entering is
a misuse.

## Workaround for users

Give the `val`/`def` holding a derived instance an explicit type (`val lr: Encoder[Http] = ...`). Documented in the
FAQ.

## Kindlings test

`circe-derivation/src/test/scala/hearth/kindlings/circederivation/NestedExpansionSpec.scala` keeps the scenario in the
shared tests with the explicit type (the only form that compiles); the failing form is in a comment there, because a
test that does not compile would break every other test of the module. When Hearth is fixed, drop the type ascription
in that test.
