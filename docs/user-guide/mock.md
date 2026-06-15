# Mocking

A small mocking framework whose API is modelled on [ScalaMock](https://github.com/ScalaMock/ScalaMock), but which is an
**independent, from-scratch reimplementation** — it does **not** depend on (or wrap) ScalaMock. The only dependency is
Hearth, used for the macros.

`Mock.mock` and `Mock.stub` synthesize an anonymous subtype of the type you want to fake at compile time (via Hearth's
`AnonymousInstance`) and route every abstract member into a tiny pure-Scala runtime engine. There is **no reflection and
no bytecode generation**, so — unlike ScalaMock's default JVM backend — the exact same code cross-compiles to **JVM,
Scala.js and Scala Native** on both Scala 2.13 and Scala 3. (Cross-platform support is the main reason this exists
rather than just using ScalaMock.)

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-mock" % "{{ kindlings_version() }}" % Test
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-mock:{{ kindlings_version() }}
    ```

## Quick start

A **mock** is _strict_: you declare expectations up front and the test fails if a call does not match one, or if an
expectation is left unsatisfied. Create the mock with an implicit `MockContext`, then use either DSL to set
expectations:

```scala
//> using dep com.kubuszok::kindlings-mock:{{ kindlings_version() }}

import hearth.kindlings.mock._
import hearth.kindlings.mock.syntax._

trait Greeter {
  def greet(name: String): String
}

implicit val ctx: MockContext = new MockContext
val greeter = Mock.mock[Greeter]

// faithful ScalaMock DSL — eta-expand the method, then `.expects`:
val _ = (greeter.greet _).expects("world").returning("hello, world")

val greeting = greeter.greet("world")
ctx.verifyExpectations()
println(greeting)
// expected output:
// hello, world
```

## Two equivalent expectation DSLs

Kindlings supports **both** the faithful ScalaMock spelling and a name-keyed spelling that needs no macro on the call
site. They register into the same `MockContext`, so you can mix them freely:

```scala
import hearth.kindlings.mock.syntax._

// faithful — `(m.method _).expects(args).returning(value)`
(greeter.greet _).expects("world").returning("hello, world")

// name-keyed — `ctx.expecting("method", args).returning(value)`
ctx.expecting("greet", "world").returning("hello, world")
```

The faithful DSL extracts the method name from the eta-expanded reference and registers against the `MockContext` that
is implicitly in scope (the same one the mock was created with).

## Return values, exceptions and computed results

```scala
ctx.expecting("greet", "world").returning("hi")               // fixed value
ctx.expecting("greet", "boom").throwing(new RuntimeException)  // throw
ctx.expecting("greet").onCall(args => s"hi, ${args.head}")     // compute from the actual args
```

## Call counts

Every expectation carries an expected call-count range. The default is exactly once:

```scala
ctx.expecting("greet", "world").returning("hi").once()
ctx.expecting("greet", "world").returning("hi").twice()
ctx.expecting("greet", "world").returning("hi").never()
ctx.expecting("greet", "world").returning("hi").anyNumberOfTimes()
ctx.expecting("greet", "world").returning("hi").atLeastOnce()
ctx.expecting("greet", "world").returning("hi").repeat(3)
```

`ctx.verifyExpectations()` at the end of the test fails if any expectation's actual count is outside its range.

## Argument matchers

Use an `ArgMatcher` in place of a literal expected value:

```scala
import hearth.kindlings.mock.ArgMatcher

ctx.expecting("greet", ArgMatcher.any).returning("hi")                 // any argument
ctx.expecting("greet", ArgMatcher.where[String](_.nonEmpty))           // predicate
ctx.expecting("greet", ArgMatcher.argThat[String](_.length > 3))       // alias for `where`
ctx.expecting("measure", ArgMatcher.epsilon(1.0, 0.05))                // numeric, within tolerance
```

`Capture` records the argument the expectation actually fired with, for later inspection:

```scala
val name = new Capture[String]
ctx.expecting("greet", ArgMatcher.capture(name)).returning("hi")
greeter.greet("Alice")
println(name.value)   // "Alice"
```

## Ordering

By default expectations match in any order. Wrap them in `inSequence` to require the declared order; `inAnyOrder`
relaxes ordering again for a nested block:

```scala
ctx.inSequence {
  ctx.expecting("open").returning(())
  ctx.inAnyOrder {
    ctx.expecting("write", "a").returning(())
    ctx.expecting("write", "b").returning(())   // these two may arrive in either order
  }
  ctx.expecting("close").returning(())
}
```

A sequenced expectation only fires once every earlier expectation in its sequence is satisfied and no later one has
fired yet; sequences may span several methods.

## Stubs

A **stub** is _lenient_: every method returns a default until you preset it, nothing fails on an unexpected call, and
you verify what happened **after** the fact. Defaults come from the `Defaultable` type class (`0`, `false`, `""`,
`None`, empty collections, …; any other reference type falls back to `null`).

```scala
//> using dep com.kubuszok::kindlings-mock:{{ kindlings_version() }}

import hearth.kindlings.mock._
import hearth.kindlings.mock.syntax._

trait Calculator {
  def add(a: Int, b: Int): Int
}

implicit val ctx: MockContext = new MockContext
val calc = Mock.stub[Calculator]

// preset one behaviour; everything else returns the Int default (0):
val _ = (calc.add _).when(2, 3).returning(5)

val preset = calc.add(2, 3)   // matches the `when`
val default = calc.add(9, 9)  // no preset → Int default

// verify call counts after the fact:
(calc.add _).verify(2, 3).once()
(calc.add _).verify(9, 9).once()

println(preset)
println(default)
// expected output:
// 5
// 0
```

Like `expects`, both `when` and `verify` have name-keyed equivalents (`ctx.when("add", 2, 3)`,
`ctx.verify("add", 2, 3)`).

## How it works

`Mock.mock[A]` / `Mock.stub[A]` use Hearth's `AnonymousInstance` to build `new A { … }`, overriding each abstract
member with a body that packs its arguments into a `Seq[Any]` and forwards them to the runtime `MockContext`. Because
the whole mechanism is ordinary generated Scala — no `java.lang.reflect.Proxy`, no `ByteBuddy` — it runs identically on
every platform Kindlings targets.

!!! note "Cross-platform parity"

    The entire test suite for this module (`mock`/`stub`, both DSLs, ordering, matchers) passes on the JVM **and**
    Scala.js, which is exactly the gap this module closes versus ScalaMock's JVM-only proxy backend.
