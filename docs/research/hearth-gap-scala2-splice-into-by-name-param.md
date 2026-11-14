# Scala 2 cross-quotes: splicing a default-value expression into a by-name parameter

Status: **RESOLVED** in Hearth `0.4.3-90-gc3f9223-SNAPSHOT` (kubuszok/hearth#402, to be released as 0.4.4): Hearth now
builds a default value from the constructor's `<init>$default$N`, and splicing it into a by-name parameter no longer
crashes the Scala 2 backend. `CirceDerivationUtils.defaultingDecoder` takes its default by-name again. See the last
section.

Found while porting circe-generic's `deriveFor[F].incomplete` (`KindlingsDecoder.incomplete`, audit round 3, item 5a).

## Symptom

On Scala 2.13 only, the expansion compiled, but the compiler crashed in the backend:

```
an unexpected type representation reached the compiler backend while compiling CirceParityRound3Spec.scala: <error>.
Error while emitting CirceParityRound3Spec.scala
<error> (of class scala.reflect.internal.Types$ErrorType$)
```

The generated code was

```scala
Expr.quote {
  CirceDerivationUtils.defaultingDecoder(
    Expr.splice(decoderExpr),
    Expr.splice(default), // the field's default value: foldInstanceFree(param.defaultValue.get, ...).value.upcast[Any]
    Expr.splice(configVal).useDefaults
  )
}
```

with `def defaultingDecoder[A](decoder: Decoder[A], default: => Any, enabled: Boolean)`. Only the field with a
default value (`case class R3QuxDefaults(firstName: String, age: Int = 7, nick: Option[String])`) triggered it.

Making the parameter strict (`default: Any`) fixed it; the same default-value expression spliced into a strict
parameter (`CirceDerivationUtils.decodeFieldUsingDefault`) always worked. Presumably the by-name argument is
re-owned into a synthetic function whose owner chain the spliced (already typed) default-getter call does not match.

## Reproducer

Splice `foldInstanceFree(param.defaultValue.get, ...)` (a call to `C.$lessinit$greater$default$N`, upcast to `Any`)
into a by-name `Any` parameter of a runtime helper inside a Scala 2 cross-quote, from a macro expanded in a test
file. Not yet minimized outside Kindlings.

## Former workaround in Kindlings

`CirceDerivationUtils.defaultingDecoder` took the default value as a strict parameter.

## Re-checked with Hearth `0.4.3-79-gc2c1ffe-SNAPSHOT` (2026-11-13)

Then still open, and the workaround stayed. Making `CirceDerivationUtils.defaultingDecoder`'s `default` parameter
by-name again (`default: => Any`) still crashes the Scala 2.13 backend while compiling `CirceParityRound3Spec.scala`
("an unexpected type representation reached the compiler backend ... `<error>` (of class
scala.reflect.internal.Types$ErrorType$)"); Scala 3 is unaffected. Not minimized: the ingredients are a Scala 2
cross-quote `Expr.quote { helper(Expr.splice(decoder), Expr.splice(default), Expr.splice(cfg).useDefaults) }` where
`helper` takes `default: => Any` and `default` is `foldInstanceFree(param.defaultValue.get, ...).value.upcast[Any]`, a
typed call to the case class's `$lessinit$greater$default$N` (here of `R3QuxDefaults`, a top-level test case class).
A reproducer should splice an already-typed `C.$lessinit$greater$default$2` call (from `Parameter.defaultValue`) into a
by-name `Any` parameter; the suspicion is that the by-name argument is re-owned into a synthetic function whose owner
chain the spliced, already-typed tree does not follow (`upcast[Any]` adds a typed ascription around it).

## Reproduced in Hearth, then resolved in `0.4.3-90-gc3f9223-SNAPSHOT` (2026-11-14)

Hearth reproduced it with one more ingredient: the case class must be compiled in the same run as the expansion. It
crashed from the same file, or from another file in a clean build. It did not crash when the case class came from a
class file, e.g. after an incremental compile. Kindlings' `R3QuxDefaults` is in the test file itself. The `upcast[Any]`
did not matter: the bare typed getter call crashed too. A strict parameter worked, and so did an upcast literal spliced
into the by-name parameter. Scala 3 was unaffected.

With 0.4.3-90, `defaultingDecoder(decoder: Decoder[A], default: => Any, enabled: Boolean)` is by-name again. A clean
`circeDerivation2_13/testFull` compiles `CirceParityRound3Spec` (`KindlingsDecoder.incomplete[String => R3QuxDefaults]`)
and passes. By-name evaluates the default on every use, like the defaults of the generated decoders
(`decodeFieldUsingDefault` is called with the default expression on every decode). `defaultingDecoder` is newer than
0.3.2, so MiMa is unaffected. The other runtime helpers that take a default (`decodeFieldUsingDefault`,
`decodeFieldWithUnsafeDefault`, ...) stay strict: they are called with the default expression on every decode anyway,
and some of them are in 0.3.2's binary API.
