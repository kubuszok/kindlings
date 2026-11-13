# Scala 2 cross-quotes: splicing a default-value expression into a by-name parameter

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

## Workaround in Kindlings

Runtime helpers that receive default values take them as strict parameters.

## Re-checked with Hearth `0.4.3-79-gc2c1ffe-SNAPSHOT` (2026-11-13)

Status: **still OPEN**, the workaround stays. Making `CirceDerivationUtils.defaultingDecoder`'s `default` parameter
by-name again (`default: => Any`) still crashes the Scala 2.13 backend while compiling `CirceParityRound3Spec.scala`
("an unexpected type representation reached the compiler backend ... `<error>` (of class
scala.reflect.internal.Types$ErrorType$)"); Scala 3 is unaffected. Not minimized: the ingredients are a Scala 2
cross-quote `Expr.quote { helper(Expr.splice(decoder), Expr.splice(default), Expr.splice(cfg).useDefaults) }` where
`helper` takes `default: => Any` and `default` is `foldInstanceFree(param.defaultValue.get, ...).value.upcast[Any]`, a
typed call to the case class's `$lessinit$greater$default$N` (here of `R3QuxDefaults`, a top-level test case class).
A reproducer should splice an already-typed `C.$lessinit$greater$default$2` call (from `Parameter.defaultValue`) into a
by-name `Any` parameter; the suspicion is that the by-name argument is re-owned into a synthetic function whose owner
chain the spliced, already-typed tree does not follow (`upcast[Any]` adds a typed ascription around it).
