# Hearth gap: `semiEval` cannot read a qualified Java enum constant on Scala 3 ("Cannot resolve module")

Status: **OPEN** (found 2026-11-28 with Hearth `0.4.4`-line snapshot pinned by Kindlings, Scala 3 only; Scala 2.13
evaluates it).

## Reproducer

In a shared (`MacroCommons`) macro implementation:

```scala
def evalMode(mode: Expr[java.math.RoundingMode]): String =
  mode.semiEval.fold(_.mkString(", "), _.toString)
```

called with:

| Argument                                                     | Scala 2.13 | Scala 3                                              |
|--------------------------------------------------------------|------------|------------------------------------------------------|
| `java.math.RoundingMode.FLOOR` (qualified, a `Select`)       | `FLOOR`    | `Cannot resolve module: java.math.RoundingMode`      |
| `FLOOR` after `import java.math.RoundingMode.FLOOR` (`Ident`) | `FLOOR`    | `FLOOR`                                              |

Observed for the argument of an annotation read with `Annotations.decodedConstructorArguments`
(`@avroScalePrecision(8, 2, java.math.RoundingMode.FLOOR)` gave `Left("Cannot resolve module: java.math.RoundingMode")`
on Scala 3). The table is derived from the evaluator's code (below); by the same path, a config built with a qualified
constant (`AvroConfig.default.withDecimalConfig(8, 2, java.math.RoundingMode.HALF_UP)`) should not be evaluable on
Scala 3 either, so the macro falls back to reading that config at runtime (correct, only slower; not separately
verified).

## Cause

In `ExprsScala3.scala` (`evalWithLocals`), Java enum values are only recognised as an `Ident` with the `Enum` +
`JavaStatic` flags (`resolveEnumValue`). A qualified reference is a `Select(qualifier, name)`: the qualifier (the Java
class's companion, `java.math.RoundingMode`) is evaluated first, through `resolveModule`, which looks for a
`java.math.RoundingMode$.MODULE$` that does not exist. Scala 2 constant-folds a Java enum value into
`Literal(Constant(symbol))`, which `ExprsScala2.scala` handles.

Suggested fix: handle `Select(_, _)` whose symbol is a Java enum value (or any `JavaStatic` field) like the `Ident`
case, i.e. `resolveEnumValue(term.symbol)` (or `Class.forName(owner).getField(name).get(null)` for static fields)
before evaluating the qualifier.

## Workaround in Kindlings

`avro-derivation` `SchemaForMacrosImpl.getScalePrecision`: when the rounding mode of `@avroScalePrecision` cannot be
evaluated, the annotation's argument expression (`Annotations.constructorArguments`) is spliced into the generated code
instead of a constant. Both paths are covered by `AvroAvro4sParitySpec` (the annotation uses the qualified form, so
Scala 3 takes the spliced path and Scala 2.13 the constant one).
