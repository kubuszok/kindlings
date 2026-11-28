# Hearth gap: `semiEval` cannot read a qualified Java enum constant on Scala 3 ("Cannot resolve module")

Status: **OPEN** (found 2026-11-28, still failing with Hearth `0.4.5-18-g99ac2ae-SNAPSHOT`, Scala 3 only; Scala 2.13
evaluates it).

## Reproducer

Verified 2026-11-28 with a Hearth-only macro against `0.4.5-18-g99ac2ae-SNAPSHOT` (output:
`repro-details/semieval-qualified-java-enum-output.txt`). A qualified constant is typed as `Ident("FLOOR")` only when it
is the **whole** argument of the macro call; inside an annotation, or nested in the arguments of another call, it stays
a `Select`, and that is what fails:

| Expression given to `semiEval` (Scala 3)                                              | Tree of the constant                                              | Result                                          |
|---------------------------------------------------------------------------------------|-------------------------------------------------------------------|-------------------------------------------------|
| macro argument `java.math.RoundingMode.FLOOR` (inline or not)                         | `Ident("FLOOR")`                                                  | `Right(FLOOR)`                                  |
| `FLOOR` after `import java.math.RoundingMode.FLOOR` (from the original report, not re-run) | `Ident("FLOOR")`                                       | `Right(FLOOR)`                                  |
| macro argument `Cfg.default.withDecimalConfig(8, 2, java.math.RoundingMode.HALF_UP)` | `Select(Select(Select(Ident("java"), "math"), "RoundingMode"), "HALF_UP")` | `Left(Cannot resolve module: java.math.RoundingMode)` |
| annotation argument, `@ann(java.math.RoundingMode.FLOOR)` (primary or secondary constructor), via `Annotations.decodedConstructorArguments` | `Select(Select(Select(Ident("java"), "math"), "RoundingMode"), "FLOOR")` | `Left(Cannot resolve module: java.math.RoundingMode)` |

(`Cfg` is a case class compiled outside the calling module mirroring `AvroConfig.withDecimalConfig`.) Scala 2.13
evaluates the annotation argument (`AvroAvro4sParitySpec` takes the constant path there); the other rows were not run on
2.13. In Kindlings: `@avroScalePrecision(8, 2, java.math.RoundingMode.FLOOR)` is not evaluable on Scala 3, and by the
same tree shape neither is `AvroConfig.default.withDecimalConfig(8, 2, java.math.RoundingMode.HALF_UP)` (not run
against `AvroConfig` itself), so the macro splices the argument / reads the config at runtime (correct, only slower).

Minimal macro (Scala 3):

```scala
final class roundingOnly(roundingMode: java.math.RoundingMode) extends scala.annotation.StaticAnnotation

class ReproMacros(q: Quotes) extends hearth.MacroCommonsScala3(using q) {
  def annotationArgs[C: Type]: Expr[String] = {
    val param = Type[C].primaryConstructor.get.parameters.flatten.head._2
    Expr(param.annotations.map(ann => Annotations.decodedConstructorArguments(ann).toString).mkString)
  }
}
// case class Primary(@roundingOnly(java.math.RoundingMode.FLOOR) amount: BigDecimal)
// annotationArgs[Primary] == "Some(List(Left(Cannot resolve module: java.math.RoundingMode)))"
```

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
