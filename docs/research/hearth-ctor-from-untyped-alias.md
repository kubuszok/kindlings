# Scala 3: CtorN.fromUntyped fails to match an alias of an opaque type application

Confirmed with Hearth **0.4.2**, Scala **3.8.4**, Iron **3.3.2**.

Upstream issue: https://github.com/kubuszok/hearth/issues/384

**Status: RESOLVED in Hearth 0.4.3** (kubuszok/hearth#385): Scala 3 `Type.CtorN.fromUntyped` retries the match on the
dealiased type. The parser no longer dealiases `.as[C]` targets itself.

## Expected vs actual

Given `type AtLeastTwo = List[Int] :| MinLength[2]`, a
`Type.Ctor2.fromUntyped[IronType]` extractor should recognize the alias just as it recognizes
its dealiased representation. Instead the original type returns `None`, while `.dealias`
returns a match. Semantic type aliases should not change provider selection.

## Standalone reproducer

Save these two files in one directory and run `scala-cli run . --server=false`.

`Probe.scala`:

```scala
//> using scala 3.8.4
//> using dep com.kubuszok::hearth:0.4.2
//> using dep io.github.iltotore::iron:3.3.2

import scala.quoted.*
import io.github.iltotore.iron.IronType

object Probe {
  inline def aliasMatches[A]: (Boolean, Boolean) = ${ impl[A] }

  private def impl[A: Type](using q: Quotes): Expr[(Boolean, Boolean)] = {
    import q.reflect.*
    val ctx = new hearth.MacroCommonsScala3
    val ctor = ctx.Type.Ctor2.fromUntyped[IronType](
      TypeRepr.of[IronType].asInstanceOf[ctx.UntypedType]
    )
    val original = TypeRepr.of[A].asInstanceOf[ctx.UntypedType].as_??
    val normalized = TypeRepr.of[A].dealias.asInstanceOf[ctx.UntypedType].as_??
    Expr((ctor.unapply(original.Underlying).isDefined,
          ctor.unapply(normalized.Underlying).isDefined))
  }
}
```

`Main.scala`:

```scala
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.collection.MinLength

object Main {
  type AtLeastTwo = List[Int] :| MinLength[2]
  def main(args: Array[String]): Unit = {
    val result = Probe.aliasMatches[AtLeastTwo]
    println(result) // actual: (false,true); expected: (true,true)
    assert(result == (true, true))
  }
}
```

The executable companion probe in `parser-hearth-probes/` produced
`Iron alias: original=false, dealiased=true`. No Kindlings provider/parser is necessary
to reproduce the mismatch.

## Downstream impact

Kindlings' parser resolves `.as[C]` repetition targets with `IsCollection`/`IsValueType`.
Its Iron provider uses `Ctor2.fromUntyped[IronType]`. Without explicitly dealiasing the
target in the Scala 3 parser bridge, the existing `IronParserSpec` fails to compile with
“not a supported collection ... nor a value type wrapping one”. Restoring `.dealias`
makes the integration pass. This normalization is currently a downstream workaround.

## Likely cause and suggested coverage

`project/TypeConstructorsGen.scala`, `fromUntypedImpl3`, emits:

```scala
val aRepr = TypeRepr.of[A](using A.asInstanceOf[scala.quoted.Type[A]])
aRepr match {
  case AppliedType(ctor, List(a, b)) if ctor =:= hktRepr.asInstanceOf[TypeRepr] => /* match */
  case _ => aRepr.baseType(hktRepr.asInstanceOf[TypeRepr].typeSymbol) match { /* ... */ }
}
```

The alias does not have the outer `AppliedType` shape; the `baseType` fallback does not
recover an opaque application. Normalize ordinary aliases before the structural match.
Check normalization of the constructor representation too, and cover Ctor1/Ctor2/higher
arities, aliases of opaque applications, and aliases in type arguments. Preserve opaque
boundaries; this is ordinary dealiasing, not extraction of an opaque type's representation.
