# Hearth findings from the jsoniter-scala parity work (2026-11)

Found while porting jsoniter-scala's own `JsonCodecMakerSpec` (v2.42.3) into the oracle suite
`JsoniterUpstreamSpecOracleSpec` (upstream-oracle-tests). Hearth `0.4.5-18-g99ac2ae-SNAPSHOT`, Scala 3.8.4 / 2.13.18.
Each one is visible in that suite (as an expected difference, a `compileErrors` gap, or - when it crashes the
compiler - a fixture which could not be added; noted below).

## 1. The `Factory` summoned for an abstract collection type is the one of its element type's companion (Scala 2 + 3)

**Status:** NOT A HEARTH BUG - kept as a documented difference (maintainer decision). A plain
`implicitly[scala.collection.Factory[...]]` gives the same result on 2.13.18 and 3.8.4: it is Scala's implicit search.
Documented in `docs/user-guide/feature-parity.md` ("Collections typed as an abstract collection type"); visible as the
expected differences of `defaults / mutableIterables`, `defaults / mutableMaps` and the two "rebuilt as the declared
implementations" tests.

`IsCollectionProviderForScalaCollection` summons `scala.collection.Factory[Item, A]`. `Factory[-A, +C]` is covariant in
the collection, and the implicit scope of `Factory[Item, Coll[Item]]` includes the companion of `Item`. When `Item` is
itself a collection which is a subtype of `Coll[_]`, its companion's `iterableFactory` / `mapFactory`
(`Factory[X, Item[X]]`, with `X = Item`) is more specific than `Coll`'s own and wins:

```scala
implicitly[Factory[mutable.Stack[Float], mutable.IndexedSeq[mutable.Stack[Float]]]]
// builds a mutable.Stack[mutable.Stack[Float]], not mutable.IndexedSeq's default (ArrayBuffer)
implicitly[Factory[(Float, mutable.ListMap[BigInt, String]), mutable.Map[Float, mutable.ListMap[BigInt, String]]]]
// builds a mutable.ListMap (linear-time inserts), not a mutable.HashMap
```

So `mutable.Map[K, mutable.ListMap[..]]`, `Set[ListSet[A]]`, `collection.Seq[Vector[A]]`, `Iterable[Queue[A]]`, ... are
decoded into the element type's implementation. Besides the observable class, this is a performance trap when the
element type is a linear-time collection (`ListMap`, `ListSet`). jsoniter-scala builds the declared type's own default.

(A possible change, not planned: resolve the factory of the standard library's abstract collection types from the
collection type's own companion, `Coll.iterableFactory[Item]` / `mapFactory`, instead of the implicit search.)

## 2. Default values of a case class nested in a trait break the compilation after typer (Scala 2 + 3)

**Status:** OPEN. Not representable as a test: the crash is after typer (no `compileErrors`), and it takes the whole
compilation unit down. The fixtures (`JUNestedDefaults.User`, `JUNestedDefaults.Box[Int]`, jsoniter-scala issue 1245)
are kept in `jsoniterUpstreamSpecFixtures.scala` without cases.

```scala
trait NestedDefaults {
  case class User(name: String, age: Int = 42)
}
object NestedDefaults extends NestedDefaults

object Codecs {
  val user = KindlingsJsonValueCodec.derived[NestedDefaults.User]
}
// java.lang.AssertionError: assertion failed: missing outer accessor in object Codecs
//   at dotty.tools.dotc.transform.ExplicitOuter$.outerParamAccessor(ExplicitOuter.scala:234)
//   at dotty.tools.dotc.transform.Erasure$Typer.typedThis(Erasure.scala:769)
```

The same class without default values derives and works (encoding, decoding and construction go through the path
`NestedDefaults.User`), so it is the default value's expression (`Parameter` default / `$lessinit$greater$default$N`)
which refers to the companion through `NestedDefaults.this` (the trait's `this`) rather than through the prefix of the
type. Same family as the `Shape.this.Empty$` type prefix bug fixed in Hearth 985e5e6
(and `hearth-bug-scala2-singleton-this-prefix.md`). Scala 2.13 fails as well, with
`Internal error: unable to find the outer accessor symbol of object Codecs`.

## 3. A Java enum nested in a Java class cannot be derived on Scala 3

**Status:** OPEN in Hearth `0.4.5-18-g99ac2ae-SNAPSHOT` (Scala 2.13 derives it). Asserted by
`JsoniterUpstreamSpecOracleSpec` "gaps":

```scala
KindlingsJsonValueCodec.derived[java.lang.Thread.State]
// Macro derivation failed with the following errors:
//   - assertion failed: expected a term symbol, but received class State
```

(upstream's fixture is `Levels.InnerLevel`).

**Cause (confirmed 2026-11-28):** `Expr.singletonOf` on the enum TYPE itself, called by `SingletonValue.parse[A]` in the
"handle as singleton" rules, which run before the enum rule. Full trace:
`repro-details/thread-state-singletonOf-trace.txt`:

```
java.lang.AssertionError: assertion failed: expected a term symbol, but received class State
  at scala.quoted.runtime.impl.QuotesImpl$reflect$Ref$.apply(QuotesImpl.scala:513)
  at hearth.typed.ExprsScala3$Expr$.singletonOf(ExprsScala3.scala:256)
  at hearth.typed.Classes$SingletonValue$.unapply(Classes.scala:220)
  at hearth.typed.Classes$SingletonValue$.parse(Classes.scala:227)
  at ...jsoniterderivation...EncoderHandleAsSingletonRule$.apply(EncoderHandleAsSingletonRule.scala:15)
```

The branch of `singletonOf` meant for Java enum constants (`sym.flags.is(Enum) && sym.flags.is(JavaStatic)` ->
`Ref(sym)`) also matches the class symbol of a Java enum nested in a class: a nested Java enum is a static nested class
(`Flags.Enum | Flags.Final | Flags.JavaDefined | Flags.JavaStatic`), while a top-level one has no `JavaStatic`. Minimal
Hearth-only reproducer (`Expr.singletonOf[java.lang.Thread.State]` and `[java.lang.Character.UnicodeScript]` throw,
`[java.math.RoundingMode]` gives `None`):

```scala
class ReproMacros(q: Quotes) extends hearth.MacroCommonsScala3(using q) {
  def run[A: Type]: Expr[String] = Expr(scala.util.Try(Expr.singletonOf[A]).toString)
}
```

Suggested fix: require a term symbol (`sym.isTerm`) in that branch.

## 4. A Java enum read from a class file has no children until its constants are completed (Scala 3)

**Status:** WORKED AROUND in every derivation module (`derivation-commons`: `JavaEnumSupport.parseEnum`, first done in
jsoniter-derivation as `CodecMacros.completeJavaEnum`), open in Hearth. Minimal reproducer, cause, and the related
children-order problem: `hearth-bug-java-enum-unreferenced-constants.md`.

```scala
// nothing else in the compilation refers to a constant of the enum
KindlingsJsonValueCodec.derived[java.math.RoundingMode]
// ... The rule handle as enum when possible was not applicable: java.math.RoundingMode is not sealed, not an
// enumeration, and not a union type
```

It derives as soon as some code compiled in the same run mentions a constant (`RoundingMode.UP`), which is why tests
(which build sample values) do not see it and users (a `case class Config(mode: RoundingMode)` codec) do. dotc
registers the children of a Java enum when the infos of its constants - static members, i.e. members of the companion
module - are completed. The workaround forces them before `Enum.parse`:

```scala
val module = TypeRepr.of[A].typeSymbol.companionModule
if module.isTerm then module.declaredFields.foreach(field => module.termRef.memberType(field))
```

Suggested fix in Hearth: do the same in `directChildren` / `isJavaEnum` for Java-defined enums.

## 5. `Enumeration` children include values which are not accessible (Scala 3)

**Status:** no longer reached by jsoniter-derivation (enumerations are looked up at runtime now, see
`JsoniterScalaEnumeration`); open in Hearth.

```scala
object CelestialBody extends Enumeration {
  val Sun, Moon = Value
  private val Vesta = Value("Vesta")
}
// a match over Enum.parse[CelestialBody.Value]'s children refers to the private `Vesta`:
// java.lang.AssertionError: assertion failed: private value Vesta in object CelestialBody ... accessed from method
//   encode_Value$macro$1   (dotty.tools.dotc.transform.ExpandPrivate.ensurePrivateAccessible)
```

The children of an `Enumeration` should only be the values accessible at the expansion site (and note that an
enumeration's values are not a closed set at compile time: lazy vals, `var`s, values created by methods).

## 6. Scala 2: a back-quoted field name which starts or ends with a space

**Status:** OPEN (gap). Asserted by `JsoniterUpstreamSpecOracleSpec` "gaps".

```scala
final case class SpacedName(` x `: Int)
KindlingsJsonValueCodec.derived[SpacedName] // Scala 2:
// ' x ' parameter of 'SpacedName' should be defined as 'val' or 'var' in the primary constructor.
```

The constructor parameter is reported as `" x "` but neither `caseFieldValuesAt` nor `Method.unsortedMethodsNamed`
finds a member under that name (Scala 2 marks local field names with a trailing space, and names are trimmed somewhere
on the way). Operators inside names (`kebab-case`, `=<>!#%^&|*/\~+-:$`) work on both versions. Scala 3 is not affected.

## 7. Scala 2: `Type.plainPrint` prints encoded names

Not a bug per se: `Type.plainPrint` of `` `US/Alaska` `` is `US$divAlaska` on Scala 2 and `US/Alaska` on Scala 3.
jsoniter-derivation now decodes the names of ADT leaves on Scala 2 (`adtLeafFullName`); other users of printed names
as data (discriminator values, schema names) have the same difference.
