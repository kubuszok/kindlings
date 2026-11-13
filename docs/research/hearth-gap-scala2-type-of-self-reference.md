# Hearth gap: `implicit val T: Type[(A, B)] = Type.of[(A, B)]` reads itself on Scala 2 (regression)

Status: **OPEN** (found 2026-11-13 while bumping Kindlings to Hearth `0.4.3-79-gc2c1ffe-SNAPSHOT`; it worked with
`0.4.3-24-gc0c6e43-SNAPSHOT`). Scala 2.13 only. Worked around in Kindlings.

## Symptom

Every String-keyed map encoder of circe-, yaml-, sconfig-, pureconfig- (and avro-) derivation failed on Scala 2.13:

```
Macro derivation failed with the following errors:
  - Cannot invoke "scala.reflect.api.TypeTags$WeakTypeTag.tpe()" because the return value of
    "hearth.typed.TypesScala2$Type$.apply(Object)" is null
```

The map rules are objects of the macro bundle with

```scala
@scala.annotation.nowarn("msg=Infinite loop")
object EncoderHandleAsMapRule extends EncoderDerivationRule("handle as map when possible") {
  implicit val JsonT: Type[Json] = Types.Json
  implicit val StringT: Type[String] = Types.String
  implicit val StringJsonPairT: Type[(String, Json)] = Type.of[(String, Json)]
  ...
}
```

With the new snapshot, the bytecode of the object's constructor initialises the field with

```
ctx.weakTypeTag(convertProvidedTypesForCrossQuotes(this.StringJsonPairT()))
```

i.e. `Type.of[(String, Json)]` found the implicit `Type[(String, Json)]` being defined (still `null`) through the
`convertProvidedTypesForCrossQuotes` implicit conversion, instead of building the type from `Type[String]` and
`Type[Json]`. The `selfReferenceShadows` of `typeOfImpl` (which shadows implicit vals enclosing the expansion) did not
apply: the generated block contained no shadow `val`.

The simple-type form (`implicit val ConfigT: Type[Configuration] = Type.of[Configuration]` in
`CirceJsonFieldConfigExtension`, the #285 case) still works; the failing values are object (class) members of an
applied type.

## Reproducer

Any Scala 2 macro bundle with

```scala
object Rule {
  implicit val StringT: Type[String] = Type.of[String]
  implicit val PairT: Type[(String, Int)] = Type.of[(String, Int)]
}
```

then use `Rule.PairT` (e.g. `Rule.PairT.prettyPrint`) in a macro expansion: `PairT` is `null`. In Kindlings: revert
the workaround below and run `sconfigDerivation2_13/testFull` (`ConfigWriterSpec` "WithMap").

## Workaround in Kindlings

The pair types are built in the modules' `Types` holders (`Types.StringJsonPair`, `Types.StringNodePair`,
`WTypes.StringValuePair`, `EncTypes.StringAnyPair`), where no implicit of the pair type is in scope, and the rules'
implicit vals reference them.
