# Hearth gap: `implicit val T: Type[X] = Type.of[X]` in an `object` of a macro bundle reads itself on Scala 2

Status: **RESOLVED** in Hearth `0.4.3-90-gc3f9223-SNAPSHOT` (kubuszok/hearth#402, released in 0.4.4): the field
being defined is now shadowed for an implicit val in an `object` nested in a macro bundle as well. Kindlings' workaround
is removed. Scala 2.13 only.

Found 2026-11-13 while bumping Kindlings to Hearth `0.4.3-79-gc2c1ffe-SNAPSHOT`. It was first described here as a
regression of applied types. Both parts were wrong: see "Root cause" below.

## Symptom

Every String-keyed map encoder of circe-, yaml-, sconfig-, pureconfig- (and avro-) derivation failed on Scala 2.13 with
0.4.3-79:

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

The bytecode of the object's constructor initialised the field with
`ctx.weakTypeTag(convertProvidedTypesForCrossQuotes(this.StringJsonPairT()))`: `Type.of[(String, Json)]` found the
implicit `Type[(String, Json)]` being defined (still `null`). The `selfReferenceShadows` of `typeOfImpl`, which shadows
the implicit vals enclosing the expansion, emitted no shadow `val`.

## Root cause (corrected)

- **Not limited to applied types.** Every `Type.of` self-reference in a nested `object` was `null`, `Type[String]` and
  `Type[Option[Int]]` included, with or without other implicit `Type`s in scope. The #285 fix covered only members of
  the bundle trait itself and method-local vals. Kindlings' #285 case (`CirceJsonFieldConfigExtension`) is a member of
  a class, so it kept working.
- **Not a regression.** `0.4.3-24-gc0c6e43-SNAPSHOT` gives the same `null`s. What changed in 0.4.3-79 is
  `LambdaBuilder.build` on Scala 2: it now reads `Type[To].tpe.dealias` to decide whether to ascribe a literal-typed
  lambda. The map rules call `builder.build[(String, V)]` with the `null` pair type in scope, so 0.4.3-79 dereferenced
  a `null` that 0.4.3-24 never touched.

Hearth reproducer: `CrossTypeSelfRefSpec` "implicit val A: Type[A] = Type.of[A] in an object of the macro bundle should
materialize directly, not resolve to itself" (fixture `CrossTypeSelfRefFixturesImpl.SelfRefRuleObject`).

## Former workaround in Kindlings (removed)

The pair types were built in the modules' `Types` holders (`Types.StringJsonPair`, `Types.StringNodePair`,
`WTypes.StringValuePair`, `EncTypes.StringAnyPair`), and the rules' implicit vals referenced them. With 0.4.3-90 the
rules define them with `Type.of[...]` again (circe, yaml, sconfig, pureconfig, avro), and the full 2.13 suites pass.
