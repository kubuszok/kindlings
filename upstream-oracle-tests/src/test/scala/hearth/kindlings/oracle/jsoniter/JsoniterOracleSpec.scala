package hearth.kindlings.oracle.jsoniter

import hearth.MacroSuite

/** Kindlings' `KindlingsJsonValueCodec.derived` vs jsoniter-scala's `JsonCodecMaker.make`, case by case: the same
  * fixture derived with the same config on both sides (Kindlings twice: with the config known at compile time and only
  * at runtime), compared on encoded JSON, accepted/rejected inputs (with error messages) and `nullValue`.
  *
  * Each case lists the *exact* differences it expects; each one must be a documented, intentional difference (see
  * `docs/user-guide/feature-parity.md` and `docs/user-guide/jsoniter-derivation.md`). A difference that appears or
  * disappears fails the test.
  */
final class JsoniterOracleSpec extends MacroSuite {

  private def quoted(s: String): String =
    "\"" + s.flatMap {
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case c    => c.toString
    } + "\""

  /** A difference whose upstream side differs between jsoniter-scala's Scala 3 and Scala 2 macros. */
  private def scala3or2(scala3: String, scala2: String): String = if (JoScalaVersion.isScala3) scala3 else scala2

  private def check[A](codecs: => JoCodecs[A], samples: JoSamples[A])(expectedDifferences: String*): Unit = {
    val differences = JsoniterOracle.differences(codecs, samples.values, samples.inputs)
    assertEquals(
      differences,
      expectedDifferences.toList,
      s"Kindlings vs jsoniter-scala differences:\n${differences.map(quoted).mkString(",\n")}"
    )
  }

  group("upstreamDefaults") {
    test("person")(check(JoCases.upstreamDefaults.person, JoSamples.person)())
    test("defaults")(check(JoCases.upstreamDefaults.defaults, JoSamples.defaults)())
    test("emptyable")(check(JoCases.upstreamDefaults.emptyable, JoSamples.emptyable)())
    test("nestedOption")(check(JoCases.upstreamDefaults.nestedOption, JoSamples.nestedOption)())
    test("primitives")(check(JoCases.upstreamDefaults.primitives, JoSamples.primitives)())
    test("boxed")(check(JoCases.upstreamDefaults.boxed, JoSamples.boxed)())
    test("big")(check(JoCases.upstreamDefaults.big, JoSamples.big)())
    test("wrapped")(check(JoCases.upstreamDefaults.wrapped, JoSamples.wrapped)())
    test("maps")(check(JoCases.upstreamDefaults.maps, JoSamples.maps)())
    test("collections")(check(JoCases.upstreamDefaults.collections, JoSamples.collections)())
    test("nonEmpty")(check(JoCases.upstreamDefaults.nonEmpty, JoSamples.nonEmpty)())
    test("tuples")(check(JoCases.upstreamDefaults.tuples, JoSamples.tuples)())
    test("time")(check(JoCases.upstreamDefaults.time, JoSamples.time)())
    test("either")(check(JoCases.upstreamDefaults.either, JoSamples.either)())
    test("shape")(check(JoCases.upstreamDefaults.shape, JoSamples.shape)())
    test("circle")(check(JoCases.upstreamDefaults.circle, JoSamples.circle)())
    test("drawing")(check(JoCases.upstreamDefaults.drawing, JoSamples.drawing)())
    // Documented message difference (jsoniter-derivation guide, "Missing fields"): for a field with a default value,
    // jsoniter-scala's error for a value of another JSON type says `or null` (its decoder gets the default as
    // `default`); Kindlings decodes `null` as the default around the field's decoder, whose error does not mention it.
    test("shapeHolder")(
      check(JoCases.upstreamDefaults.shapeHolder, JoSamples.shapeHolder)(
        "decode {\"shape\":\"x\"}: kindlings=rejected: expected '{', offset: 0x00000009 upstream=rejected: expected '{' or null, offset: 0x00000009",
        "decode {\"shape\":1}: kindlings=rejected: expected '{', offset: 0x00000009 upstream=rejected: expected '{' or null, offset: 0x00000009",
        "decode {\"colour\":1}: kindlings=rejected: expected '{', offset: 0x0000000a upstream=rejected: expected '{' or null, offset: 0x0000000a",
        "decode {\"either\":1}: kindlings=rejected: expected '{', offset: 0x0000000a upstream=rejected: expected '{' or null, offset: 0x0000000a",
        "decode {\"either\":\"x\"}: kindlings=rejected: expected '{', offset: 0x0000000a upstream=rejected: expected '{' or null, offset: 0x0000000a"
      )
    )
    test("colour")(check(JoCases.upstreamDefaults.colour, JoSamples.colour)())
    test("enums")(check(JoCases.upstreamDefaults.enums, JoSamples.enums)())
    test("annotated")(check(JoCases.upstreamDefaults.annotated, JoSamples.annotated)())
    test("camel")(check(JoCases.upstreamDefaults.camel, JoSamples.camel)())
    test("user")(check(JoCases.upstreamDefaults.user, JoSamples.user)())
    test("scalaTransient")(check(JoCases.upstreamDefaults.scalaTransient, JoSamples.scalaTransient)())
    test("stringy")(check(JoCases.upstreamDefaults.stringy, JoSamples.stringy)())
    test("listInt")(check(JoCases.upstreamDefaults.listInt, JoSamples.listInt)())
    test("setInt")(check(JoCases.upstreamDefaults.setInt, JoSamples.setInt)())
    test("mapIntString")(check(JoCases.upstreamDefaults.mapIntString, JoSamples.mapIntString)())
    test("optionInt")(check(JoCases.upstreamDefaults.optionInt, JoSamples.optionInt)())
    test("tupleTop")(check(JoCases.upstreamDefaults.tupleTop, JoSamples.tupleTop)())
    test("literal")(check(JoCases.upstreamDefaults.literal, JoSamples.literal)())
  }
  group("upstreamDefaultsRecursive") {
    test("tree")(check(JoCases.upstreamDefaultsRecursive.tree, JoSamples.tree)())
    test("expr")(check(JoCases.upstreamDefaultsRecursive.expr, JoSamples.expr)())
  }
  // Kindlings' own defaults vs `make`'s: exactly the documented default-value differences (feature-parity,
  // "Default value differences"; `JsoniterConfig.jsoniterScalaDefaults` restores upstream's, see "upstreamDefaults"):
  //   - `transientDefault`/`transientEmpty`/`transientNone` are off: every field is written;
  //   - `checkFieldDuplication` is off: a repeated key is accepted (the last one wins);
  //   - `discriminatorFieldName` is `None`: leaves are wrapper objects (`{"Leaf":{...}}`), case objects strings, and an
  //     `Option` nested in an `Option` is the ADT `Option` in that encoding; (`{"Leaf":{}}` is accepted for case objects
  //     too, a documented superset);
  //   - `mapMaxInsertNumber`/`setMaxInsertNumber` are `Int.MaxValue`: no size limits.
  group("kindlingsDefaults") {
    test("person")(
      check(JoCases.kindlingsDefaults.person, JoSamples.person)(
        "encode JOPerson(Bob,0,None,List()): kindlings={\"name\":\"Bob\",\"age\":0,\"email\":null,\"tags\":[]} upstream={\"name\":\"Bob\",\"age\":0}",
        "decode {\"name\":\"Ann\",\"age\":30,\"email\":\"a@b.c\",\"tags\":[\"x\"],\"name\":\"Bob\"}: kindlings=accepted JOPerson(Bob,30,Some(a@b.c),List(x)) upstream=rejected: duplicated field \"name\", offset: 0x0000003a",
        "decode {\"name\":\"Ann\",\"age\":30,\"email\":\"a@b.c\",\"tags\":[\"x\"],\"age\":31}: kindlings=accepted JOPerson(Ann,31,Some(a@b.c),List(x)) upstream=rejected: duplicated field \"age\", offset: 0x00000039",
        "decode {\"name\":\"Ann\",\"age\":30,\"email\":\"a@b.c\",\"tags\":[\"x\"],\"email\":null}: kindlings=accepted JOPerson(Ann,30,None,List(x)) upstream=rejected: duplicated field \"email\", offset: 0x0000003b"
      )
    )
    test("defaults")(
      check(JoCases.kindlingsDefaults.defaults, JoSamples.defaults)(
        "encode JODefaults(1,d,List(1, 2),Some(3),Map(k -> 1)): kindlings={\"i\":1,\"s\":\"d\",\"l\":[1,2],\"o\":3,\"m\":{\"k\":1}} upstream={}",
        "encode JODefaults(2,e,List(),None,Map()): kindlings={\"i\":2,\"s\":\"e\",\"l\":[],\"o\":null,\"m\":{}} upstream={\"i\":2,\"s\":\"e\"}",
        "encode JODefaults(1,d,List(5),Some(4),Map(k -> 1)): kindlings={\"i\":1,\"s\":\"d\",\"l\":[5],\"o\":4,\"m\":{\"k\":1}} upstream={\"l\":[5],\"o\":4}",
        "decode {\"i\":1,\"i\":2}: kindlings=accepted JODefaults(2,d,List(1, 2),Some(3),Map(k -> 1)) upstream=rejected: duplicated field \"i\", offset: 0x0000000a"
      )
    )
    test("emptyable")(
      check(JoCases.kindlingsDefaults.emptyable, JoSamples.emptyable)(
        "encode JOEmptyable(List(),Vector(),Map(),Set(),None): kindlings={\"l\":[],\"v\":[],\"m\":{},\"s\":[],\"o\":null} upstream={}",
        "decode {\"l\":[1],\"l\":[2]}: kindlings=accepted JOEmptyable(List(2),Vector(),Map(),Set(),None) upstream=rejected: duplicated field \"l\", offset: 0x0000000c",
        "decode {\"o\":1,\"o\":null}: kindlings=accepted JOEmptyable(List(),Vector(),Map(),Set(),None) upstream=rejected: duplicated field \"o\", offset: 0x0000000a"
      )
    )
    test("nestedOption")(
      check(JoCases.kindlingsDefaults.nestedOption, JoSamples.nestedOption)(
        "encode JONestedOption(None,None): kindlings={\"oo\":null,\"o\":null} upstream={}",
        "encode JONestedOption(Some(None),Some(x)): kindlings={\"oo\":\"None\",\"o\":\"x\"} upstream={\"oo\":{\"type\":\"None\"},\"o\":\"x\"}",
        "encode JONestedOption(Some(Some(1)),None): kindlings={\"oo\":{\"Some\":{\"value\":1}},\"o\":null} upstream={\"oo\":{\"type\":\"Some\",\"value\":1}}",
        "decode {\"oo\":{\"type\":\"Some\",\"value\":1}}: kindlings=rejected: illegal discriminator, offset: 0x0000000d upstream=accepted JONestedOption(Some(Some(1)),None)",
        "decode {\"oo\":{\"type\":\"None\"}}: kindlings=rejected: illegal discriminator, offset: 0x0000000d upstream=accepted JONestedOption(Some(None),None)",
        "decode {\"oo\":\"None\"}: kindlings=accepted JONestedOption(Some(None),None) upstream=rejected: expected '{', offset: 0x00000006",
        "decode {\"oo\":{\"Some\":{\"value\":1}}}: kindlings=accepted JONestedOption(Some(Some(1)),None) upstream=rejected: expected key: \"type\", offset: 0x0000000d",
        "decode {\"oo\":{\"type\":\"Some\",\"value\":null}}: kindlings=rejected: illegal discriminator, offset: 0x0000000d upstream=rejected: illegal number, offset: 0x0000001d",
        "decode {\"oo\":{\"type\":\"Some\",\"value\":{\"type\":\"Some\",\"value\":1}}}: kindlings=rejected: illegal discriminator, offset: 0x0000000d upstream=rejected: illegal number, offset: 0x0000001d",
        "decode {\"oo\":{\"type\":\"None\"},\"o\":\"x\"}: kindlings=rejected: illegal discriminator, offset: 0x0000000d upstream=accepted JONestedOption(Some(None),Some(x))"
      )
    )
    test("shape")(
      check(JoCases.kindlingsDefaults.shape, JoSamples.shape)(
        "encode JOCircle(1.0): kindlings={\"JOCircle\":{\"radius\":1.0}} upstream={\"type\":\"JOCircle\",\"radius\":1.0}",
        "encode JORect(1.0,2.0): kindlings={\"JORect\":{\"width\":1.0,\"height\":2.0}} upstream={\"type\":\"JORect\",\"width\":1.0,\"height\":2.0}",
        "encode JOEmpty: kindlings=\"JOEmpty\" upstream={\"type\":\"JOEmpty\"}",
        "encode JOSquare(3.0): kindlings={\"sq\":{\"side\":3.0}} upstream={\"type\":\"sq\",\"side\":3.0}",
        "decode {\"type\":\"JOCircle\",\"radius\":1}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=accepted JOCircle(1.0)",
        "decode {\"radius\":1,\"type\":\"JOCircle\"}: kindlings=rejected: illegal discriminator, offset: 0x00000009 upstream=rejected: expected key: \"type\", offset: 0x00000009",
        "decode {\"type\":\"JOCircle\"}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=rejected: missing required field \"radius\", offset: 0x00000012",
        "decode {\"type\":\"JOCircle\",\"radius\":1,\"type\":\"JORect\"}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=rejected: duplicated field \"type\", offset: 0x00000024",
        "decode {\"type\":\"JOCircle\",\"radius\":1,\"extra\":1}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=accepted JOCircle(1.0)",
        "decode {\"type\":\"JOCircle\",\"radius\":1,\"radius\":2}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=rejected: duplicated field \"radius\", offset: 0x00000026",
        "decode {\"type\":\"Unknown\"}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=rejected: illegal value of discriminator field \"type\", offset: 0x00000010",
        "decode {\"type\":1}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=rejected: expected '\"', offset: 0x00000008",
        "decode {\"type\":null}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=rejected: expected '\"', offset: 0x00000008",
        "decode {\"type\":\"JOEmpty\"}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=accepted JOEmpty",
        "decode {\"type\":\"JOEmpty\",\"extra\":1}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=accepted JOEmpty",
        "decode {\"type\":\"sq\",\"side\":1}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=accepted JOSquare(1.0)",
        "decode {\"type\":\"JOSquare\",\"side\":1}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=rejected: illegal value of discriminator field \"type\", offset: 0x00000011",
        "decode {\"type\":\"jo_circle\",\"radius\":1}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=rejected: illegal value of discriminator field \"type\", offset: 0x00000012",
        "decode {\"type\":\"JO_CIRCLE\",\"radius\":1}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=rejected: illegal value of discriminator field \"type\", offset: 0x00000012",
        "decode {\"kind\":\"JOCircle\",\"radius\":1}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=rejected: expected key: \"type\", offset: 0x00000007",
        "decode {\"kind\":\"JOEmpty\"}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=rejected: expected key: \"type\", offset: 0x00000007",
        "decode \"JOEmpty\": kindlings=accepted JOEmpty upstream=rejected: expected '{', offset: 0x00000000",
        "decode \"jo_empty\": kindlings=rejected: illegal discriminator, offset: 0x00000009 upstream=rejected: expected '{', offset: 0x00000000",
        "decode \"JOCircle\": kindlings=rejected: illegal discriminator, offset: 0x00000009 upstream=rejected: expected '{', offset: 0x00000000",
        "decode {\"JOCircle\":{\"radius\":1}}: kindlings=accepted JOCircle(1.0) upstream=rejected: expected key: \"type\", offset: 0x0000000b",
        "decode {\"JOCircle\":{\"radius\":1},\"JORect\":{}}: kindlings=rejected: expected '}' or ',', offset: 0x00000018 upstream=rejected: expected key: \"type\", offset: 0x0000000b",
        "decode {\"JOEmpty\":{}}: kindlings=accepted JOEmpty upstream=rejected: expected key: \"type\", offset: 0x0000000a",
        "decode {\"JOEmpty\":{\"x\":1}}: kindlings=accepted JOEmpty upstream=rejected: expected key: \"type\", offset: 0x0000000a",
        "decode {\"sq\":{\"side\":1}}: kindlings=accepted JOSquare(1.0) upstream=rejected: expected key: \"type\", offset: 0x00000005",
        "decode {\"Unknown\":{}}: kindlings=rejected: illegal discriminator, offset: 0x0000000a upstream=rejected: expected key: \"type\", offset: 0x0000000a",
        "decode null: kindlings=rejected: expected '\"' or '{', offset: 0x00000000 upstream=rejected: expected '{', offset: 0x00000000",
        "decode []: kindlings=rejected: expected '\"' or '{', offset: 0x00000000 upstream=rejected: expected '{', offset: 0x00000000",
        "decode 1: kindlings=rejected: expected '\"' or '{', offset: 0x00000000 upstream=rejected: expected '{', offset: 0x00000000",
        "decode \"x\": kindlings=rejected: illegal discriminator, offset: 0x00000002 upstream=rejected: expected '{', offset: 0x00000000",
        "decode {\"a\":1,}: kindlings=rejected: illegal discriminator, offset: 0x00000004 upstream=rejected: expected key: \"type\", offset: 0x00000004",
        "decode {\"type\":\"JOCircle\",\"radius\":1.0}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=accepted JOCircle(1.0)",
        "decode {\"type\":\"JORect\",\"width\":1.0,\"height\":2.0}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=accepted JORect(1.0,2.0)",
        "decode {\"type\":\"sq\",\"side\":3.0}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=accepted JOSquare(3.0)"
      )
    )
    test("drawing")(
      check(JoCases.kindlingsDefaults.drawing, JoSamples.drawing)(
        "encode JODrawing(List(JOCircle(1.0), JOEmpty),JORect(1.0,2.0)): kindlings={\"shapes\":[{\"JOCircle\":{\"radius\":1.0}},\"JOEmpty\"],\"main\":{\"JORect\":{\"width\":1.0,\"height\":2.0}}} upstream={\"shapes\":[{\"type\":\"JOCircle\",\"radius\":1.0},{\"type\":\"JOEmpty\"}],\"main\":{\"type\":\"JORect\",\"width\":1.0,\"height\":2.0}}",
        "decode {\"shapes\":[],\"main\":\"JOEmpty\"}: kindlings=accepted JODrawing(List(),JOEmpty) upstream=rejected: expected '{', offset: 0x00000014",
        "decode {\"shapes\":[],\"main\":{\"type\":\"JOEmpty\"}}: kindlings=rejected: illegal discriminator, offset: 0x0000001b upstream=accepted JODrawing(List(),JOEmpty)",
        "decode {\"main\":{\"type\":\"JOCircle\",\"radius\":1}}: kindlings=rejected: illegal discriminator, offset: 0x0000000f upstream=accepted JODrawing(List(),JOCircle(1.0))",
        "decode {\"shapes\":[{\"kind\":\"JOEmpty\"}],\"main\":{\"kind\":\"JOCircle\",\"radius\":1}}: kindlings=rejected: illegal discriminator, offset: 0x00000012 upstream=rejected: expected key: \"type\", offset: 0x00000012",
        "decode {\"shapes\":[null],\"main\":{\"type\":\"JOEmpty\"}}: kindlings=rejected: expected '\"' or '{', offset: 0x0000000b upstream=rejected: expected '{', offset: 0x0000000b",
        "decode {\"shapes\":[{\"type\":\"JOCircle\",\"radius\":1.0},{\"type\":\"JOEmpty\"}],\"main\":{\"type\":\"JORect\",\"width\":1.0,\"height\":2.0}}: kindlings=rejected: illegal discriminator, offset: 0x00000012 upstream=accepted JODrawing(List(JOCircle(1.0), JOEmpty),JORect(1.0,2.0))"
      )
    )
    test("colour")(
      check(JoCases.kindlingsDefaults.colour, JoSamples.colour)(
        "encode JORed: kindlings=\"JORed\" upstream={\"type\":\"JORed\"}",
        "encode JOBlue: kindlings=\"blue\" upstream={\"type\":\"blue\"}",
        "decode \"JORed\": kindlings=accepted JORed upstream=rejected: expected '{', offset: 0x00000000",
        "decode \"blue\": kindlings=accepted JOBlue upstream=rejected: expected '{', offset: 0x00000000",
        "decode \"JOBlue\": kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=rejected: expected '{', offset: 0x00000000",
        "decode \"jo_red\": kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=rejected: expected '{', offset: 0x00000000",
        "decode \"Purple\": kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=rejected: expected '{', offset: 0x00000000",
        "decode {\"type\":\"JORed\"}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=accepted JORed",
        "decode {\"type\":\"blue\"}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=accepted JOBlue",
        "decode {\"JORed\":{}}: kindlings=accepted JORed upstream=rejected: expected key: \"type\", offset: 0x00000008",
        "decode {\"blue\":{}}: kindlings=accepted JOBlue upstream=rejected: expected key: \"type\", offset: 0x00000007",
        "decode null: kindlings=rejected: expected '\"', offset: 0x00000000 upstream=rejected: expected '{', offset: 0x00000000",
        "decode []: kindlings=rejected: expected '\"', offset: 0x00000000 upstream=rejected: expected '{', offset: 0x00000000",
        "decode 1: kindlings=rejected: expected '\"', offset: 0x00000000 upstream=rejected: expected '{', offset: 0x00000000",
        "decode \"x\": kindlings=rejected: illegal discriminator, offset: 0x00000002 upstream=rejected: expected '{', offset: 0x00000000",
        "decode {\"a\":1,}: kindlings=rejected: illegal discriminator, offset: 0x00000004 upstream=rejected: expected key: \"type\", offset: 0x00000004"
      )
    )
    test("setInt")(
      check(JoCases.kindlingsDefaults.setInt, JoSamples.setInt)(
        "decode [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23...(4019 chars): kindlings=accepted HashSet(809, 629, 1024, 760, 101, 479, 347, 846, 893, 518, 2...(5050 chars) upstream=rejected: too many set inserts, offset: 0x00000fb1"
      )
    )
    test("mapIntString")(
      check(JoCases.kindlingsDefaults.mapIntString, JoSamples.mapIntString)(
        "decode {\"1\":\"v\",\"2\":\"v\",\"3\":\"v\",\"4\":\"v\",\"5\":\"v\",\"6\":\"v\",\"7\":\"v\",\"8\"...(10169 chars): kindlings=accepted HashMap(809 -> v, 629 -> v, 1024 -> v, 760 -> v, 101 -> v, 4...(10175 chars) upstream=rejected: too many map inserts, offset: 0x000027b7"
      )
    )
  }
  group("snakeCase") {
    test("camel")(check(JoCases.snakeCase.camel, JoSamples.camel)())
  }
  group("snakeCase2") {
    test("camel")(check(JoCases.snakeCase2.camel, JoSamples.camel)())
  }
  group("kebabCase") {
    test("camel")(check(JoCases.kebabCase.camel, JoSamples.camel)())
  }
  group("pascalCase") {
    test("camel")(check(JoCases.pascalCase.camel, JoSamples.camel)())
  }
  group("camelCase") {
    test("camel")(check(JoCases.camelCase.camel, JoSamples.camel)())
  }
  group("partialFieldMapper") {
    test("camel")(check(JoCases.partialFieldMapper.camel, JoSamples.camel)())
  }
  group("snakeCaseLeaves") {
    test("shape")(check(JoCases.snakeCaseLeaves.shape, JoSamples.shape)())
    test("colour")(check(JoCases.snakeCaseLeaves.colour, JoSamples.colour)())
  }
  group("discriminatorKind") {
    test("shape")(check(JoCases.discriminatorKind.shape, JoSamples.shape)())
    test("drawing")(check(JoCases.discriminatorKind.drawing, JoSamples.drawing)())
  }
  group("noDiscriminator") {
    // Documented superset (jsoniter-derivation guide, "ADTs, enums and tuples"): without a discriminator a case object is
    // also read from `{"Leaf":{}}`.
    test("shape")(
      check(JoCases.noDiscriminator.shape, JoSamples.shape)(
        "decode {\"JOEmpty\":{}}: kindlings=accepted JOEmpty upstream=rejected: illegal discriminator, offset: 0x0000000a",
        "decode {\"JOEmpty\":{\"x\":1}}: kindlings=accepted JOEmpty upstream=rejected: illegal discriminator, offset: 0x0000000a"
      )
    )
    test("drawing")(check(JoCases.noDiscriminator.drawing, JoSamples.drawing)())
    // Documented message difference (jsoniter-derivation guide, "Missing fields"): for a field with a default value,
    // jsoniter-scala's error for a value of another JSON type says `or null` (its decoder gets the default as
    // `default`); Kindlings decodes `null` as the default around the field's decoder, whose error does not mention it.
    // (`"colour":{}`: Kindlings also reads case objects from `{"Leaf":{}}`, see "colour" below.)
    test("shapeHolder")(
      check(JoCases.noDiscriminator.shapeHolder, JoSamples.shapeHolder)(
        "decode {\"shape\":1}: kindlings=rejected: expected '\"' or '{', offset: 0x00000009 upstream=rejected: expected '\"' or '{' or null, offset: 0x00000009",
        "decode {\"colour\":1}: kindlings=rejected: expected '\"', offset: 0x0000000a upstream=rejected: expected '\"' or null, offset: 0x0000000a",
        "decode {\"colour\":{}}: kindlings=rejected: expected '\"', offset: 0x0000000b upstream=rejected: expected '\"' or null, offset: 0x0000000a",
        scala3or2(
          "decode {\"either\":1}: kindlings=rejected: expected '\"' or '{', offset: 0x0000000a upstream=rejected: expected '\"' or '{' or null, offset: 0x0000000a",
          "decode {\"either\":1}: kindlings=rejected: expected '{', offset: 0x0000000a upstream=rejected: expected '{' or null, offset: 0x0000000a"
        ),
        scala3or2(
          "decode {\"either\":\"x\"}: kindlings=rejected: expected '\"' or '{', offset: 0x0000000a upstream=rejected: expected '\"' or '{' or null, offset: 0x0000000a",
          "decode {\"either\":\"x\"}: kindlings=rejected: expected '{', offset: 0x0000000a upstream=rejected: expected '{' or null, offset: 0x0000000a"
        )
      )
    )
    // Documented superset (jsoniter-derivation guide, "ADTs, enums and tuples"): without a discriminator a case object is
    // also read from `{"Leaf":{}}`, so for an ADT of case objects only
    // an object is read as such (and fails with Kindlings' errors) where jsoniter-scala expects a string.
    test("colour")(
      check(JoCases.noDiscriminator.colour, JoSamples.colour)(
        "decode {\"type\":\"JORed\"}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=rejected: expected '\"', offset: 0x00000000",
        "decode {\"type\":\"blue\"}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=rejected: expected '\"', offset: 0x00000000",
        "decode {\"JORed\":{}}: kindlings=accepted JORed upstream=rejected: expected '\"', offset: 0x00000000",
        "decode {\"blue\":{}}: kindlings=accepted JOBlue upstream=rejected: expected '\"', offset: 0x00000000",
        "decode {}: kindlings=rejected: expected '\"', offset: 0x00000001 upstream=rejected: expected '\"', offset: 0x00000000",
        "decode {: kindlings=rejected: unexpected end of input, offset: 0x00000001 upstream=rejected: expected '\"', offset: 0x00000000",
        "decode {\"a\":1,}: kindlings=rejected: illegal discriminator, offset: 0x00000004 upstream=rejected: expected '\"', offset: 0x00000000"
      )
    )
    test("either")(check(JoCases.noDiscriminator.either, JoSamples.either)())
  }
  group("discriminatorAnywhere") {
    test("shape")(check(JoCases.discriminatorAnywhere.shape, JoSamples.shape)())
  }
  group("alwaysEmitDiscriminator") {
    test("shape")(check(JoCases.alwaysEmitDiscriminator.shape, JoSamples.shape)())
    test("circle")(check(JoCases.alwaysEmitDiscriminator.circle, JoSamples.circle)())
  }
  group("circeLike") {
    test("shape")(check(JoCases.circeLike.shape, JoSamples.shape)())
    // Documented message difference (jsoniter-derivation guide, "Missing fields"): for a field with a default value,
    // jsoniter-scala's error for a value of another JSON type says `or null` (its decoder gets the default as
    // `default`); Kindlings decodes `null` as the default around the field's decoder, whose error does not mention it.
    test("shapeHolder")(
      check(JoCases.circeLike.shapeHolder, JoSamples.shapeHolder)(
        scala3or2(
          "decode {\"shape\":\"x\"}: kindlings=rejected: expected '\"' or '{', offset: 0x00000009 upstream=rejected: expected '\"' or '{' or null, offset: 0x00000009",
          "decode {\"shape\":\"x\"}: kindlings=rejected: expected '{', offset: 0x00000009 upstream=rejected: expected '{' or null, offset: 0x00000009"
        ),
        scala3or2(
          "decode {\"shape\":1}: kindlings=rejected: expected '\"' or '{', offset: 0x00000009 upstream=rejected: expected '\"' or '{' or null, offset: 0x00000009",
          "decode {\"shape\":1}: kindlings=rejected: expected '{', offset: 0x00000009 upstream=rejected: expected '{' or null, offset: 0x00000009"
        ),
        scala3or2(
          "decode {\"colour\":1}: kindlings=rejected: expected '\"' or '{', offset: 0x0000000a upstream=rejected: expected '\"' or '{' or null, offset: 0x0000000a",
          "decode {\"colour\":1}: kindlings=rejected: expected '{', offset: 0x0000000a upstream=rejected: expected '{' or null, offset: 0x0000000a"
        ),
        scala3or2(
          "decode {\"either\":1}: kindlings=rejected: expected '\"' or '{', offset: 0x0000000a upstream=rejected: expected '\"' or '{' or null, offset: 0x0000000a",
          "decode {\"either\":1}: kindlings=rejected: expected '{', offset: 0x0000000a upstream=rejected: expected '{' or null, offset: 0x0000000a"
        ),
        scala3or2(
          "decode {\"either\":\"x\"}: kindlings=rejected: expected '\"' or '{', offset: 0x0000000a upstream=rejected: expected '\"' or '{' or null, offset: 0x0000000a",
          "decode {\"either\":\"x\"}: kindlings=rejected: expected '{', offset: 0x0000000a upstream=rejected: expected '{' or null, offset: 0x0000000a"
        )
      )
    )
    test("colour")(check(JoCases.circeLike.colour, JoSamples.colour)())
  }
  group("strict") {
    test("person")(check(JoCases.strict.person, JoSamples.person)())
    test("shape")(check(JoCases.strict.shape, JoSamples.shape)())
  }
  group("noTransientDefault") {
    test("defaults")(check(JoCases.noTransientDefault.defaults, JoSamples.defaults)())
  }
  group("noTransientEmpty") {
    test("emptyable")(check(JoCases.noTransientEmpty.emptyable, JoSamples.emptyable)())
    test("defaults")(check(JoCases.noTransientEmpty.defaults, JoSamples.defaults)())
  }
  group("noTransientNone") {
    test("emptyable")(check(JoCases.noTransientNone.emptyable, JoSamples.emptyable)())
    test("nestedOption")(check(JoCases.noTransientNone.nestedOption, JoSamples.nestedOption)())
  }
  group("requireCollections") {
    test("emptyable")(check(JoCases.requireCollections.emptyable, JoSamples.emptyable)())
    test("defaults")(check(JoCases.requireCollections.defaults, JoSamples.defaults)())
  }
  group("requireDefaults") {
    test("defaults")(check(JoCases.requireDefaults.defaults, JoSamples.defaults)())
  }
  group("noDuplicationCheck") {
    // Documented difference (jsoniter-derivation guide, "Missing fields"): with `checkFieldDuplication = false`,
    // jsoniter-scala passes the value read for an earlier duplicate of the key as the decoder's `default` (so a repeated
    // `null` keeps it); Kindlings passes the field's default (here `nullValue`, `None`).
    test("person")(
      check(JoCases.noDuplicationCheck.person, JoSamples.person)(
        "decode {\"name\":\"Ann\",\"age\":30,\"email\":\"a@b.c\",\"tags\":[\"x\"],\"email\":null}: kindlings=accepted JOPerson(Ann,30,None,List(x)) upstream=accepted JOPerson(Ann,30,Some(a@b.c),List(x))"
      )
    )
    test("defaults")(check(JoCases.noDuplicationCheck.defaults, JoSamples.defaults)())
  }
  group("stringified") {
    test("stringy")(check(JoCases.stringified.stringy, JoSamples.stringy)())
    test("listInt")(check(JoCases.stringified.listInt, JoSamples.listInt)())
    test("optionInt")(check(JoCases.stringified.optionInt, JoSamples.optionInt)())
    test("tupleTop")(check(JoCases.stringified.tupleTop, JoSamples.tupleTop)())
    test("mapIntString")(check(JoCases.stringified.mapIntString, JoSamples.mapIntString)())
    test("wrapped")(check(JoCases.stringified.wrapped, JoSamples.wrapped)())
  }
  group("mapAsArray") {
    test("maps")(check(JoCases.mapAsArray.maps, JoSamples.maps)())
    test("mapIntString")(check(JoCases.mapAsArray.mapIntString, JoSamples.mapIntString)())
  }
  group("bigDecimalPrecision") {
    test("big")(check(JoCases.bigDecimalPrecision.big, JoSamples.big)())
  }
  group("bigDecimalScaleLimit") {
    test("big")(check(JoCases.bigDecimalScaleLimit.big, JoSamples.big)())
  }
  group("bigDecimalDigitsLimit") {
    test("big")(check(JoCases.bigDecimalDigitsLimit.big, JoSamples.big)())
  }
  group("bigIntDigitsLimit") {
    test("big")(check(JoCases.bigIntDigitsLimit.big, JoSamples.big)())
  }
  group("insertLimits") {
    test("emptyable")(check(JoCases.insertLimits.emptyable, JoSamples.emptyable)())
    test("maps")(check(JoCases.insertLimits.maps, JoSamples.maps)())
  }
  group("bitSetValueLimit") {
    test("collections")(check(JoCases.bitSetValueLimit.collections, JoSamples.collections)())
  }
  group("scalaEnumValueId") {
    test("enums")(check(JoCases.scalaEnumValueId.enums, JoSamples.enums)())
  }
  group("javaEnumValueNameMapper") {
    test("enums")(check(JoCases.javaEnumValueNameMapper.enums, JoSamples.enums)())
  }
  group("skipNestedOptionValues") {
    test("nestedOption")(check(JoCases.skipNestedOptionValues.nestedOption, JoSamples.nestedOption)())
  }
  group("inlineOneValueClasses") {
    test("user")(check(JoCases.inlineOneValueClasses.user, JoSamples.user)())
    test("nameMap")(check(JoCases.inlineOneValueClasses.nameMap, JoSamples.nameMap)())
  }
  group("scalaTransientSupport") {
    test("scalaTransient")(check(JoCases.scalaTransientSupport.scalaTransient, JoSamples.scalaTransient)())
  }
  group("decodingOnly") {
    test("person")(check(JoCases.decodingOnly.person, JoSamples.person)())
  }
  group("encodingOnly") {
    test("person")(check(JoCases.encodingOnly.person, JoSamples.person)())
  }
  group("makeWithoutDiscriminator") {
    // Documented superset (jsoniter-derivation guide, "ADTs, enums and tuples"): without a discriminator a case object is
    // also read from `{"Leaf":{}}`.
    test("shape")(
      check(JoCases.makeWithoutDiscriminator.shape, JoSamples.shape)(
        "decode {\"JOEmpty\":{}}: kindlings=accepted JOEmpty upstream=rejected: illegal discriminator, offset: 0x0000000a",
        "decode {\"JOEmpty\":{\"x\":1}}: kindlings=accepted JOEmpty upstream=rejected: illegal discriminator, offset: 0x0000000a"
      )
    )
  }
  group("makeWithRequiredCollectionFields") {
    test("emptyable")(check(JoCases.makeWithRequiredCollectionFields.emptyable, JoSamples.emptyable)())
  }
  group("makeWithRequiredDefaultFields") {
    test("defaults")(check(JoCases.makeWithRequiredDefaultFields.defaults, JoSamples.defaults)())
  }
  group("makeWithSkipNestedOptionValues") {
    test("nestedOption")(check(JoCases.makeWithSkipNestedOptionValues.nestedOption, JoSamples.nestedOption)())
  }
  group("makeCirceLike") {
    test("shape")(check(JoCases.makeCirceLike.shape, JoSamples.shape)())
    test("colour")(check(JoCases.makeCirceLike.colour, JoSamples.colour)())
    test("emptyable")(check(JoCases.makeCirceLike.emptyable, JoSamples.emptyable)())
  }
  group("makeCirceLikeSnakeCased") {
    test("camel")(check(JoCases.makeCirceLikeSnakeCased.camel, JoSamples.camel)())
    test("shape")(check(JoCases.makeCirceLikeSnakeCased.shape, JoSamples.shape)())
    test("enums")(check(JoCases.makeCirceLikeSnakeCased.enums, JoSamples.enums)())
  }
  group("makeOpenapiLike") {
    test("shape")(check(JoCases.makeOpenapiLike.shape, JoSamples.shape)())
    test("defaults")(check(JoCases.makeOpenapiLike.defaults, JoSamples.defaults)())
    test("emptyable")(check(JoCases.makeOpenapiLike.emptyable, JoSamples.emptyable)())
  }

}
