package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, Json}

// Types for the third round of upstream-circe parity tests.

case class R3IntKeyPersonMap(m: Map[Int, SimplePerson])

// An intermediate sealed trait whose instance is derived by Kindlings (wrapper mode).
sealed trait R3GP
case class R3Uncle(n: Int) extends R3GP
sealed trait R3Parent extends R3GP
case class R3Child(a: Int) extends R3Parent
case class R3Child2(b: String) extends R3Parent
object R3Parent {
  implicit val codec: KindlingsCodecAsObject[R3Parent] = KindlingsCodecAsObject.derived[R3Parent]
}

// The same, with the intermediate's instance typed as circe's types and derived with a discriminator.
sealed trait R3DGP
case class R3DUncle(n: Int) extends R3DGP
sealed trait R3DParent extends R3DGP
case class R3DChild(a: Int) extends R3DParent
case class R3DChild2(b: String) extends R3DParent
object R3DParent {
  implicit val encoder: Encoder.AsObject[R3DParent] = {
    implicit val cfg: Configuration = Configuration.default.withDiscriminator("type")
    KindlingsEncoder.deriveAsObject[R3DParent]
  }
  implicit val decoder: Decoder[R3DParent] = {
    implicit val cfg: Configuration = Configuration.default.withDiscriminator("type")
    KindlingsDecoder.derived[R3DParent]
  }
}

// Separate encoder/decoder (not a codec) on a 3-level hierarchy where the middle level is Kindlings-derived and has
// a Kindlings-derived intermediate of its own.
sealed trait R3Top
case class R3TopLeaf(x: Int) extends R3Top
sealed trait R3Mid extends R3Top
case class R3MidLeaf(y: Int) extends R3Mid
sealed trait R3Low extends R3Mid
case class R3LowLeaf(z: Int) extends R3Low
object R3Low {
  implicit val codec: Codec.AsObject[R3Low] = KindlingsCodecAsObject.derived[R3Low]
}
object R3Mid {
  implicit val encoder: Encoder[R3Mid] = KindlingsEncoder.derived[R3Mid]
  implicit val decoder: Decoder[R3Mid] = KindlingsDecoder.derived[R3Mid]
}

// A Decoder[Option[X]] overriding tryDecode for absent keys, on a field with a default (item 4a).
case class R3MarkerWithDefault(name: String, marker: Option[ParityMissingMarker] = None)

// A value class over Option with a user decoder that does not override tryDecode (item 4b).
final case class R3UserMaybe(v: Option[String]) extends AnyVal
object R3UserMaybe {
  implicit val decoder: Decoder[R3UserMaybe] = Decoder.instance(c => c.as[Option[String]].map(R3UserMaybe(_)))
}
case class R3HasUserMaybe(m: R3UserMaybe)

// A case object outside a sum (item 4c).
case class R3HasYes(y: Yes.type)

// A case-object hierarchy whose intermediate has a hand-written instance (item 4e).
sealed trait R3Enum
case object R3A extends R3Enum
sealed trait R3EnumSub extends R3Enum
case object R3B extends R3EnumSub
case object R3C extends R3EnumSub
object R3EnumSub {
  implicit val encoder: Encoder[R3EnumSub] = Encoder.instance(s => Json.obj("sub" -> Json.fromString(s.toString)))
  implicit val decoder: Decoder[R3EnumSub] = Decoder.instance(c =>
    c.downField("sub").as[String].flatMap {
      case "R3B" => Right(R3B)
      case "R3C" => Right(R3C)
      case other => Left(DecodingFailure(s"unknown $other", c.history))
    }
  )
}

// Incomplete decoders (item 5a).
case class R3Qux(i: Int, a: String, j: Int)
case class R3QuxDefaults(firstName: String, age: Int = 7, nick: Option[String])

// A leaf reachable through two intermediates (diamond, item 4g).
sealed trait R3DiaGP
sealed trait R3DiaA extends R3DiaGP
sealed trait R3DiaB extends R3DiaGP
case class R3DiaLeaf(x: Int) extends R3DiaA with R3DiaB
case class R3DiaOther(y: Int) extends R3DiaB

// Map keys of a nested case-object hierarchy (item 4f).
case class R3HierKeyMap(m: Map[ParityHierEnum, Int])

final class CirceParityRound3Spec extends MacroSuite {

  group("upstream circe parity, round 3") {

    group("item 1: a user KeyEncoder/KeyDecoder for a built-in key type wins over the built-in toString") {

      test("Map[Int, DerivedValue] with hex key codecs") {
        import ParityCustomKeys.*
        val value = R3IntKeyPersonMap(Map(255 -> SimplePerson("a", 1)))
        val expected =
          Json.obj("m" -> Json.obj("ff" -> Json.obj("name" -> Json.fromString("a"), "age" -> Json.fromInt(1))))
        KindlingsEncoder.encode[R3IntKeyPersonMap](value) ==> expected
        KindlingsEncoder.derived[R3IntKeyPersonMap].apply(value) ==> expected
        KindlingsDecoder.decode[R3IntKeyPersonMap](expected) ==> Right(value)
        KindlingsDecoder.derived[R3IntKeyPersonMap].decodeJson(expected) ==> Right(value)
      }

      test("without user key codecs the built-in decimal form is kept") {
        val value = R3IntKeyPersonMap(Map(255 -> SimplePerson("a", 1)))
        val json = KindlingsEncoder.encode[R3IntKeyPersonMap](value)
        json.hcursor.downField("m").keys.map(_.toList) ==> Some(List("255"))
        KindlingsDecoder.decode[R3IntKeyPersonMap](json) ==> Right(value)
      }
    }

    group("item 2: an intermediate sealed trait with a Kindlings-derived instance is flattened") {

      test("wrapper mode: encodes the leaf under its own name (not nested under the intermediate)") {
        val value: R3GP = R3Child(1)
        val expected = Json.obj("R3Child" -> Json.obj("a" -> Json.fromInt(1)))
        KindlingsEncoder.encode[R3GP](value) ==> expected
        KindlingsEncoder.derived[R3GP].apply(value) ==> expected
        KindlingsCodecAsObject.derived[R3GP].apply(value) ==> expected
      }

      test("wrapper mode: decodes the leaf name at the top level") {
        val json = Json.obj("R3Child2" -> Json.obj("b" -> Json.fromString("x")))
        KindlingsDecoder.decode[R3GP](json) ==> Right(R3Child2("x"))
        KindlingsDecoder.derived[R3GP].decodeJson(json) ==> Right(R3Child2("x"))
        KindlingsDecoder.derived[R3GP].decodeAccumulating(json.hcursor).toEither ==> Right(R3Child2("x"))
        KindlingsDecoder.decode[R3GP](Json.obj("R3Uncle" -> Json.obj("n" -> Json.fromInt(2)))) ==> Right(R3Uncle(2))
      }

      test("wrapper mode: superfluous keys and unknown names") {
        val json = Json.obj("R3Child" -> Json.obj("a" -> Json.fromInt(1)), "extra" -> Json.True)
        KindlingsDecoder.decode[R3GP](json) ==> Right(R3Child(1))
        val Left(err) = KindlingsDecoder.decode[R3GP](Json.obj("Nope" -> Json.obj())): @unchecked
        err.message ==> "type R3GP has no class/object/case named 'Nope'."
      }

      test("discriminator mode: round-trips through the intermediate's own instance") {
        implicit val config: Configuration = Configuration.default.withDiscriminator("type")
        val value: R3DGP = R3DChild(1)
        val json = KindlingsEncoder.encode[R3DGP](value)
        json ==> Json.obj("a" -> Json.fromInt(1), "type" -> Json.fromString("R3DChild"))
        KindlingsDecoder.decode[R3DGP](json) ==> Right(value)
        KindlingsDecoder.derived[R3DGP].decodeJson(json) ==> Right(value)
        val codec = KindlingsCodecAsObject.derived[R3DGP]
        codec.decodeJson(codec(R3DChild2("q"))) ==> Right(R3DChild2("q"))
        codec.decodeJson(codec(R3DUncle(3))) ==> Right(R3DUncle(3))
      }

      test("three levels with Kindlings-derived instances at two levels") {
        val enc = KindlingsEncoder.derived[R3Top]
        val dec = KindlingsDecoder.derived[R3Top]
        val low: R3Top = R3LowLeaf(3)
        enc(low) ==> Json.obj("R3LowLeaf" -> Json.obj("z" -> Json.fromInt(3)))
        dec.decodeJson(enc(low)) ==> Right(low)
        val mid: R3Top = R3MidLeaf(2)
        enc(mid) ==> Json.obj("R3MidLeaf" -> Json.obj("y" -> Json.fromInt(2)))
        dec.decodeJson(enc(mid)) ==> Right(mid)
        dec.decodeJson(Json.obj("R3TopLeaf" -> Json.obj("x" -> Json.fromInt(1)))) ==> Right(R3TopLeaf(1))
      }

      test("hand-written intermediate instances keep their current behaviour") {
        val value: ParityUserGP = ParityUserChild(3)
        KindlingsEncoder.encode[ParityUserGP](value) ==>
          Json.obj("ParityUserParent" -> Json.obj("custom" -> Json.fromInt(3)))
      }
    }

    group("item 4a: useDefaults keeps a user Decoder[Option[X]]'s value for an absent key") {

      test("absent key") {
        implicit val config: Configuration = Configuration.default.withDefaults
        val json = Json.obj("name" -> Json.fromString("n"))
        val expected = R3MarkerWithDefault("n", Some(ParityMissingMarker("<absent>")))
        KindlingsDecoder.decode[R3MarkerWithDefault](json) ==> Right(expected)
        KindlingsDecoder.derived[R3MarkerWithDefault].decodeJson(json) ==> Right(expected)
        KindlingsDecoder.derived[R3MarkerWithDefault].decodeAccumulating(json.hcursor).toEither ==> Right(expected)
      }

      test("an absent Option field with a Some default still uses the default (keyMissingNone)") {
        implicit val config: Configuration = Configuration.default.withDefaults
        KindlingsDecoder.decode[ParityOptDerivedSomeDefault](Json.obj()) ==> Right(ParityOptDerivedSomeDefault())
        KindlingsDecoder.derived[ParityOptDerivedSomeDefault].decodeAccumulating(Json.obj().hcursor).toEither ==>
          Right(ParityOptDerivedSomeDefault())
        KindlingsDecoder.decode[WithOptionalAndDefault](Json.obj("name" -> Json.fromString("n"))) ==>
          Right(WithOptionalAndDefault("n"))
      }

      test("an explicit null on an Option field with a default decodes as None") {
        implicit val config: Configuration = Configuration.default.withDefaults
        KindlingsDecoder.decode[ParityOptDerivedSomeDefault](Json.obj("o" -> Json.Null)) ==>
          Right(ParityOptDerivedSomeDefault(None))
      }
    }

    group("item 4b: a value class over Option with a user decoder fails on an absent key") {

      test("missing field, with the cursor history") {
        val Left(err) = KindlingsDecoder.decode[R3HasUserMaybe](Json.obj()): @unchecked
        err.reason ==> DecodingFailure.Reason.MissingField
        err.pathToRootString ==> Some(".m")
        assert(KindlingsDecoder.derived[R3HasUserMaybe].decodeJson(Json.obj()).isLeft)
        assert(KindlingsDecoder.derived[R3HasUserMaybe].decodeAccumulating(Json.obj().hcursor).isInvalid)
      }

      test("a present value or null decodes") {
        KindlingsDecoder.decode[R3HasUserMaybe](Json.obj("m" -> Json.Null)) ==> Right(R3HasUserMaybe(R3UserMaybe(None)))
        KindlingsDecoder.decode[R3HasUserMaybe](Json.obj("m" -> Json.fromString("x"))) ==>
          Right(R3HasUserMaybe(R3UserMaybe(Some("x"))))
      }
    }

    group("item 4c: strict decoding of a case object outside a sum rejects extra keys") {

      test("top-level case object") {
        implicit val config: Configuration = Configuration.default.withStrictDecoding
        val Left(err) = KindlingsDecoder.decode[Yes.type](Json.obj("extra" -> Json.True)): @unchecked
        err.message ==> "Strict decoding Yes - unexpected fields: extra; valid fields: ."
        assert(KindlingsDecoder.derived[Yes.type].decodeJson(Json.obj("extra" -> Json.True)).isLeft)
        KindlingsDecoder.decode[Yes.type](Json.obj()) ==> Right(Yes)
      }

      test("case object field") {
        implicit val config: Configuration = Configuration.default.withStrictDecoding
        assert(KindlingsDecoder.decode[R3HasYes](Json.obj("y" -> Json.obj("extra" -> Json.True))).isLeft)
        KindlingsDecoder.decode[R3HasYes](Json.obj("y" -> Json.obj())) ==> Right(R3HasYes(Yes))
      }

      test("non-strict decoding still ignores extra keys") {
        KindlingsDecoder.decode[Yes.type](Json.obj("extra" -> Json.True)) ==> Right(Yes)
      }
    }

    group("item 4d: the strict wrapper failure lists the direct constructor names") {

      test("nested hierarchy") {
        implicit val config: Configuration = Configuration.default.withStrictDecoding
        val json = Json.obj(
          "ParityChild" -> Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromString("a")),
          "extra" -> Json.True
        )
        val Left(err) = KindlingsDecoder.decode[ParityGrandParent](json): @unchecked
        err.message ==>
          "Strict decoding ParityGrandParent - expected a single key json object with one of: ParityParent, ParityUncle."
      }
    }

    group("item 4e: enumAsStrings on a case-object hierarchy whose intermediate has a user instance") {

      test("the leaves are encoded and decoded as strings, like ConfiguredEnumCodec") {
        implicit val config: Configuration = Configuration.default.withEnumAsStrings
        KindlingsEncoder.encode[R3Enum](R3B) ==> Json.fromString("R3B")
        KindlingsEncoder.encode[R3Enum](R3A) ==> Json.fromString("R3A")
        KindlingsDecoder.decode[R3Enum](Json.fromString("R3C")) ==> Right(R3C)
        KindlingsDecoder.decode[R3Enum](Json.fromString("R3A")) ==> Right(R3A)
        KindlingsEncoder.derived[R3Enum].apply(R3C) ==> Json.fromString("R3C")
        KindlingsDecoder.derived[R3Enum].decodeJson(Json.fromString("R3B")) ==> Right(R3B)
      }

      test("without enumAsStrings the intermediate's instance is used") {
        KindlingsEncoder.encode[R3Enum](R3B) ==> Json.obj("R3EnumSub" -> Json.obj("sub" -> Json.fromString("R3B")))
        KindlingsDecoder.decode[R3Enum](Json.obj("R3EnumSub" -> Json.obj("sub" -> Json.fromString("R3C")))) ==>
          Right(R3C)
      }
    }

    group("item 4f: map keys of a nested case-object hierarchy") {

      test("round-trip with the leaf names") {
        val value = R3HierKeyMap(Map(ParityHA -> 1, ParityHB -> 2, ParityHC -> 3))
        val json = KindlingsEncoder.encode[R3HierKeyMap](value)
        json ==> Json.obj(
          "m" -> Json.obj("ParityHA" -> Json.fromInt(1), "ParityHB" -> Json.fromInt(2), "ParityHC" -> Json.fromInt(3))
        )
        KindlingsDecoder.decode[R3HierKeyMap](json) ==> Right(value)
      }
    }

    group("item 3: withoutValueClassUnwrapping codes AnyVal case classes as products (upstream parity)") {

      def runtimeConfig(unwrap: Boolean): Configuration = Configuration(unwrapValueClasses = unwrap)

      test("a value class over a primitive") {
        implicit val config: Configuration = Configuration.default.withoutValueClassUnwrapping
        KindlingsEncoder.encode[WrappedInt](WrappedInt(5)) ==> Json.obj("value" -> Json.fromInt(5))
        KindlingsEncoder.derived[WrappedInt].apply(WrappedInt(5)) ==> Json.obj("value" -> Json.fromInt(5))
        KindlingsDecoder.decode[WrappedInt](Json.obj("value" -> Json.fromInt(5))) ==> Right(WrappedInt(5))
        KindlingsDecoder.derived[WrappedInt].decodeJson(Json.obj("value" -> Json.fromInt(5))) ==> Right(WrappedInt(5))
        assert(KindlingsDecoder.decode[WrappedInt](Json.fromInt(5)).isLeft)
      }

      test("value class fields, with a runtime configuration") {
        implicit val config: Configuration = runtimeConfig(false)
        val value = ParityHasWrapper(ParityWrapper(SimplePerson("a", 1)), ParityIds(List(UserId(1))))
        val json = KindlingsEncoder.encode[ParityHasWrapper](value)
        json ==> Json.obj(
          "w" -> Json.obj("inner" -> Json.obj("name" -> Json.fromString("a"), "age" -> Json.fromInt(1))),
          "ids" -> Json.obj("values" -> Json.arr(Json.obj("value" -> Json.fromInt(1))))
        )
        KindlingsDecoder.decode[ParityHasWrapper](json) ==> Right(value)
        KindlingsDecoder.derived[ParityHasWrapper].decodeAccumulating(json.hcursor).toEither ==> Right(value)
        KindlingsEncoder.derived[ParityHasWrapper].apply(value) ==> json
      }

      test("the same derived instances keep unwrapping with the default configuration") {
        implicit val config: Configuration = runtimeConfig(true)
        val value = ParityHasWrapper(ParityWrapper(SimplePerson("a", 1)), ParityIds(List(UserId(1))))
        val json = KindlingsEncoder.encode[ParityHasWrapper](value)
        json ==> Json.obj(
          "w" -> Json.obj("name" -> Json.fromString("a"), "age" -> Json.fromInt(1)),
          "ids" -> Json.arr(Json.fromInt(1))
        )
        KindlingsDecoder.decode[ParityHasWrapper](json) ==> Right(value)
      }

      test("value-class sum children (wrapper)") {
        implicit val config: Configuration = Configuration.default.withoutValueClassUnwrapping
        val value: ParityId = ParityIntId(5)
        val json = KindlingsEncoder.encode[ParityId](value)
        json ==> Json.obj("ParityIntId" -> Json.obj("v" -> Json.fromInt(5)))
        KindlingsDecoder.decode[ParityId](json) ==> Right(value)
        KindlingsDecoder.derived[ParityId].decodeAccumulating(json.hcursor).toEither ==> Right(value)
      }

      test("value-class sum children (discriminator)") {
        implicit val config: Configuration =
          Configuration.default.withoutValueClassUnwrapping.withDiscriminator("type")
        val value: ParityId = ParityStrId("x")
        val json = KindlingsEncoder.encode[ParityId](value)
        json ==> Json.obj("v" -> Json.fromString("x"), "type" -> Json.fromString("ParityStrId"))
        KindlingsDecoder.decode[ParityId](json) ==> Right(value)
        KindlingsDecoder.derived[ParityId].decodeJson(json) ==> Right(value)
      }

      test("a value class over Option is a required object field") {
        implicit val config: Configuration = Configuration.default.withoutValueClassUnwrapping
        val Left(err) = KindlingsDecoder.decode[ParityP](Json.obj()): @unchecked
        err.reason ==> DecodingFailure.Reason.MissingField
        assert(KindlingsDecoder.derived[ParityP].decodeJson(Json.obj()).isLeft)
        KindlingsDecoder.decode[ParityP](Json.obj("name" -> Json.obj("v" -> Json.Null))) ==>
          Right(ParityP(ParityMaybeName(None)))
        KindlingsEncoder.encode[ParityP](ParityP(ParityMaybeName(Some("x")))) ==>
          Json.obj("name" -> Json.obj("v" -> Json.fromString("x")))
      }
    }

    group("item 5a: incomplete decoders (circe-generic deriveFor[F].incomplete)") {

      test("one supplied field (the first field of that type)") {
        val decoder: Decoder[Int => R3Qux] = KindlingsDecoder.incomplete[Int => R3Qux]
        val json = Json.obj("a" -> Json.fromString("x"), "j" -> Json.fromInt(2))
        decoder.decodeJson(json).map(_(1)) ==> Right(R3Qux(1, "x", 2))
      }

      test("several supplied fields") {
        val decoder = KindlingsDecoder.incomplete[(Int, Int) => R3Qux]
        decoder.decodeJson(Json.obj("a" -> Json.fromString("x"))).map(_(1, 2)) ==> Right(R3Qux(1, "x", 2))
        val all = KindlingsDecoder.incomplete[(String, Int, Int) => R3Qux]
        all.decodeJson(Json.obj()).map(_("y", 3, 4)) ==> Right(R3Qux(3, "y", 4))
      }

      test("errors: fail-fast and accumulating") {
        val decoder = KindlingsDecoder.incomplete[Int => R3Qux]
        val Left(err) = decoder.decodeJson(Json.obj("a" -> Json.fromInt(1))): @unchecked
        err.pathToRootString ==> Some(".a")
        // like circe: as many errors as invalid (here: missing) fields
        decoder.decodeAccumulating(Json.obj().hcursor).fold(_.size, _ => 0) ==> 2
      }

      test("configured names, Option fields and defaults (generic-extras deriveConfiguredFor[F].incomplete)") {
        implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames.withDefaults
        val decoder = KindlingsDecoder.incomplete[String => R3QuxDefaults]
        decoder.decodeJson(Json.obj()).map(_("n")) ==> Right(R3QuxDefaults("n", 7, None))
        decoder.decodeJson(Json.obj("age" -> Json.fromInt(3), "nick" -> Json.fromString("k"))).map(_("n")) ==>
          Right(R3QuxDefaults("n", 3, Some("k")))
      }

      test("a supplied type that is not a field type is a compile-time error") {
        compileErrors(
          """
          import hearth.kindlings.circederivation.{KindlingsDecoder, R3Qux}
          KindlingsDecoder.incomplete[Boolean => R3Qux]
          """
        ).check("has no (remaining) field of type")
      }
    }

    group("item 5b: public renaming helpers (Scala 3 core io.circe.derivation.renaming)") {

      test("the transformations") {
        renaming.snakeCase("HTMLParser") ==> "html_parser"
        renaming.snakeCase("firstName") ==> "first_name"
        renaming.screamingSnakeCase("firstName") ==> "FIRST_NAME"
        renaming.kebabCase("firstName") ==> "first-name"
        renaming.pascalCase("firstName") ==> "FirstName"
        renaming.replaceWith("Circle" -> "circle", "Rectangle" -> "rect")("Rectangle") ==> "rect"
        renaming.replaceWith("Circle" -> "circle")("Other") ==> "Other"
      }

      test("used in a configuration") {
        implicit val config: Configuration =
          Configuration.default.withTransformConstructorNames(renaming.replaceWith("Circle" -> "round"))
        KindlingsEncoder.encode[Shape](Circle(1.0)) ==> Json.obj(
          "round" -> Json.obj("radius" -> Json.fromDoubleOrNull(1.0))
        )
      }
    }

    group("item 5d: ExtrasDecoder (generic-extras isStrict / decodeStrict)") {

      test("a strict derived case-class decoder reports the extraneous fields") {
        implicit val config: Configuration = Configuration.default.withStrictDecoding
        val decoder: ExtrasDecoder[SimplePerson] = KindlingsDecoder.derived[SimplePerson]
        decoder.isStrict ==> true
        val json =
          Json.obj("name" -> Json.fromString("a"), "age" -> Json.fromInt(1), "x" -> Json.True, "y" -> Json.True)
        val Left((_, extraneous)) = decoder.decodeStrict(json.hcursor): @unchecked
        extraneous ==> List("x", "y")
        decoder.decodeStrict(Json.obj("name" -> Json.fromString("a"), "age" -> Json.fromInt(1)).hcursor) ==>
          Right(SimplePerson("a", 1))
      }

      test("a non-strict decoder") {
        val decoder: ExtrasDecoder[SimplePerson] = KindlingsDecoder.derived[SimplePerson]
        decoder.isStrict ==> false
        val Left((_, extraneous)) = decoder.decodeStrict(Json.obj("x" -> Json.True).hcursor): @unchecked
        extraneous ==> Nil
      }

      test("a codec delegates to its decoder") {
        implicit val config: Configuration = Configuration.default.withStrictDecoding
        val codec = KindlingsCodecAsObject.derived[SimplePerson]
        codec.isStrict ==> true
        val Left((_, extraneous)) =
          codec.decodeStrict(Json.obj("name" -> Json.fromString("a"), "z" -> Json.True).hcursor): @unchecked
        extraneous ==> List("z")
      }

      test("sum decoders are not strict (like generic-extras' ADT decoders)") {
        implicit val config: Configuration = Configuration.default.withStrictDecoding
        KindlingsDecoder.derived[Shape].isStrict ==> false
      }
    }

    group("item 5g: a compile-time configuration is evaluated (inline expression, named arguments)") {

      // A local `implicit val` is followed on Scala 3 only (CirceScala3Spec): Hearth's `semiEval` cannot resolve local
      // vals and their trees are not available to Scala 2 macros.
      test("enumAsStrings with non-singleton cases is a compile-time error") {
        compileErrors(
          """
          import hearth.kindlings.circederivation.{Configuration, KindlingsEncoder, Shape, Circle}
          KindlingsEncoder.encode[Shape](Circle(1.0))(using Configuration(enumAsStrings = true))
          """
        ).check("enumAsStrings is enabled but")
        compileErrors(
          """
          import hearth.kindlings.circederivation.{Configuration, KindlingsDecoder, Shape}
          KindlingsDecoder.decode[Shape](io.circe.Json.fromString("Circle"))(using Configuration.default.withEnumAsStrings)
          """
        ).check("enumAsStrings is enabled but")
        compileErrors(
          """
          import hearth.kindlings.circederivation.{Configuration, KindlingsEncoder, Shape}
          KindlingsEncoder.derived[Shape](using Configuration(enumAsStrings = true))
          """
        ).check("enumAsStrings is enabled but")
      }
    }

    group("item 4g: a leaf reachable through several intermediates (diamond)") {

      test("written once, under its own name, and read back") {
        val leaf: R3DiaGP = R3DiaLeaf(1)
        val json = KindlingsEncoder.encode[R3DiaGP](leaf)
        json ==> Json.obj("R3DiaLeaf" -> Json.obj("x" -> Json.fromInt(1)))
        KindlingsDecoder.decode[R3DiaGP](json) ==> Right(leaf)
        val other: R3DiaGP = R3DiaOther(2)
        KindlingsDecoder.decode[R3DiaGP](KindlingsEncoder.encode[R3DiaGP](other)) ==> Right(other)
      }
    }

    group("multiple parameter lists are rejected consistently") {

      test("an empty first parameter list") {
        compileErrors(
          """
          import hearth.kindlings.circederivation.KindlingsEncoder
          case class EmptyFirst()(val b: String)
          KindlingsEncoder.encode(EmptyFirst()("x"))
          """
        ).check("multiple parameter lists")
        compileErrors(
          """
          import hearth.kindlings.circederivation.KindlingsDecoder
          case class EmptyFirst()(val b: String)
          KindlingsDecoder.decode[EmptyFirst](io.circe.Json.obj())
          """
        ).check("multiple parameter lists")
      }
    }
  }
}
