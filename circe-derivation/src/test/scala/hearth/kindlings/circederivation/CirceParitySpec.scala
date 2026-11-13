package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

// Types for the upstream-circe parity tests (circe 0.14.17 `io.circe.derivation.Configured*`).

case class ParityOptDerivedDefault(name: String, o: Option[SimplePerson] = None)
case class ParityOptDerivedSomeDefault(o: Option[SimplePerson] = Some(SimplePerson("dflt", 1)))
case class ParityPersonMap(m: Map[String, SimplePerson])

// Two-level nested hierarchy (upstream ConfiguredDerivesSuite "hierarchy of more than 1 level").
sealed trait ParityGrandParent
sealed trait ParityParent extends ParityGrandParent
case class ParityChild(a: Int, b: String) extends ParityParent
case class ParityUncle(name: String) extends ParityGrandParent

// Three-level hierarchy where a sibling has a field named like a leaf (upstream "more than 2 level").
sealed trait ParityGreatGrandParent
sealed trait ParityGrandParent3 extends ParityGreatGrandParent
case class ParityUncle3(ParityChild3: Int) extends ParityGrandParent3
sealed trait ParityParent3 extends ParityGrandParent3
case class ParityChild3(a: Int, b: String) extends ParityParent3

// Hierarchical case-object enum (upstream ConfiguredEnumDerivesSuites HierarchicalEnum).
sealed trait ParityHierEnum
case object ParityHA extends ParityHierEnum
sealed trait ParityHierSub extends ParityHierEnum
case object ParityHB extends ParityHierSub
case object ParityHC extends ParityHierSub

// Intermediate sealed trait with user-provided instances: upstream keeps using them under the intermediate's name.
sealed trait ParityUserGP
case class ParityUserLeaf(x: Int) extends ParityUserGP
sealed trait ParityUserParent extends ParityUserGP
case class ParityUserChild(a: Int) extends ParityUserParent
object ParityUserParent {
  implicit val encoder: Encoder[ParityUserParent] = Encoder.instance { case ParityUserChild(a) =>
    Json.obj("custom" -> Json.fromInt(a))
  }
  implicit val decoder: Decoder[ParityUserParent] =
    Decoder.instance(c => c.downField("custom").as[Int].map(ParityUserChild(_)))
}

// Round 2 types.
final case class ParityWrapper(inner: SimplePerson) extends AnyVal
final case class ParityIds(values: List[UserId]) extends AnyVal
case class ParityHasWrapper(w: ParityWrapper, ids: ParityIds)
sealed trait ParityId extends Any
final case class ParityIntId(v: Int) extends AnyVal with ParityId
final case class ParityStrId(v: String) extends AnyVal with ParityId
final case class ParityMaybeName(v: Option[String]) extends AnyVal
case class ParityP(name: ParityMaybeName)
sealed trait ParityColor
case object ParityRed extends ParityColor { override def toString = "r" }
case object ParityBlue extends ParityColor
case class ParityColorMap(m: Map[ParityColor, Int])

final class CirceParitySpec extends MacroSuite {

  private def mkPersonWithDefaultsDecoder(cfg: Configuration): Decoder[PersonWithDefaults] = {
    implicit val config: Configuration = cfg
    KindlingsDecoder.derived[PersonWithDefaults]
  }

  private def mkOptDerivedDefaultDecoder(cfg: Configuration): Decoder[ParityOptDerivedDefault] = {
    implicit val config: Configuration = cfg
    KindlingsDecoder.derived[ParityOptDerivedDefault]
  }

  private def mkShapeDecoder(cfg: Configuration): Decoder[Shape] = {
    implicit val config: Configuration = cfg
    KindlingsDecoder.derived[Shape]
  }

  group("upstream circe parity") {

    group("item 1: useDefaults + explicit null on a non-Option field uses the default") {

      val json = Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.Null)

      test("decode (static config)") {
        implicit val config: Configuration = Configuration.default.withDefaults
        KindlingsDecoder.decode[PersonWithDefaults](json) ==> Right(PersonWithDefaults("Alice", 25))
      }

      test("derived decoder apply and decodeAccumulating (static config)") {
        implicit val config: Configuration = Configuration.default.withDefaults
        val decoder = KindlingsDecoder.derived[PersonWithDefaults]
        decoder.decodeJson(json) ==> Right(PersonWithDefaults("Alice", 25))
        decoder.decodeAccumulating(json.hcursor).toEither ==> Right(PersonWithDefaults("Alice", 25))
      }

      test("derived decoder apply and decodeAccumulating (runtime config)") {
        val decoder = mkPersonWithDefaultsDecoder(Configuration.default.withDefaults)
        decoder.decodeJson(json) ==> Right(PersonWithDefaults("Alice", 25))
        decoder.decodeAccumulating(json.hcursor).toEither ==> Right(PersonWithDefaults("Alice", 25))
      }

      test("a present value of the wrong type still fails") {
        implicit val config: Configuration = Configuration.default.withDefaults
        val bad = Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromString("x"))
        assert(KindlingsDecoder.decode[PersonWithDefaults](bad).isLeft)
        assert(KindlingsDecoder.derived[PersonWithDefaults].decodeAccumulating(bad.hcursor).isInvalid)
      }

      test("without useDefaults an explicit null still fails") {
        assert(KindlingsDecoder.decode[PersonWithDefaults](json).isLeft)
      }
    }

    group("item 2: missing Option[Derived] field with a default decodes as None when useDefaults=false") {

      val json = Json.obj("name" -> Json.fromString("Alice"))

      test("decode (static config)") {
        KindlingsDecoder.decode[ParityOptDerivedDefault](json) ==> Right(ParityOptDerivedDefault("Alice", None))
      }

      test("derived decoder apply and decodeAccumulating (static config)") {
        val decoder = KindlingsDecoder.derived[ParityOptDerivedDefault]
        decoder.decodeJson(json) ==> Right(ParityOptDerivedDefault("Alice", None))
        decoder.decodeAccumulating(json.hcursor).toEither ==> Right(ParityOptDerivedDefault("Alice", None))
      }

      test("derived decoder apply and decodeAccumulating (runtime config)") {
        val decoder = mkOptDerivedDefaultDecoder(Configuration.default)
        decoder.decodeJson(json) ==> Right(ParityOptDerivedDefault("Alice", None))
        decoder.decodeAccumulating(json.hcursor).toEither ==> Right(ParityOptDerivedDefault("Alice", None))
      }

      test("a Some(...) default is not used when useDefaults=false") {
        KindlingsDecoder.decode[ParityOptDerivedSomeDefault](Json.obj()) ==> Right(ParityOptDerivedSomeDefault(None))
        KindlingsDecoder.derived[ParityOptDerivedSomeDefault].decodeAccumulating(Json.obj().hcursor).toEither ==>
          Right(ParityOptDerivedSomeDefault(None))
      }

      test("a Some(...) default is used when useDefaults=true") {
        implicit val config: Configuration = Configuration.default.withDefaults
        KindlingsDecoder.decode[ParityOptDerivedSomeDefault](Json.obj()) ==> Right(ParityOptDerivedSomeDefault())
      }
    }

    group("item 3: strictDecoding + discriminator accepts the discriminator key") {

      val json = Json.obj("type" -> Json.fromString("Circle"), "radius" -> Json.fromDoubleOrNull(1.0))

      test("decode (static config)") {
        implicit val config: Configuration = Configuration(strictDecoding = true, discriminator = Some("type"))
        KindlingsDecoder.decode[Shape](json) ==> Right(Circle(1.0))
      }

      test("derived decoder apply and decodeAccumulating (static config)") {
        implicit val config: Configuration = Configuration(strictDecoding = true, discriminator = Some("type"))
        val decoder = KindlingsDecoder.derived[Shape]
        decoder.decodeJson(json) ==> Right(Circle(1.0))
        decoder.decodeAccumulating(json.hcursor).toEither ==> Right(Circle(1.0))
      }

      test("derived decoder (runtime config)") {
        val decoder = mkShapeDecoder(Configuration(strictDecoding = true, discriminator = Some("type")))
        decoder.decodeJson(json) ==> Right(Circle(1.0))
        decoder.decodeAccumulating(json.hcursor).toEither ==> Right(Circle(1.0))
      }

      test("other unexpected keys are still rejected") {
        implicit val config: Configuration = Configuration(strictDecoding = true, discriminator = Some("type"))
        val extra = json.deepMerge(Json.obj("extra" -> Json.True))
        assert(KindlingsDecoder.decode[Shape](extra).isLeft)
      }
    }

    group("item 4: nested sealed hierarchies are flattened") {

      test("wrapper mode encodes a nested leaf under its own name") {
        val value: ParityGrandParent = ParityChild(1, "a")
        KindlingsEncoder.encode[ParityGrandParent](value) ==>
          Json.obj("ParityChild" -> Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromString("a")))
        KindlingsEncoder.derived[ParityGrandParent].apply(value) ==>
          Json.obj("ParityChild" -> Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromString("a")))
      }

      test("wrapper mode decodes a nested leaf name at the top level") {
        val json = Json.obj("ParityChild" -> Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromString("a")))
        KindlingsDecoder.decode[ParityGrandParent](json) ==> Right(ParityChild(1, "a"))
        KindlingsDecoder.derived[ParityGrandParent].decodeJson(json) ==> Right(ParityChild(1, "a"))
      }

      test("discriminator mode encodes the leaf name and round-trips") {
        implicit val config: Configuration = Configuration.default.withDiscriminator("type")
        val value: ParityGrandParent = ParityChild(1, "a")
        val json = KindlingsEncoder.encode[ParityGrandParent](value)
        json.asObject.flatMap(_("type")) ==> Some(Json.fromString("ParityChild"))
        KindlingsDecoder.decode[ParityGrandParent](json) ==> Right(value)
        val codec = KindlingsCodecAsObject.derived[ParityGrandParent]
        codec.decodeJson(codec(value)) ==> Right(value)
      }

      test("discriminator mode round-trips a 3-level hierarchy with a field named like a leaf") {
        implicit val config: Configuration = Configuration.default.withDiscriminator("type")
        val codec = KindlingsCodecAsObject.derived[ParityGreatGrandParent]
        val child: ParityGreatGrandParent = ParityChild3(1, "a")
        codec.decodeJson(codec(child)) ==> Right(child)
        val uncle: ParityGreatGrandParent = ParityUncle3(2)
        codec.decodeJson(codec(uncle)) ==> Right(uncle)
      }

      test("wrapper mode round-trips a 3-level hierarchy") {
        val codec = KindlingsCodecAsObject.derived[ParityGreatGrandParent]
        val child: ParityGreatGrandParent = ParityChild3(1, "a")
        codec(child) ==> Json.obj("ParityChild3" -> Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromString("a")))
        codec.decodeJson(codec(child)) ==> Right(child)
      }

      test("enumAsStrings treats a hierarchy of case objects as an enum") {
        implicit val config: Configuration = Configuration.default.withEnumAsStrings
        val value: ParityHierEnum = ParityHB
        KindlingsEncoder.encode[ParityHierEnum](value) ==> Json.fromString("ParityHB")
        KindlingsDecoder.decode[ParityHierEnum](Json.fromString("ParityHB")) ==> Right(ParityHB)
        KindlingsDecoder.decode[ParityHierEnum](Json.fromString("ParityHA")) ==> Right(ParityHA)
      }

      test("intermediate sealed trait with user-provided instances keeps using them (wrapper)") {
        val value: ParityUserGP = ParityUserChild(3)
        val json = KindlingsEncoder.encode[ParityUserGP](value)
        json ==> Json.obj("ParityUserParent" -> Json.obj("custom" -> Json.fromInt(3)))
        KindlingsDecoder.decode[ParityUserGP](json) ==> Right(value)
      }

      test("intermediate sealed trait with user-provided instances keeps using them (discriminator)") {
        implicit val config: Configuration = Configuration.default.withDiscriminator("type")
        val value: ParityUserGP = ParityUserChild(3)
        val json = KindlingsEncoder.encode[ParityUserGP](value)
        json.asObject.map(_.toMap) ==>
          Some(Map("type" -> Json.fromString("ParityUserParent"), "custom" -> Json.fromInt(3)))
        KindlingsDecoder.decode[ParityUserGP](json) ==> Right(value)
      }
    }

    group("item 5: KindlingsCodecAsObject preserves decodeAccumulating") {

      test("reports every field error") {
        val codec = KindlingsCodecAsObject.derived[SimplePerson]
        val json = Json.obj("name" -> Json.fromInt(1), "age" -> Json.fromString("x"))
        codec.decodeAccumulating(json.hcursor).fold(errs => errs.size, _ => 0) ==> 2
      }
    }

    group("item 6: errors inside derived collection elements keep the cursor history") {

      val json = Json.obj(
        "name" -> Json.fromString("team"),
        "members" -> Json.arr(
          Json.obj("name" -> Json.fromString("a"), "age" -> Json.fromInt(1)),
          Json.obj("name" -> Json.fromString("b"), "age" -> Json.fromString("x"))
        )
      )

      test("List element (decode)") {
        val Left(err) = KindlingsDecoder.decode[TeamWithMembers](json): @unchecked
        err.pathToRootString ==> Some(".members[1].age")
      }

      test("List element (derived apply and decodeAccumulating)") {
        val decoder = KindlingsDecoder.derived[TeamWithMembers]
        val Left(err) = decoder.decodeJson(json): @unchecked
        err.pathToRootString ==> Some(".members[1].age")
        decoder.decodeAccumulating(json.hcursor).fold(_.toList.map(_.pathToRootString), _ => Nil) ==>
          List(Some(".members[1].age"))
      }

      test("top-level List of derived elements") {
        val arr = Json.arr(Json.obj("name" -> Json.fromString("b"), "age" -> Json.fromString("x")))
        val Left(err) = KindlingsDecoder.decode[List[SimplePerson]](arr): @unchecked
        err.pathToRootString ==> Some("[0].age")
      }

      test("Map value") {
        val mapJson = Json.obj(
          "m" -> Json.obj("bob" -> Json.obj("name" -> Json.fromString("b"), "age" -> Json.fromString("x")))
        )
        val Left(err) = KindlingsDecoder.decode[ParityPersonMap](mapJson): @unchecked
        err.pathToRootString ==> Some(".m.bob.age")
      }
    }

    group("item 7: strictDecoding in decodeAccumulating also accumulates field errors") {

      test("reports the strict failure and the field error") {
        implicit val config: Configuration = Configuration.default.withStrictDecoding
        val json = Json.obj(
          "name" -> Json.fromString("Alice"),
          "age" -> Json.fromString("not an int"),
          "extra" -> Json.True
        )
        val errors: List[DecodingFailure] =
          KindlingsDecoder.derived[SimplePerson].decodeAccumulating(json.hcursor).fold(_.toList, _ => Nil)
        errors.size ==> 2
        assert(errors.head.message.contains("extra"))
        errors(1).pathToRootString ==> Some(".age")
      }
    }

    group("item 8: wrapped sums with superfluous keys") {

      test("non-strict decoding ignores superfluous keys before the constructor key") {
        val json = Json.obj("extraField" -> Json.True, "Circle" -> Json.obj("radius" -> Json.fromDoubleOrNull(3.0)))
        KindlingsDecoder.decode[Shape](json) ==> Right(Circle(3.0))
        KindlingsDecoder.derived[Shape].decodeJson(json) ==> Right(Circle(3.0))
        KindlingsDecoder.derived[Shape].decodeAccumulating(json.hcursor).toEither ==> Right(Circle(3.0))
      }

      test("strict decoding rejects a multi-key wrapper object") {
        implicit val config: Configuration = Configuration.default.withStrictDecoding
        val json = Json.obj("Circle" -> Json.obj("radius" -> Json.fromDoubleOrNull(3.0)), "extraField" -> Json.True)
        assert(KindlingsDecoder.decode[Shape](json).isLeft)
        assert(KindlingsDecoder.derived[Shape].decodeAccumulating(json.hcursor).isInvalid)
      }
    }
    group(
      "item 9 (Kindlings default: unwrapping; upstream codes AnyVal as products, see round 3 item 3): value class wrapping a derived type round-trips"
    ) {

      test("AnyVal over a case class") {
        val value = ParityWrapper(SimplePerson("a", 1))
        val json = KindlingsEncoder.encode[ParityWrapper](value)
        json ==> Json.obj("name" -> Json.fromString("a"), "age" -> Json.fromInt(1))
        KindlingsDecoder.decode[ParityWrapper](json) ==> Right(value)
      }

      test("AnyVal over a List of value classes, as fields") {
        val value = ParityHasWrapper(ParityWrapper(SimplePerson("a", 1)), ParityIds(List(UserId(1), UserId(2))))
        val json = KindlingsEncoder.encode[ParityHasWrapper](value)
        KindlingsDecoder.decode[ParityHasWrapper](json) ==> Right(value)
        KindlingsDecoder.derived[ParityHasWrapper].decodeAccumulating(json.hcursor).toEither ==> Right(value)
      }
    }

    group("item 10 (Kindlings default: unwrapping): value-class children of a universal trait decode") {

      test("round-trip") {
        val value: ParityId = ParityIntId(5)
        val json = KindlingsEncoder.encode[ParityId](value)
        json ==> Json.obj("ParityIntId" -> Json.fromInt(5))
        KindlingsDecoder.decode[ParityId](json) ==> Right(value)
        KindlingsDecoder.derived[ParityId].decodeAccumulating(json.hcursor).toEither ==> Right(value)
        val str: ParityId = ParityStrId("x")
        KindlingsDecoder.decode[ParityId](KindlingsEncoder.encode[ParityId](str)) ==> Right(str)
      }
    }

    group("item 11 (Kindlings default: unwrapping): value class over Option as an absent field") {

      test("decodes as the wrapped None") {
        KindlingsDecoder.decode[ParityP](Json.obj()) ==> Right(ParityP(ParityMaybeName(None)))
        KindlingsDecoder.derived[ParityP].decodeJson(Json.obj()) ==> Right(ParityP(ParityMaybeName(None)))
        KindlingsDecoder.derived[ParityP].decodeAccumulating(Json.obj().hcursor).toEither ==>
          Right(ParityP(ParityMaybeName(None)))
      }

      test("present value still decodes") {
        KindlingsDecoder.decode[ParityP](Json.obj("name" -> Json.fromString("x"))) ==>
          Right(ParityP(ParityMaybeName(Some("x"))))
      }
    }

    group("item 12: enum map keys use the constructor name on both sides") {

      test("round-trip with an overridden toString") {
        val value = ParityColorMap(Map(ParityRed -> 1, ParityBlue -> 2))
        val json = KindlingsEncoder.encode[ParityColorMap](value)
        json ==> Json.obj("m" -> Json.obj("ParityRed" -> Json.fromInt(1), "ParityBlue" -> Json.fromInt(2)))
        KindlingsDecoder.decode[ParityColorMap](json) ==> Right(value)
      }

      test("transformConstructorNames applies on both sides") {
        implicit val config: Configuration = Configuration.default.withSnakeCaseConstructorNames
        val value = ParityColorMap(Map(ParityRed -> 1))
        val json = KindlingsEncoder.encode[ParityColorMap](value)
        json ==> Json.obj("m" -> Json.obj("parity_red" -> Json.fromInt(1)))
        KindlingsDecoder.decode[ParityColorMap](json) ==> Right(value)
      }
    }
    group(
      "backlog 3 (Kindlings default: unwrapping, matches generic-extras for non-object children): discriminator with a non-object child falls back to the wrapped form"
    ) {

      test("encodes wrapped and round-trips") {
        implicit val config: Configuration = Configuration.default.withDiscriminator("type")
        val value: ParityId = ParityIntId(5)
        val json = KindlingsEncoder.encode[ParityId](value)
        json ==> Json.obj("ParityIntId" -> Json.fromInt(5))
        KindlingsDecoder.decode[ParityId](json) ==> Right(value)
        KindlingsDecoder.derived[ParityId].decodeAccumulating(json.hcursor).toEither ==> Right(value)
      }

      test("object children still use the discriminator") {
        implicit val config: Configuration = Configuration.default.withDiscriminator("type")
        val value: Shape = Circle(1.0)
        val json = KindlingsEncoder.encode[Shape](value)
        json.asObject.flatMap(_("type")) ==> Some(Json.fromString("Circle"))
        KindlingsDecoder.decode[Shape](json) ==> Right(value)
      }
    }
  }
}
