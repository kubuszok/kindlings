package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.{Decoder, DecodingFailure, Encoder, Json, KeyDecoder, KeyEncoder}

// Types for the second round of upstream-circe parity tests.

case class ParityQux(i: Int, a: String, j: Int)
case class ParityPatchOuter(name: String, inner: SimplePerson)
case class ParityPatchCamel(firstName: String, lastAge: Int)
case class ParityTwoPersons(a: SimplePerson, b: SimplePerson)
case class ParityWithTuple(t: (Int, SimplePerson))
case class ParityWithTuple3(t: (String, Int, Option[SimplePerson]))
case class ParityIntKeyMap(m: Map[Int, String])
object ParityCustomKeys {
  // Custom key codecs for Int: hex instead of the default decimal.
  implicit val hexKeyEncoder: KeyEncoder[Int] = KeyEncoder.instance(i => Integer.toHexString(i))
  implicit val hexKeyDecoder: KeyDecoder[Int] =
    KeyDecoder.instance(s => scala.util.Try(Integer.parseInt(s, 16)).toOption)
}
final case class ParityMissingMarker(v: String)
object ParityMissingMarker {
  // A user-provided Option decoder that turns an absent key into a marker value.
  implicit val markerOptDecoder: Decoder[Option[ParityMissingMarker]] = new Decoder[Option[ParityMissingMarker]] {
    def apply(c: io.circe.HCursor): Decoder.Result[Option[ParityMissingMarker]] =
      c.as[Option[String]].map(_.map(ParityMissingMarker(_)))
    override def tryDecode(c: io.circe.ACursor): Decoder.Result[Option[ParityMissingMarker]] =
      if (c.failed) Right(Some(ParityMissingMarker("<absent>"))) else super.tryDecode(c)
    override def tryDecodeAccumulating(
        c: io.circe.ACursor
    ): Decoder.AccumulatingResult[Option[ParityMissingMarker]] =
      if (c.failed) cats.data.Validated.valid(Some(ParityMissingMarker("<absent>"))) else super.tryDecodeAccumulating(c)
  }
}
case class ParityWithMarker(name: String, marker: Option[ParityMissingMarker])
case class ParityOuterOfOptions(o: Option[SimplePerson])
case class ParityNameWrapper(name: String)
case class ParityPersonWrapper(person: SimplePerson)
case class ParityOptWrapper(v: Option[String])
object ParityOptWrapper {
  implicit val wrapperDecoder: Decoder[ParityOptWrapper] = KindlingsDecoder.deriveUnwrapped[ParityOptWrapper]
}
case class ParityHasOptWrapper(w: ParityOptWrapper)

final class CirceParityBacklogSpec extends MacroSuite {

  private def errorsOf[A](result: Decoder.AccumulatingResult[A]): List[DecodingFailure] =
    result.fold(_.toList, _ => Nil)

  group("upstream circe parity, round 2") {

    group("backlog 1: KindlingsDecoder.patch") {

      val q = ParityQux(1, "a", 2)

      test("absent and null fields keep the original value, present ones replace it") {
        val patch = KindlingsDecoder.patch[ParityQux]
        val options: List[Option[Int]] = List(None, Some(10))
        for {
          i <- options
          a <- List(None, Some("z"))
          j <- options
        } {
          val json = Json.obj(
            "i" -> Encoder[Option[Int]].apply(i),
            "a" -> Encoder[Option[String]].apply(a),
            "j" -> Encoder[Option[Int]].apply(j)
          )
          patch.decodeJson(json).map(_(q)) ==> Right(ParityQux(i.getOrElse(q.i), a.getOrElse(q.a), j.getOrElse(q.j)))
        }
        patch.decodeJson(Json.obj()).map(_(q)) ==> Right(q)
        patch.decodeJson(Json.obj("j" -> Json.fromInt(7))).map(_(q)) ==> Right(ParityQux(1, "a", 7))
      }

      test("a wrong type is a Left at decode time") {
        val patch = KindlingsDecoder.patch[ParityQux]
        val Left(err) = patch.decodeJson(Json.obj("i" -> Json.fromString("x"))): @unchecked
        err.pathToRootString ==> Some(".i")
      }

      test("accumulating reports every wrong field") {
        val patch = KindlingsDecoder.patch[ParityQux]
        val json = Json.obj("i" -> Json.fromString("x"), "a" -> Json.fromInt(1))
        errorsOf(patch.decodeAccumulating(json.hcursor)).map(_.pathToRootString) ==> List(Some(".i"), Some(".a"))
      }

      test("a non-object patch is a Left") {
        val patch = KindlingsDecoder.patch[ParityQux]
        assert(patch.decodeJson(Json.fromInt(1)).isLeft)
        assert(patch.decodeJson(Json.arr()).isLeft)
        assert(patch.decodeJson(Json.Null).isLeft)
      }

      test("nested fields are replaced, not deep-merged") {
        val patch = KindlingsDecoder.patch[ParityPatchOuter]
        val original = ParityPatchOuter("n", SimplePerson("a", 1))
        patch
          .decodeJson(Json.obj("inner" -> Json.obj("name" -> Json.fromString("b"), "age" -> Json.fromInt(2))))
          .map(
            _(original)
          ) ==> Right(ParityPatchOuter("n", SimplePerson("b", 2)))
        // a partial nested object does not decode (no deep merge)
        assert(patch.decodeJson(Json.obj("inner" -> Json.obj("name" -> Json.fromString("b")))).isLeft)
      }

      test("member names are transformed by the configuration") {
        implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames
        val patch = KindlingsDecoder.patch[ParityPatchCamel]
        patch.decodeJson(Json.obj("last_age" -> Json.fromInt(3))).map(_(ParityPatchCamel("x", 1))) ==>
          Right(ParityPatchCamel("x", 3))
      }

      test("works without implicit Encoder/Decoder of the patched type in scope") {
        val patch: Decoder[SimplePerson => SimplePerson] = KindlingsDecoder.patch[SimplePerson]
        patch.decodeJson(Json.obj("age" -> Json.fromInt(99))).map(_(SimplePerson("Alice", 30))) ==>
          Right(SimplePerson("Alice", 99))
      }
    }

    group("backlog 2: deriveAsObject / KindlingsCodecAsObject on non-object types") {

      test("deriveAsObject on a value class is a compile-time error") {
        compileErrors(
          """
          import hearth.kindlings.circederivation.{KindlingsEncoder, WrappedInt}
          KindlingsEncoder.deriveAsObject[WrappedInt]
          """
        ).check("does not encode to a JSON object")
      }

      test("KindlingsCodecAsObject on a value class is a compile-time error") {
        compileErrors(
          """
          import hearth.kindlings.circederivation.{KindlingsCodecAsObject, WrappedInt}
          KindlingsCodecAsObject.derived[WrappedInt]
          """
        ).check("does not encode to a JSON object")
      }

      test("KindlingsEncoder.derived returns an Encoder.AsObject for products and sums") {
        assert(KindlingsEncoder.derived[SimplePerson].isInstanceOf[Encoder.AsObject[?]])
        assert(KindlingsEncoder.derived[Shape].isInstanceOf[Encoder.AsObject[?]])
        assert(!KindlingsEncoder.derived[WrappedInt].isInstanceOf[Encoder.AsObject[?]])
        KindlingsEncoder.derived[SimplePerson].apply(SimplePerson("a", 1)) ==>
          Json.obj("name" -> Json.fromString("a"), "age" -> Json.fromInt(1))
      }
    }

    group("backlog 3: enumAsStrings with non-singleton cases") {

      // The compile-time variant (a configuration `semiEval` can evaluate) is tested in CirceParityRound3Spec (item 5g).

      test("a clear runtime failure when the config is only known at runtime") {
        def mkCfg(b: Boolean): Configuration = Configuration(enumAsStrings = b)
        implicit val cfg: Configuration = mkCfg(true)
        val decoder = KindlingsDecoder.derived[Shape]
        val Left(err) = decoder.decodeJson(Json.fromString("Circle")): @unchecked
        assert(err.message.contains("enumAsStrings"), err.message)
        val encoder = KindlingsEncoder.derived[Shape]
        val thrown = intercept[IllegalArgumentException](encoder(Circle(1.0)))
        assert(thrown.getMessage.contains("enumAsStrings"), thrown.getMessage)
      }
    }

    group("backlog 4: accumulating decoding accumulates in nested derived types") {

      val badPerson = Json.obj("name" -> Json.fromInt(1), "age" -> Json.fromString("x"))

      test("nested derived case class") {
        val json = Json.obj("a" -> badPerson, "b" -> badPerson)
        errorsOf(KindlingsDecoder.derived[ParityTwoPersons].decodeAccumulating(json.hcursor))
          .map(_.pathToRootString) ==> List(Some(".a.name"), Some(".a.age"), Some(".b.name"), Some(".b.age"))
      }

      test("collection of derived elements") {
        val json = Json.obj("name" -> Json.fromInt(1), "members" -> Json.arr(badPerson, badPerson))
        errorsOf(KindlingsDecoder.derived[TeamWithMembers].decodeAccumulating(json.hcursor))
          .map(_.pathToRootString) ==> List(
          Some(".name"),
          Some(".members[0].name"),
          Some(".members[0].age"),
          Some(".members[1].name"),
          Some(".members[1].age")
        )
      }

      test("Option of a derived type") {
        val json = Json.obj("o" -> badPerson)
        errorsOf(KindlingsDecoder.derived[ParityOuterOfOptions].decodeAccumulating(json.hcursor)).size ==> 2
      }

      test("Map values") {
        val json = Json.obj("m" -> Json.obj("x" -> badPerson, "y" -> badPerson))
        errorsOf(KindlingsDecoder.derived[ParityPersonMap].decodeAccumulating(json.hcursor)).size ==> 4
      }

      test("a top-level derived collection") {
        val json = Json.arr(badPerson, badPerson)
        errorsOf(KindlingsDecoder.derived[List[SimplePerson]].decodeAccumulating(json.hcursor))
          .map(_.pathToRootString) ==> List(Some("[0].name"), Some("[0].age"), Some("[1].name"), Some("[1].age"))
      }

      test("a sum child") {
        val json = Json.obj("Rectangle" -> Json.obj("width" -> Json.True, "height" -> Json.True))
        errorsOf(KindlingsDecoder.derived[Shape].decodeAccumulating(json.hcursor)).size ==> 2
      }

      test("fail-fast still stops at the first error") {
        val json = Json.obj("a" -> badPerson, "b" -> badPerson)
        val Left(err) = KindlingsDecoder.derived[ParityTwoPersons].decodeJson(json): @unchecked
        err.pathToRootString ==> Some(".a.name")
      }
    }

    group("backlog 5: tuples encode and decode as JSON arrays") {

      test("a tuple of a built-in and a derived type") {
        val value = ParityWithTuple((1, SimplePerson("a", 2)))
        val json = KindlingsEncoder.encode[ParityWithTuple](value)
        json ==> Json.obj(
          "t" -> Json.arr(Json.fromInt(1), Json.obj("name" -> Json.fromString("a"), "age" -> Json.fromInt(2)))
        )
        KindlingsDecoder.decode[ParityWithTuple](json) ==> Right(value)
        KindlingsDecoder.derived[ParityWithTuple].decodeAccumulating(json.hcursor).toEither ==> Right(value)
        KindlingsEncoder.derived[ParityWithTuple].apply(value) ==> json
      }

      test("a top-level tuple containing an Option of a derived type") {
        val value = ("x", 3, Option(SimplePerson("a", 2)))
        val json = KindlingsEncoder.encode[(String, Int, Option[SimplePerson])](value)
        json ==> Json.arr(
          Json.fromString("x"),
          Json.fromInt(3),
          Json.obj("name" -> Json.fromString("a"), "age" -> Json.fromInt(2))
        )
        KindlingsDecoder.decode[(String, Int, Option[SimplePerson])](json) ==> Right(value)
        KindlingsDecoder.decode[ParityWithTuple3](Json.obj("t" -> json)) ==> Right(ParityWithTuple3(value))
      }

      test("a wrong arity or a non-array is rejected") {
        val Left(err) =
          KindlingsDecoder.decode[ParityWithTuple](Json.obj("t" -> Json.arr(Json.fromInt(1)))): @unchecked
        err.pathToRootString ==> Some(".t")
        assert(KindlingsDecoder.decode[ParityWithTuple](Json.obj("t" -> Json.obj())).isLeft)
      }

      test("accumulating collects every element error (same failures as circe's tuple decoder)") {
        val elems = Json.arr(Json.fromString("x"), Json.obj())
        val json = Json.obj("t" -> elems)
        val circeTuple = Decoder.decodeTuple2(Decoder.decodeInt, KindlingsDecoder.derived[SimplePerson])
        val expected =
          errorsOf(circeTuple.decodeAccumulating(json.hcursor.downField("t").asInstanceOf[io.circe.HCursor]))
        expected.size ==> 3
        errorsOf(KindlingsDecoder.derived[ParityWithTuple].decodeAccumulating(json.hcursor)) ==> expected
      }
    }

    group("backlog 6: error messages match upstream") {

      test("unknown constructor (wrapped)") {
        val Left(err) = KindlingsDecoder.decode[Shape](Json.obj("Triangle" -> Json.obj())): @unchecked
        err.message ==> "type Shape has no class/object/case named 'Triangle'."
        err.pathToRootString ==> Some(".Triangle")
        val Left(err2) = KindlingsDecoder.derived[Shape].decodeJson(Json.obj("Triangle" -> Json.obj())): @unchecked
        err2.message ==> "type Shape has no class/object/case named 'Triangle'."
      }

      test("unknown constructor (discriminator)") {
        implicit val config: Configuration = Configuration.default.withDiscriminator("type")
        val Left(err) = KindlingsDecoder.decode[Shape](Json.obj("type" -> Json.fromString("Triangle"))): @unchecked
        err.message ==> "type Shape has no class/object/case named 'Triangle'."
      }

      test("missing or null discriminator") {
        implicit val config: Configuration = Configuration.default.withDiscriminator("type")
        val Left(err) = KindlingsDecoder.decode[Shape](Json.obj("radius" -> Json.fromInt(1))): @unchecked
        err.message ==> "Shape: could not find discriminator field 'type' or its null."
        err.pathToRootString ==> Some(".type")
        val Left(err2) = KindlingsDecoder.decode[Shape](Json.obj("type" -> Json.Null)): @unchecked
        err2.message ==> "Shape: could not find discriminator field 'type' or its null."
      }

      test("strict decoding of a product lists unexpected fields in JSON order") {
        implicit val config: Configuration = Configuration.default.withStrictDecoding
        val json = Json.obj(
          "zzz" -> Json.True,
          "name" -> Json.fromString("a"),
          "aaa" -> Json.True,
          "age" -> Json.fromInt(1)
        )
        val Left(err) = KindlingsDecoder.decode[SimplePerson](json): @unchecked
        err.message ==> "Strict decoding SimplePerson - unexpected fields: zzz, aaa; valid fields: name, age."
        val Left(err2) = KindlingsDecoder.derived[SimplePerson].decodeJson(json): @unchecked
        err2.message ==> "Strict decoding SimplePerson - unexpected fields: zzz, aaa; valid fields: name, age."
      }

      test("strict decoding of a product with a discriminator lists it as valid") {
        implicit val config: Configuration = Configuration(strictDecoding = true, discriminator = Some("type"))
        val json = Json.obj("type" -> Json.fromString("Circle"), "radius" -> Json.fromInt(1), "x" -> Json.True)
        val Left(err) = KindlingsDecoder.decode[Shape](json): @unchecked
        err.message ==> "Strict decoding Circle - unexpected fields: x; valid fields: radius, type."
      }

      test("strict decoding of a wrapper") {
        implicit val config: Configuration = Configuration.default.withStrictDecoding
        val json = Json.obj("Circle" -> Json.obj("radius" -> Json.fromInt(1)), "extra" -> Json.True)
        val Left(err) = KindlingsDecoder.decode[Shape](json): @unchecked
        err.message ==> "Strict decoding Shape - expected a single key json object with one of: Circle, Rectangle."
      }

      test("wrapper expectations") {
        val Left(err) = KindlingsDecoder.decode[Shape](Json.fromInt(1)): @unchecked
        err.reason ==> DecodingFailure.Reason.WrongTypeExpectation("object", Json.fromInt(1))
        val Left(err2) = KindlingsDecoder.decode[Shape](Json.obj()): @unchecked
        err2.reason ==> DecodingFailure.Reason.WrongTypeExpectation("non-empty json object", Json.obj())
      }

      test("collection and map expectations") {
        val Left(err) = KindlingsDecoder.decode[TeamWithMembers](
          Json.obj("name" -> Json.fromString("t"), "members" -> Json.fromInt(1))
        ): @unchecked
        err.reason ==> DecodingFailure.Reason.WrongTypeExpectation("array", Json.fromInt(1))
        err.pathToRootString ==> Some(".members")
        val Left(err2) = KindlingsDecoder.decode[ParityPersonMap](Json.obj("m" -> Json.arr())): @unchecked
        err2.reason ==> DecodingFailure.Reason.WrongTypeExpectation("object", Json.arr())
        err2.pathToRootString ==> Some(".m")
      }

      test("enumAsStrings unknown case") {
        implicit val config: Configuration = Configuration.default.withEnumAsStrings
        val Left(err) = KindlingsDecoder.decode[CardinalDirection](Json.fromString("Up")): @unchecked
        err.message ==> "enum CardinalDirection does not contain case: Up"
        val Left(err2) = KindlingsDecoder.decode[CardinalDirection](Json.fromInt(1)): @unchecked
        err2.reason ==> DecodingFailure.Reason.WrongTypeExpectation("string", Json.fromInt(1))
      }

      test("map-key decode errors keep the cursor history") {
        val Left(err) =
          KindlingsDecoder.decode[ParityIntKeyMap](Json.obj("m" -> Json.obj("x" -> Json.fromString("a")))): @unchecked
        err.message ==> "Couldn't decode key."
        err.pathToRootString ==> Some(".m.x")
      }
    }

    group("backlog 7: smaller items") {

      test("strict decoding applies to case objects in sums") {
        implicit val config: Configuration = Configuration.default.withStrictDecoding
        assert(KindlingsDecoder.decode[SimpleEnumCirce](Json.obj("Yes" -> Json.obj("extra" -> Json.fromInt(1)))).isLeft)
        KindlingsDecoder.decode[SimpleEnumCirce](Json.obj("Yes" -> Json.obj())) ==> Right(Yes)
      }

      test("the discriminator is appended after the fields") {
        implicit val config: Configuration = Configuration.default.withDiscriminator("type")
        val json = KindlingsEncoder.encode[Shape](Circle(1.0))
        json.asObject.map(_.keys.toList) ==> Some(List("radius", "type"))
        KindlingsEncoder.derived[Shape].apply(Circle(1.0)).asObject.map(_.keys.toList) ==>
          Some(List("radius", "type"))
      }

      test("user-provided KeyEncoder/KeyDecoder for Int take precedence") {
        import ParityCustomKeys.*
        val value = ParityIntKeyMap(Map(255 -> "x"))
        val json = KindlingsEncoder.encode[ParityIntKeyMap](value)
        json ==> Json.obj("m" -> Json.obj("ff" -> Json.fromString("x")))
        KindlingsDecoder.decode[ParityIntKeyMap](json) ==> Right(value)
        KindlingsEncoder.derived[ParityIntKeyMap].apply(value) ==> json
        KindlingsDecoder.derived[ParityIntKeyMap].decodeJson(json) ==> Right(value)
      }

      test("a user-provided Decoder[Option[X]] is consulted for an absent key") {
        KindlingsDecoder.decode[ParityWithMarker](Json.obj("name" -> Json.fromString("n"))) ==>
          Right(ParityWithMarker("n", Some(ParityMissingMarker("<absent>"))))
        KindlingsDecoder.derived[ParityWithMarker].decodeJson(Json.obj("name" -> Json.fromString("n"))) ==>
          Right(ParityWithMarker("n", Some(ParityMissingMarker("<absent>"))))
        KindlingsDecoder
          .derived[ParityWithMarker]
          .decodeAccumulating(Json.obj("name" -> Json.fromString("n")).hcursor)
          .toEither ==> Right(ParityWithMarker("n", Some(ParityMissingMarker("<absent>"))))
      }

      test("a derived Decoder[Option[X]] keeps tryDecode semantics for an absent key") {
        val decoder: Decoder[Option[SimplePerson]] = KindlingsDecoder.derived[Option[SimplePerson]]
        val missing = Json.obj().hcursor.downField("x")
        decoder.tryDecode(missing) ==> Right(None)
        decoder.tryDecodeAccumulating(missing).toEither ==> Right(None)
        // incorrect focus (downField on a non-object) is still an error, like circe
        assert(decoder.tryDecode(Json.fromInt(1).hcursor.downField("x")).isLeft)
      }

      test("derived Decoder[Unit] accepts only null, {} and []") {
        val decoder = KindlingsDecoder.derived[Unit]
        decoder.decodeJson(Json.Null) ==> Right(())
        decoder.decodeJson(Json.obj()) ==> Right(())
        decoder.decodeJson(Json.arr()) ==> Right(())
        assert(decoder.decodeJson(Json.fromInt(1)).isLeft)
        assert(decoder.decodeJson(Json.obj("a" -> Json.True)).isLeft)
        assert(KindlingsDecoder.decode[Unit](Json.fromString("x")).isLeft)
      }

      test("explicit unwrapping of a single-field case class (generic-extras deriveUnwrapped*)") {
        val enc = KindlingsEncoder.deriveUnwrapped[ParityNameWrapper]
        val dec = KindlingsDecoder.deriveUnwrapped[ParityNameWrapper]
        enc(ParityNameWrapper("x")) ==> Json.fromString("x")
        dec.decodeJson(Json.fromString("x")) ==> Right(ParityNameWrapper("x"))
        val pEnc = KindlingsEncoder.deriveUnwrapped[ParityPersonWrapper]
        val pDec = KindlingsDecoder.deriveUnwrapped[ParityPersonWrapper]
        val json = Json.obj("name" -> Json.fromString("a"), "age" -> Json.fromInt(1))
        pEnc(ParityPersonWrapper(SimplePerson("a", 1))) ==> json
        pDec.decodeJson(json) ==> Right(ParityPersonWrapper(SimplePerson("a", 1)))
        errorsOf(pDec.decodeAccumulating(Json.obj().hcursor)).size ==> 2
      }

      test("an unwrapped Option wrapper decodes an absent key as the wrapped None") {
        KindlingsDecoder.decode[ParityHasOptWrapper](Json.obj()) ==> Right(ParityHasOptWrapper(ParityOptWrapper(None)))
      }

      test("explicit unwrapping rejects case classes without exactly one field") {
        compileErrors(
          """
          import hearth.kindlings.circederivation.{KindlingsEncoder, SimplePerson}
          KindlingsEncoder.deriveUnwrapped[SimplePerson]
          """
        ).check("exactly one field")
      }

      test("multiple parameter lists are rejected at compile time") {
        compileErrors(
          """
          import hearth.kindlings.circederivation.KindlingsEncoder
          case class TwoLists(a: Int)(val b: String)
          KindlingsEncoder.encode(TwoLists(1)("x"))
          """
        ).check("multiple parameter lists")
        compileErrors(
          """
          import hearth.kindlings.circederivation.KindlingsDecoder
          case class TwoLists(a: Int)(val b: String)
          KindlingsDecoder.decode[TwoLists](io.circe.Json.obj())
          """
        ).check("multiple parameter lists")
      }
    }
  }
}
