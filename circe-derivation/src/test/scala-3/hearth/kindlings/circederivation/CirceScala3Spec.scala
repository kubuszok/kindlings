package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.{Decoder, Encoder, Json}

final class CirceScala3Spec extends MacroSuite {

  group("Scala 3 enums") {

    group("encoding") {

      test("enum variant with fields (wrapper-style)") {
        KindlingsEncoder.encode[Fruit](Fruit.Apple(1.5)) ==>
          Json.obj("Apple" -> Json.obj("weight" -> Json.fromDoubleOrNull(1.5)))
      }

      test("second enum variant (wrapper-style)") {
        KindlingsEncoder.encode[Fruit](Fruit.Banana(20.0)) ==>
          Json.obj("Banana" -> Json.obj("length" -> Json.fromDoubleOrNull(20.0)))
      }

      test("enum with discriminator") {
        implicit val config: Configuration = Configuration(discriminator = Some("type"))
        KindlingsEncoder.encode[Fruit](Fruit.Banana(20.0)) ==>
          Json.obj(
            "type" -> Json.fromString("Banana"),
            "length" -> Json.fromDoubleOrNull(20.0)
          )
      }

      test("enum with custom constructor name transform") {
        implicit val config: Configuration =
          Configuration(transformConstructorNames = _.toLowerCase)
        KindlingsEncoder.encode[Fruit](Fruit.Apple(1.5)) ==>
          Json.obj("apple" -> Json.obj("weight" -> Json.fromDoubleOrNull(1.5)))
      }
    }

    group("decoding") {

      test("enum variant with fields (wrapper-style)") {
        val json = Json.obj("Banana" -> Json.obj("length" -> Json.fromDoubleOrNull(20.0)))
        KindlingsDecoder.decode[Fruit](json) ==> Right(Fruit.Banana(20.0))
      }

      test("second enum variant (wrapper-style)") {
        val json = Json.obj("Apple" -> Json.obj("weight" -> Json.fromDoubleOrNull(1.5)))
        KindlingsDecoder.decode[Fruit](json) ==> Right(Fruit.Apple(1.5))
      }

      test("enum with discriminator") {
        implicit val config: Configuration = Configuration(discriminator = Some("type"))
        val json = Json.obj(
          "type" -> Json.fromString("Apple"),
          "weight" -> Json.fromDoubleOrNull(1.5)
        )
        KindlingsDecoder.decode[Fruit](json) ==> Right(Fruit.Apple(1.5))
      }

      test("enum with custom constructor name transform") {
        implicit val config: Configuration =
          Configuration(transformConstructorNames = _.toLowerCase)
        val json = Json.obj("banana" -> Json.obj("length" -> Json.fromDoubleOrNull(20.0)))
        KindlingsDecoder.decode[Fruit](json) ==> Right(Fruit.Banana(20.0))
      }
    }
  }

  group("opaque types") {

    test("encode standalone opaque type") {
      import OpaqueTypes.*
      KindlingsEncoder.encode(UserId(42)) ==> Json.fromInt(42)
    }

    test("decode standalone opaque type") {
      import OpaqueTypes.*
      KindlingsDecoder.decode[UserId](Json.fromInt(42)) ==> Right(UserId(42))
    }

    test("encode case class with opaque type field") {
      import OpaqueTypes.*
      KindlingsEncoder.encode(UserWithOpaque(UserId(42), "Alice")) ==>
        Json.obj("id" -> Json.fromInt(42), "name" -> Json.fromString("Alice"))
    }

    test("decode case class with opaque type field") {
      import OpaqueTypes.*
      val json = Json.obj("id" -> Json.fromInt(42), "name" -> Json.fromString("Alice"))
      KindlingsDecoder.decode[UserWithOpaque](json) ==> Right(UserWithOpaque(UserId(42), "Alice"))
    }
  }

  group("parameterless Scala 3 enums") {

    test("encode parameterless enum") {
      KindlingsEncoder.encode[Color](Color.Red) ==> Json.obj("Red" -> Json.obj())
    }

    test("decode parameterless enum") {
      val json = Json.obj("Green" -> Json.obj())
      KindlingsDecoder.decode[Color](json) ==> Right(Color.Green)
    }
  }

  group("named tuples (Scala 3.7+)") {

    group("encoding") {

      test("simple named tuple") {
        val nt: (name: String, age: Int) = ("Alice", 42)
        KindlingsEncoder.encode(nt) ==>
          Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(42))
      }

      test("named tuple with nested case class") {
        val nt: (person: SimplePerson, score: Int) = (SimplePerson("Bob", 25), 100)
        KindlingsEncoder.encode(nt) ==>
          Json.obj(
            "person" -> Json.obj("name" -> Json.fromString("Bob"), "age" -> Json.fromInt(25)),
            "score" -> Json.fromInt(100)
          )
      }

      test("named tuple with member name transform") {
        implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames
        val nt: (firstName: String, lastName: String) = ("Alice", "Smith")
        KindlingsEncoder.encode(nt) ==>
          Json.obj("first_name" -> Json.fromString("Alice"), "last_name" -> Json.fromString("Smith"))
      }
    }

    group("decoding") {

      test("simple named tuple") {
        val json = Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(42))
        KindlingsDecoder.decode[(name: String, age: Int)](json) ==> Right(("Alice", 42))
      }

      test("named tuple with nested case class") {
        val json = Json.obj(
          "person" -> Json.obj("name" -> Json.fromString("Bob"), "age" -> Json.fromInt(25)),
          "score" -> Json.fromInt(100)
        )
        KindlingsDecoder.decode[(person: SimplePerson, score: Int)](json) ==> Right((SimplePerson("Bob", 25), 100))
      }

      test("named tuple with member name transform") {
        implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames
        val json = Json.obj("first_name" -> Json.fromString("Alice"), "last_name" -> Json.fromString("Smith"))
        KindlingsDecoder.decode[(firstName: String, lastName: String)](json) ==> Right(("Alice", "Smith"))
      }
    }
  }

  group("IArray (Scala 3)") {

    test("IArray[Int] encodes as JSON array") {
      val value: IArray[Int] = IArray(1, 2, 3)
      val json = KindlingsEncoder.encode(value)
      json ==> Json.arr(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3))
    }

    test("IArray[Int] decodes from JSON array") {
      val json = Json.arr(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3))
      val decoded = KindlingsDecoder.decode[IArray[Int]](json)
      assert(decoded.isRight)
      assert(decoded.toOption.get.toSeq == Seq(1, 2, 3))
    }

    test("IArray[Int] round-trip") {
      val original: IArray[Int] = IArray(1, 2, 3)
      val json = KindlingsEncoder.encode(original)
      val decoded = KindlingsDecoder.decode[IArray[Int]](json)
      assert(decoded.isRight)
      assert(decoded.toOption.get.toSeq == original.toSeq)
    }
  }

  group("literal types") {

    group("encoding") {

      test("case class with literal String field") {
        KindlingsEncoder.encode(WithLiteralString("hello", "Alice")) ==>
          Json.obj("tag" -> Json.fromString("hello"), "name" -> Json.fromString("Alice"))
      }

      test("case class with literal Int field") {
        KindlingsEncoder.encode(WithLiteralInt(42, "Bob")) ==>
          Json.obj("code" -> Json.fromInt(42), "name" -> Json.fromString("Bob"))
      }

      test("case class with literal Boolean field") {
        KindlingsEncoder.encode(WithLiteralBoolean(true, "Carol")) ==>
          Json.obj("flag" -> Json.fromBoolean(true), "name" -> Json.fromString("Carol"))
      }
    }

    group("decoding") {

      test("case class with literal String field") {
        val json = Json.obj("tag" -> Json.fromString("hello"), "name" -> Json.fromString("Alice"))
        KindlingsDecoder.decode[WithLiteralString](json) ==> Right(WithLiteralString("hello", "Alice"))
      }

      test("case class with literal Int field") {
        val json = Json.obj("code" -> Json.fromInt(42), "name" -> Json.fromString("Bob"))
        KindlingsDecoder.decode[WithLiteralInt](json) ==> Right(WithLiteralInt(42, "Bob"))
      }

      test("decode literal String with wrong value fails") {
        val json = Json.obj("tag" -> Json.fromString("wrong"), "name" -> Json.fromString("Alice"))
        val result = KindlingsDecoder.decode[WithLiteralString](json)
        assert(result.isLeft)
      }

      test("decode literal Int with wrong value fails") {
        val json = Json.obj("code" -> Json.fromInt(99), "name" -> Json.fromString("Bob"))
        val result = KindlingsDecoder.decode[WithLiteralInt](json)
        assert(result.isLeft)
      }
    }
  }

  group("union types (Scala 3)") {

    // Union type member names use fully-qualified names from Hearth's directChildren
    val StringFQN = "java.lang.String"
    val IntFQN = "scala.Int"
    val ParrotFQN = "hearth.kindlings.circederivation.Parrot"
    val HamsterFQN = "hearth.kindlings.circederivation.Hamster"

    group("encoding") {

      test("String member of String | Int") {
        val json = KindlingsEncoder.encode[StringOrInt]("hello")
        json ==> Json.obj(StringFQN -> Json.fromString("hello"))
      }

      test("Int member of String | Int") {
        val json = KindlingsEncoder.encode[StringOrInt](42)
        json ==> Json.obj(IntFQN -> Json.fromInt(42))
      }

      test("case class member of union") {
        val json = KindlingsEncoder.encode[ParrotOrHamster](Parrot("Polly", 100))
        json ==> Json.obj(ParrotFQN -> Json.obj("name" -> Json.fromString("Polly"), "vocabulary" -> Json.fromInt(100)))
      }

      test("second case class member of union") {
        val json = KindlingsEncoder.encode[ParrotOrHamster](Hamster("Biscuit", 7.5))
        json ==> Json.obj(
          HamsterFQN -> Json.obj("name" -> Json.fromString("Biscuit"), "wheelSize" -> Json.fromDoubleOrNull(7.5))
        )
      }
    }

    group("decoding") {

      test("String member of String | Int") {
        val json = Json.obj(StringFQN -> Json.fromString("hello"))
        KindlingsDecoder.decode[StringOrInt](json) ==> Right("hello": StringOrInt)
      }

      test("Int member of String | Int") {
        val json = Json.obj(IntFQN -> Json.fromInt(42))
        KindlingsDecoder.decode[StringOrInt](json) ==> Right(42: StringOrInt)
      }

      test("case class member of union") {
        val json =
          Json.obj(ParrotFQN -> Json.obj("name" -> Json.fromString("Polly"), "vocabulary" -> Json.fromInt(100)))
        KindlingsDecoder.decode[ParrotOrHamster](json) ==> Right(Parrot("Polly", 100): ParrotOrHamster)
      }

      test("second case class member of union") {
        val json = Json.obj(
          HamsterFQN -> Json.obj("name" -> Json.fromString("Biscuit"), "wheelSize" -> Json.fromDoubleOrNull(7.5))
        )
        KindlingsDecoder.decode[ParrotOrHamster](json) ==> Right(Hamster("Biscuit", 7.5): ParrotOrHamster)
      }
    }
  }

  group("auto-derivation isolation") {

    group("encoder uses kindlings derivation, not circe auto-derivation") {

      test("constructor name transforms are applied") {
        implicit val config: Configuration =
          Configuration(transformConstructorNames = _.toLowerCase)
        // If circe's auto-derivation were used, the constructor name would remain "Circle"
        KindlingsEncoder.encode[Shape](Circle(5.0)) ==>
          Json.obj("circle" -> Json.obj("radius" -> Json.fromDoubleOrNull(5.0)))
      }

      test("member name transforms are applied") {
        implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames
        KindlingsEncoder.encode(PersonWithAddress("Bob", 25, Address("123 Main", "SF"))) ==>
          Json.obj(
            "name" -> Json.fromString("Bob"),
            "age" -> Json.fromInt(25),
            "address" -> Json.obj("street" -> Json.fromString("123 Main"), "city" -> Json.fromString("SF"))
          )
      }
    }

    group("decoder uses kindlings derivation, not circe auto-derivation") {

      test("constructor name transforms are applied") {
        implicit val config: Configuration =
          Configuration(transformConstructorNames = _.toLowerCase)
        val json = Json.obj("circle" -> Json.obj("radius" -> Json.fromDoubleOrNull(5.0)))
        // If circe's auto-derivation were used, it would look for "Circle" not "circle"
        KindlingsDecoder.decode[Shape](json) ==> Right(Circle(5.0): Shape)
      }
    }
  }
}
