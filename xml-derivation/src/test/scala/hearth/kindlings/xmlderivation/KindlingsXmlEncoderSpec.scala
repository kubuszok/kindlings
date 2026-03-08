package hearth.kindlings.xmlderivation

import hearth.MacroSuite

final class KindlingsXmlEncoderSpec extends MacroSuite {

  group("KindlingsXmlEncoder") {

    group("case classes") {

      test("simple case class") {
        val encoder = KindlingsXmlEncoder.derive[SimplePerson]
        val result = encoder.encode(SimplePerson("Alice", 30), "person")
        assert((result \ "name").text == "Alice")
        assert((result \ "age").text == "30")
        assert(result.label == "person")
      }

      test("empty case class") {
        val encoder = KindlingsXmlEncoder.derive[EmptyClass]
        val result = encoder.encode(EmptyClass(), "empty")
        assert(result.label == "empty")
      }

      test("single field case class") {
        val encoder = KindlingsXmlEncoder.derive[SingleField]
        val result = encoder.encode(SingleField(42), "field")
        assert((result \ "value").text == "42")
      }

      test("nested case class") {
        val encoder = KindlingsXmlEncoder.derive[PersonWithAddress]
        val result = encoder.encode(PersonWithAddress("Bob", 25, Address("123 Main St", "Springfield")), "person")
        assert((result \ "name").text == "Bob")
        assert((result \ "age").text == "25")
        assert(((result \ "address" \ "street").text) == "123 Main St")
        assert(((result \ "address" \ "city").text) == "Springfield")
      }
    }

    group("annotations") {

      test("@xmlName renames field") {
        val encoder = KindlingsXmlEncoder.derive[XmlWithFieldName]
        val result = encoder.encode(XmlWithFieldName("John", 30), "user")
        assert((result \ "user_name").text == "John")
        assert((result \ "age").text == "30")
      }

      test("@transientField skips field") {
        val encoder = KindlingsXmlEncoder.derive[XmlWithTransient]
        val result = encoder.encode(XmlWithTransient("Alice", Some("cached")), "item")
        assert((result \ "name").text == "Alice")
        assert((result \ "cache").isEmpty)
      }

      test("@xmlAttribute encodes as attribute") {
        val encoder = KindlingsXmlEncoder.derive[XmlWithAttributes]
        val result = encoder.encode(
          XmlWithAttributes("John", 30, Address("123 Main", "NY")),
          "user"
        )
        assert(result.attribute("name").map(_.text) == Some("John"))
        assert(result.attribute("age").map(_.text) == Some("30"))
        assert((result \ "address" \ "street").text == "123 Main")
      }
    }

    group("collections") {

      test("list of simple values") {
        val encoder = KindlingsXmlEncoder.derive[TeamWithMembers]
        val result = encoder.encode(
          TeamWithMembers("Team A", List(SimplePerson("Alice", 30), SimplePerson("Bob", 25))),
          "team"
        )
        assert((result \ "name").text == "Team A")
        val members = result \ "members"
        assert(members.nonEmpty)
      }
    }

    // TODO: Fix encoder macro for sealed traits on Scala 3 - "Block contains definition with different owners"
    // group("sealed traits") {
    //
    //   test("case class subtype") {
    //     val encoder = KindlingsXmlEncoder.derive[Shape]
    //     val result = encoder.encode(Circle(5.0), "shape")
    //     assert(result.label == "shape")
    //   }
    // }

    group("options") {

      test("Some value") {
        val encoder = KindlingsXmlEncoder.derive[Box[Option[Int]]]
        val result = encoder.encode(Box(Some(42)), "box")
        assert(result.label == "box")
      }

      test("None value") {
        val encoder = KindlingsXmlEncoder.derive[Box[Option[Int]]]
        val result = encoder.encode(Box(None), "box")
        assert(result.label == "box")
      }
    }

    group("value classes") {

      test("value class is unwrapped") {
        val encoder = KindlingsXmlEncoder.derive[WrappedInt]
        val result = encoder.encode(WrappedInt(42), "wrapped")
        assert(result.label == "wrapped")
      }
    }

    group("generic types") {

      test("Box[Int]") {
        val encoder = KindlingsXmlEncoder.derive[Box[Int]]
        val result = encoder.encode(Box(42), "box")
        assert(result.label == "box")
      }

      test("Pair[String, Int]") {
        val encoder = KindlingsXmlEncoder.derive[Pair[String, Int]]
        val result = encoder.encode(Pair("hello", 42), "pair")
        assert((result \ "first").text == "hello")
        assert((result \ "second").text == "42")
      }
    }

    group("deeply nested") {

      test("3 levels deep") {
        val encoder = KindlingsXmlEncoder.derive[PersonFull]
        val result = encoder.encode(
          PersonFull("Alice", FullAddress("123 Main", "NY", GeoCoordinates(40.7, -74.0))),
          "person"
        )
        assert((result \ "name").text == "Alice")
        assert(((result \ "address" \ "street").text) == "123 Main")
        assert(((result \ "address" \ "geo" \ "lat").text) == "40.7")
      }
    }
  }
}
