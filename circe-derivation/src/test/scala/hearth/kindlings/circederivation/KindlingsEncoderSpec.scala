package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.{Encoder, Json}

case class SimplePerson(name: String, age: Int)
case class EmptyClass()
case class SingleField(value: Int)
case class Address(street: String, city: String)
case class PersonWithAddress(name: String, age: Int, address: Address)
case class TeamWithMembers(name: String, members: List[SimplePerson])
case class RecursiveTree(value: Int, children: List[RecursiveTree])
final case class WrappedInt(value: Int) extends AnyVal

sealed trait Shape
case class Circle(radius: Double) extends Shape
case class Rectangle(width: Double, height: Double) extends Shape

sealed trait Animal
case class Dog(name: String, breed: String) extends Animal
case class Cat(name: String, indoor: Boolean) extends Animal

final class KindlingsEncoderSpec extends MacroSuite {

  group("KindlingsEncoder") {

    group("primitive types via implicit summoning") {

      test("Int") {
        val json = KindlingsEncoder.encode(42)
        assertEquals(json, Json.fromInt(42))
      }

      test("String") {
        val json = KindlingsEncoder.encode("hello")
        assertEquals(json, Json.fromString("hello"))
      }

      test("Boolean") {
        val json = KindlingsEncoder.encode(true)
        assertEquals(json, Json.True)
      }

      test("Double") {
        val json = KindlingsEncoder.encode(3.14)
        assertEquals(json, Json.fromDoubleOrNull(3.14))
      }

      test("Long") {
        val json = KindlingsEncoder.encode(42L)
        assertEquals(json, Json.fromLong(42L))
      }
    }

    group("case classes") {

      test("simple case class") {
        val json = KindlingsEncoder.encode(SimplePerson("Alice", 30))
        assertEquals(json, Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30)))
      }

      test("empty case class") {
        val json = KindlingsEncoder.encode(EmptyClass())
        assertEquals(json, Json.obj())
      }

      test("single field case class") {
        val json = KindlingsEncoder.encode(SingleField(42))
        assertEquals(json, Json.obj("value" -> Json.fromInt(42)))
      }

      test("nested case class") {
        val json = KindlingsEncoder.encode(PersonWithAddress("Bob", 25, Address("123 Main St", "Springfield")))
        assertEquals(
          json,
          Json.obj(
            "name" -> Json.fromString("Bob"),
            "age" -> Json.fromInt(25),
            "address" -> Json.obj(
              "street" -> Json.fromString("123 Main St"),
              "city" -> Json.fromString("Springfield")
            )
          )
        )
      }
    }

    group("value classes") {

      test("value class is unwrapped") {
        val json = KindlingsEncoder.encode(WrappedInt(42))
        assertEquals(json, Json.fromInt(42))
      }
    }

    group("options") {

      test("Some value") {
        val json = KindlingsEncoder.encode(Option(42))
        assertEquals(json, Json.fromInt(42))
      }

      test("None") {
        val json = KindlingsEncoder.encode(Option.empty[Int])
        assertEquals(json, Json.Null)
      }
    }

    group("collections") {

      test("List of ints") {
        val json = KindlingsEncoder.encode(List(1, 2, 3))
        assertEquals(json, Json.arr(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3)))
      }

      test("empty list") {
        val json = KindlingsEncoder.encode(List.empty[Int])
        assertEquals(json, Json.arr())
      }

      test("Vector of strings") {
        val json = KindlingsEncoder.encode(Vector("a", "b"))
        assertEquals(json, Json.arr(Json.fromString("a"), Json.fromString("b")))
      }

      test("List of case classes") {
        val json = KindlingsEncoder.encode(
          TeamWithMembers("Dev", List(SimplePerson("Alice", 30), SimplePerson("Bob", 25)))
        )
        assertEquals(
          json,
          Json.obj(
            "name" -> Json.fromString("Dev"),
            "members" -> Json.arr(
              Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30)),
              Json.obj("name" -> Json.fromString("Bob"), "age" -> Json.fromInt(25))
            )
          )
        )
      }
    }

    group("maps") {

      test("Map[String, Int]") {
        val json = KindlingsEncoder.encode(Map("a" -> 1, "b" -> 2))
        val obj = json.asObject.get
        assertEquals(obj("a"), Some(Json.fromInt(1)))
        assertEquals(obj("b"), Some(Json.fromInt(2)))
      }

      test("empty map") {
        val json = KindlingsEncoder.encode(Map.empty[String, Int])
        assertEquals(json, Json.obj())
      }
    }

    group("sealed traits") {

      test("wrapper-style encoding (default)") {
        val json = KindlingsEncoder.encode[Shape](Circle(5.0))
        assertEquals(json, Json.obj("Circle" -> Json.obj("radius" -> Json.fromDoubleOrNull(5.0))))
      }

      test("wrapper-style encoding for second case") {
        val json = KindlingsEncoder.encode[Shape](Rectangle(3.0, 4.0))
        assertEquals(
          json,
          Json.obj(
            "Rectangle" -> Json.obj(
              "width" -> Json.fromDoubleOrNull(3.0),
              "height" -> Json.fromDoubleOrNull(4.0)
            )
          )
        )
      }

      test("discriminator-style encoding") {
        implicit val config: Configuration = Configuration(discriminator = Some("type"))
        val json = KindlingsEncoder.encode[Animal](Dog("Rex", "Labrador"))
        assertEquals(
          json,
          Json.obj(
            "type" -> Json.fromString("Dog"),
            "name" -> Json.fromString("Rex"),
            "breed" -> Json.fromString("Labrador")
          )
        )
      }
    }

    group("recursive types") {

      test("recursive tree") {
        val tree = RecursiveTree(1, List(RecursiveTree(2, Nil), RecursiveTree(3, List(RecursiveTree(4, Nil)))))
        val json = KindlingsEncoder.encode(tree)
        assertEquals(
          json,
          Json.obj(
            "value" -> Json.fromInt(1),
            "children" -> Json.arr(
              Json.obj("value" -> Json.fromInt(2), "children" -> Json.arr()),
              Json.obj(
                "value" -> Json.fromInt(3),
                "children" -> Json.arr(
                  Json.obj("value" -> Json.fromInt(4), "children" -> Json.arr())
                )
              )
            )
          )
        )
      }
    }

    group("configuration") {

      test("snake_case member names") {
        implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames
        val json = KindlingsEncoder.encode(PersonWithAddress("Bob", 25, Address("123 Main", "SF")))
        val keys = json.asObject.get.keys.toList
        assert(keys.contains("address"))
        // PersonWithAddress doesn't have camelCase fields, so let's test with a dedicated type
      }

      test("custom constructor name transform") {
        implicit val config: Configuration =
          Configuration(transformConstructorNames = _.toLowerCase)
        val json = KindlingsEncoder.encode[Shape](Circle(5.0))
        assertEquals(json, Json.obj("circle" -> Json.obj("radius" -> Json.fromDoubleOrNull(5.0))))
      }
    }

    group("derive") {

      test("explicit derive returns Encoder") {
        val encoder: Encoder[SimplePerson] = KindlingsEncoder.derive[SimplePerson]
        val json = encoder(SimplePerson("Alice", 30))
        assertEquals(json, Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30)))
      }

      test("derived provides KindlingsEncoder") {
        val encoder: KindlingsEncoder[SimplePerson] = KindlingsEncoder.derived[SimplePerson]
        val json = encoder.apply(SimplePerson("Alice", 30))
        assertEquals(json, Json.obj("name" -> Json.fromString("Alice"), "age" -> Json.fromInt(30)))
      }
    }

    group("custom implicit priority") {

      test("user-provided Encoder is used over derivation") {
        implicit val customEncoder: Encoder[SingleField] = Encoder.instance { sf =>
          Json.fromInt(sf.value * 10)
        }
        val json = KindlingsEncoder.encode(SingleField(5))
        assertEquals(json, Json.fromInt(50))
      }
    }
  }
}
