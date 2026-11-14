package hearth.kindlings.tapirschemaderivation

import hearth.MacroSuite
import hearth.kindlings.tapirschemaderivation.internal.runtime.TapirSchemaUtils
import sttp.tapir.{Schema, SchemaType, Validator}

/** A collection which is not an `Iterable` (like cats' `NonEmptyList`): its elements are reached through `toIterable`.
  */
final case class NotIterableBag(items: List[Int])

/** Unit tests of the runtime helpers the derived schemas are built with. */
final class TapirSchemaUtilsSpec extends MacroSuite {

  /** The `minSize`s of a validator, looking through `all`/`contramap`. */
  private def minSizes(v: Validator[?]): List[Int] = v match {
    case Validator.MinSize(n)   => List(n)
    case Validator.All(vs)      => vs.toList.flatMap(minSizes)
    case Validator.Mapped(w, _) => minSizes(w)
    case _                      => Nil
  }

  private val bagElements: Any => Iterable[Int] = _.asInstanceOf[NotIterableBag].items

  group("nullableSchema") {

    test("is an optional SOption of the element schema, keeping its format and deprecation") {
      val element = Schema.schemaForString.format("email").deprecated(true)
      val schema = TapirSchemaUtils.nullableSchema(element)
      assert(schema.isOptional)
      assertEquals(schema.format, Some("email"))
      assert(schema.deprecated)
      schema.schemaType match {
        case o: SchemaType.SOption[?, ?] =>
          assertEquals(o.element: Any, element: Any)
          val toOption = o.toOption.asInstanceOf[Any => Option[Any]]
          assertEquals(toOption("a"), Some("a"))
          assertEquals(toOption(null), None)
        case other => fail(s"Expected SOption, got: $other")
      }
    }

    test("of a plain element schema has no format and is not deprecated") {
      val schema = TapirSchemaUtils.nullableSchema(Schema.schemaForInt)
      assertEquals(schema.format, Schema.schemaForInt.format)
      assert(!schema.deprecated)
    }
  }

  group("collectionSchemaVia") {

    test("is an optional SArray whose elements are returned by toIterable") {
      val schema = TapirSchemaUtils.collectionSchemaVia[Int](Schema.schemaForInt, bagElements)
      assert(schema.isOptional)
      schema.schemaType match {
        case a: SchemaType.SArray[?, ?] =>
          assertEquals(a.element: Any, Schema.schemaForInt: Any)
          assertEquals(a.asInstanceOf[SchemaType.SArray[Any, Int]].toIterable(NotIterableBag(List(1, 2))), List(1, 2))
        case other => fail(s"Expected SArray, got: $other")
      }
    }

    test("element validators reach the elements of a non-Iterable collection") {
      val schema =
        TapirSchemaUtils.collectionSchemaVia[Int](Schema.schemaForInt.validate(Validator.min(0)), bagElements)
      assertEquals(schema.applyValidation(NotIterableBag(List(1, 2))), Nil)
      assertEquals(schema.applyValidation(NotIterableBag(List(1, -1))).size, 1)
    }
  }

  group("nonEmptyCollectionSchema") {

    test("is required and rejects an empty collection with minSize(1)") {
      val schema = TapirSchemaUtils.nonEmptyCollectionSchema(
        TapirSchemaUtils.collectionSchemaVia[Int](Schema.schemaForInt, bagElements),
        bagElements
      )
      assert(!schema.isOptional)
      assertEquals(minSizes(schema.validator), List(1))
      assertEquals(schema.applyValidation(NotIterableBag(List(1))), Nil)
      assertEquals(schema.applyValidation(NotIterableBag(Nil)).size, 1)
    }

    test("of a map counts its entries") {
      val toEntries: Any => Iterable[Any] = _.asInstanceOf[Map[String, Int]]
      val schema = TapirSchemaUtils.nonEmptyCollectionSchema(TapirSchemaUtils.mapSchema(Schema.schemaForInt), toEntries)
      assert(!schema.isOptional)
      assertEquals(schema.applyValidation(Map("a" -> 1)), Nil)
      assertEquals(schema.applyValidation(Map.empty[String, Int]).size, 1)
    }
  }

  group("setFieldOptional") {

    test("marks a required schema optional and an optional schema required") {
      val required = Schema.schemaForString
      val optional = required.copy(isOptional = true)
      assert(TapirSchemaUtils.setFieldOptional(required.asInstanceOf[Schema[Any]], optional = true).isOptional)
      assert(!TapirSchemaUtils.setFieldOptional(optional.asInstanceOf[Schema[Any]], optional = false).isOptional)
    }

    test("returns the very same schema when it already has the requested optionality") {
      val required = Schema.schemaForString.asInstanceOf[Schema[Any]]
      val optional = required.copy(isOptional = true)
      assert(TapirSchemaUtils.setFieldOptional(required, optional = false) eq required)
      assert(TapirSchemaUtils.setFieldOptional(optional, optional = true) eq optional)
    }
  }
}
