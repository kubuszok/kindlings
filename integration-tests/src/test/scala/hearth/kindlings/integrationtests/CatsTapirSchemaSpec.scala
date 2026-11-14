package hearth.kindlings.integrationtests

import cats.data.{
  Chain,
  NonEmptyChain,
  NonEmptyLazyList,
  NonEmptyList,
  NonEmptyMap,
  NonEmptySeq,
  NonEmptySet,
  NonEmptyVector
}
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, JsonValueCodec}
import hearth.MacroSuite
import hearth.kindlings.jsoniterderivation.{JsoniterConfig, KindlingsJsonValueCodec}
import hearth.kindlings.tapirschemaderivation.{KindlingsSchema, PreferSchemaConfig}
import sttp.tapir.{Schema, SchemaType, Validator}

import scala.util.Try

case class WithListOfNEL(values: List[NonEmptyList[Int]])
case class WithNELDefault(values: NonEmptyList[Int] = NonEmptyList.one(1))

object CatsTapirSchemas {
  implicit val config: JsoniterConfig = JsoniterConfig.default
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]

  val nel: Schema[WithNEL] = KindlingsSchema.derived[WithNEL].schema
  val nev: Schema[WithNEV] = KindlingsSchema.derived[WithNEV].schema
  val neSeq: Schema[WithNESeq] = KindlingsSchema.derived[WithNESeq].schema
  val nell: Schema[WithNELL] = KindlingsSchema.derived[WithNELL].schema
  val nec: Schema[WithNEC] = KindlingsSchema.derived[WithNEC].schema
  val nem: Schema[WithNEM] = KindlingsSchema.derived[WithNEM].schema
  val nes: Schema[WithNES] = KindlingsSchema.derived[WithNES].schema
  val chain: Schema[WithChain] = KindlingsSchema.derived[WithChain].schema
  val listOfNel: Schema[WithListOfNEL] = KindlingsSchema.derived[WithListOfNEL].schema
  val nelDefault: Schema[WithNELDefault] = KindlingsSchema.derived[WithNELDefault].schema

  val nelCodec: JsonValueCodec[WithNEL] = KindlingsJsonValueCodec.derived[WithNEL]
  val nemCodec: JsonValueCodec[WithNEM] = KindlingsJsonValueCodec.derived[WithNEM]
  val chainCodec: JsonValueCodec[WithChain] = KindlingsJsonValueCodec.derived[WithChain]
  val nelDefaultCodec: JsonValueCodec[WithNELDefault] = KindlingsJsonValueCodec.derived[WithNELDefault]
}

/** Non-empty collections are described as tapir-cats describes them: required, with `Validator.minSize(1)` (`minItems:
  * 1`), matching the jsoniter decoder, which rejects both an absent field and an empty array/object.
  */
final class CatsTapirSchemaSpec extends MacroSuite {
  import CatsTapirSchemas.*

  private def valuesField(schema: Schema[?]): Schema[?] = schema.schemaType match {
    case p: SchemaType.SProduct[?] =>
      p.fields.find(_.name.encodedName == "values").map(f => f.schema: Schema[?]).getOrElse(fail("no values field"))
    case other => fail(s"Expected SProduct, got: $other")
  }

  private def required(schema: Schema[?]): List[String] = schema.schemaType match {
    case p: SchemaType.SProduct[?] => p.required.map(_.encodedName)
    case other                     => fail(s"Expected SProduct, got: $other")
  }

  /** The `minSize`s of a validator, looking through `all`/`contramap` like tapir's docs interpreters do. */
  private def minSizes(schema: Schema[?]): List[Int] = {
    def loop(v: Validator[?]): List[Int] = v match {
      case Validator.MinSize(n)   => List(n)
      case Validator.All(vs)      => vs.toList.flatMap(loop)
      case Validator.Mapped(w, _) => loop(w)
      case _                      => Nil
    }
    loop(schema.validator)
  }

  private def assertNonEmpty(schema: Schema[?], value: Any): Unit = {
    val field = valuesField(schema)
    assertEquals(field.isOptional, false, s"values should be required: $field")
    assertEquals(required(schema), List("values"))
    assertEquals(minSizes(field), List(1))
    assertEquals(field.asInstanceOf[Schema[Any]].applyValidation(value), Nil)
  }

  group("Cats + Tapir Schema: non-empty collections") {

    test("NonEmptyList") {
      assertNonEmpty(nel, NonEmptyList.of(1, 2))
      assert(valuesField(nel).schemaType.isInstanceOf[SchemaType.SArray[?, ?]])
    }
    test("NonEmptyVector")(assertNonEmpty(nev, NonEmptyVector.of(1)))
    test("NonEmptySeq")(assertNonEmpty(neSeq, NonEmptySeq.of(1)))
    test("NonEmptyLazyList")(assertNonEmpty(nell, NonEmptyLazyList(1)))
    test("NonEmptyChain")(assertNonEmpty(nec, NonEmptyChain(1, 2)))
    test("NonEmptySet")(assertNonEmpty(nes, NonEmptySet.of(1, 2)))
    test("NonEmptyMap") {
      assertNonEmpty(nem, NonEmptyMap.of("a" -> 1))
      assert(valuesField(nem).schemaType.isInstanceOf[SchemaType.SOpenProduct[?, ?]])
    }

    test("Chain can be empty: optional, without minSize") {
      val field = valuesField(chain)
      assertEquals(field.isOptional, true)
      assertEquals(minSizes(field), Nil)
    }

    test("a non-empty collection as a collection element is not optional and has minSize(1)") {
      valuesField(listOfNel).schemaType match {
        case a: SchemaType.SArray[?, ?] =>
          assertEquals(a.element.isOptional, false)
          assertEquals(minSizes(a.element), List(1))
        case other => fail(s"Expected SArray, got: $other")
      }
    }

    test("a non-empty collection with a default value is optional (the decoder fills in the default)") {
      val field = valuesField(nelDefault)
      assertEquals(field.isOptional, true)
      assertEquals(minSizes(field), List(1))
      assertEquals(readFromString("{}")(nelDefaultCodec), WithNELDefault())
    }
  }

  group("the schema matches the jsoniter decoder") {

    test("NonEmptyList: absent and empty are rejected, as the schema says") {
      assert(Try(readFromString("{}")(nelCodec)).isFailure)
      assert(Try(readFromString("""{"values":[]}""")(nelCodec)).isFailure)
      assertEquals(readFromString("""{"values":[1]}""")(nelCodec), WithNEL(NonEmptyList.one(1)))
    }

    test("NonEmptyMap: absent and empty are rejected, as the schema says") {
      assert(Try(readFromString("{}")(nemCodec)).isFailure)
      assert(Try(readFromString("""{"values":{}}""")(nemCodec)).isFailure)
    }

    test("Chain: absent and empty are accepted, as the schema says") {
      assertEquals(readFromString("{}")(chainCodec), WithChain(Chain.empty))
      assertEquals(readFromString("""{"values":[]}""")(chainCodec), WithChain(Chain.empty))
    }
  }
}
