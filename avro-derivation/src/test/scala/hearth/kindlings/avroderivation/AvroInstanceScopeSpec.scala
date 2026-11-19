package hearth.kindlings.avroderivation

import hearth.MacroSuite
import org.apache.avro.{Schema, SchemaBuilder}
import org.apache.avro.generic.GenericRecord

/** A field type whose encoder/decoder are `def`s counting how often they are created. */
final case class AvroCounted(value: Int)
object AvroCounted {
  @volatile var instancesCreated: Int = 0
  // one implicit that is an encoder, a decoder and a schema (two would make `AvroSchemaFor` ambiguous)
  implicit def countedCodec: AvroEncoder[AvroCounted] with AvroDecoder[AvroCounted] = {
    instancesCreated += 1
    new AvroEncoder[AvroCounted] with AvroDecoder[AvroCounted] {
      val schema: Schema = Schema.create(Schema.Type.INT)
      def encode(value: AvroCounted): Any = value.value
      def decode(value: Any): AvroCounted = AvroCounted(value.asInstanceOf[Int])
    }
  }
}
final case class WithAvroCounted(counted: AvroCounted, other: Int)
final case class WithCountedEither(either: Either[AvroCounted, String])

// Nested records: the derived encoder must build each record schema once and reuse it for the encoded nested records.
final case class SharedLeaf(value: Int)
final case class SharedMiddle(leaf: SharedLeaf, name: String)
final case class SharedRoot(middle: SharedMiddle, leaf: SharedLeaf)

// A nested type with a user-provided schema: it must keep winning over a derived one when schemas are shared.
final case class CustomSchemaInner(value: Int)
object CustomSchemaInner {
  implicit val schemaFor: AvroSchemaFor[CustomSchemaInner] = new AvroSchemaFor[CustomSchemaInner] {
    val schema: Schema = SchemaBuilder.record("RenamedInner").fields().requiredInt("value").endRecord()
  }
}
final case class CustomSchemaHolder(inner: CustomSchemaInner)
final case class CustomSchemaOuter(first: CustomSchemaInner, second: CustomSchemaHolder)
// A holder with a user-provided schema but a derived encoder: the encoder derives the holder's schema itself (the
// parent's schema uses the user's one), after the encoder of `first` derived CustomSchemaInner's schema structurally.
// Its `inner` field must still use the user-provided schema of CustomSchemaInner.
final case class CustomSchemaHolder2(inner: CustomSchemaInner)
object CustomSchemaHolder2 {
  implicit val schemaFor: AvroSchemaFor[CustomSchemaHolder2] = new AvroSchemaFor[CustomSchemaHolder2] {
    val schema: Schema =
      SchemaBuilder
        .record("RenamedHolder")
        .fields()
        .name("inner")
        .`type`(CustomSchemaInner.schemaFor.schema)
        .noDefault()
        .endRecord()
  }
}
final case class CustomSchemaOuter2(first: CustomSchemaInner, second: CustomSchemaHolder2)

/** Derived encoders and decoders build schemas and resolve the instances of their fields once per instance, not on
  * every encoded/decoded value.
  */
final class AvroInstanceScopeSpec extends MacroSuite {

  // Calls `op` three times and returns how many field instances the 2nd and 3rd call created.
  private def createdByRepeatedCalls(op: () => Any): Int = {
    op()
    val before = AvroCounted.instancesCreated
    op()
    op()
    AvroCounted.instancesCreated - before
  }

  test("encoder and decoder of a case class with a field instance") {
    val encoder = AvroEncoder.derived[WithAvroCounted]
    val decoder = AvroDecoder.derived[WithAvroCounted]
    val value = WithAvroCounted(AvroCounted(1), 2)
    val encoded = encoder.encode(value)
    decoder.decode(encoded) ==> value
    createdByRepeatedCalls(() => encoder.encode(value)) ==> 0
    createdByRepeatedCalls(() => decoder.decode(encoded)) ==> 0
  }

  test("decoder of an Either (its union schema)") {
    val encoder = AvroEncoder.derived[WithCountedEither]
    val decoder = AvroDecoder.derived[WithCountedEither]
    List(WithCountedEither(Left(AvroCounted(1))), WithCountedEither(Right("x"))).foreach { value =>
      val encoded = encoder.encode(value)
      decoder.decode(encoded) ==> value
      createdByRepeatedCalls(() => decoder.decode(encoded)) ==> 0
    }
  }

  test("the encoder shares the schemas of nested records between its schema and the encoded records") {
    val encoder = AvroEncoder.derived[SharedRoot]
    val value = SharedRoot(SharedMiddle(SharedLeaf(1), "m"), SharedLeaf(2))
    val record = encoder.encode(value).asInstanceOf[GenericRecord]
    val middle = record.get("middle").asInstanceOf[GenericRecord]
    val middleSchema = encoder.schema.getField("middle").schema()
    val leafSchema = encoder.schema.getField("leaf").schema()
    assert(middleSchema.getField("leaf").schema() eq leafSchema)
    assert(middle.getSchema eq middleSchema)
    assert(middle.get("leaf").asInstanceOf[GenericRecord].getSchema eq leafSchema)
    assert(record.get("leaf").asInstanceOf[GenericRecord].getSchema eq leafSchema)
  }

  test("a user-provided AvroSchemaFor of a nested type still wins over the derived schema") {
    def assertCustom(schema: Schema): Unit = {
      schema.getField("first").schema().getName ==> "RenamedInner"
      schema.getField("second").schema().getField("inner").schema().getName ==> "RenamedInner"
    }
    val value = CustomSchemaOuter(CustomSchemaInner(1), CustomSchemaHolder(CustomSchemaInner(2)))
    val encoder = AvroEncoder.derived[CustomSchemaOuter]
    assertCustom(encoder.schema)
    val record = encoder.encode(value).asInstanceOf[GenericRecord]
    record.get("second").asInstanceOf[GenericRecord].getSchema.getField("inner").schema().getName ==> "RenamedInner"
    assertCustom(AvroDecoder.derived[CustomSchemaOuter].schema)
    assertCustom(AvroSchemaFor.derived[CustomSchemaOuter].schema)

    val encoder2 = AvroEncoder.derived[CustomSchemaOuter2]
    encoder2.schema.getField("second").schema().getName ==> "RenamedHolder"
    val record2 = encoder2.encode(CustomSchemaOuter2(CustomSchemaInner(1), CustomSchemaHolder2(CustomSchemaInner(2))))
    record2
      .asInstanceOf[GenericRecord]
      .get("second")
      .asInstanceOf[GenericRecord]
      .getSchema
      .getField("inner")
      .schema()
      .getName ==> "RenamedInner"
  }

  test("enums of case objects round-trip (schema and symbols)") {
    val encoder = AvroEncoder.derived[Color]
    val decoder = AvroDecoder.derived[Color]
    List[Color](Red, Green, Blue).foreach { c =>
      decoder.decode(encoder.encode(c)) ==> c
    }
  }
}
