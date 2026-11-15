package hearth.kindlings.oracle.avro

import com.sksamuel.avro4s.{Decoder, Encoder, SchemaFor}

/** avro4s v5 (the Scala 3 line): encoders/decoders are functions of the schema they are given. */
object AvroUpstream {

  val majorVersion: Int = 5

  def side[A](implicit schemaFor: SchemaFor[A], encoder: Encoder[A], decoder: Decoder[A]): AvroSide[A] = {
    val schema = schemaFor.schema
    new AvroSide[A](schema, encoder.encode(schema), decoder.decode(schema))
  }

  /** avro4s 5 cannot derive recursive types. */
  lazy val tree: Option[AvroSide[AvTree]] = None

  /** avro4s 5 cannot derive value classes. */
  lazy val usesId: Option[AvroSide[AvUsesId]] = None

  /** avro4s 5 has no scala.Enumeration support. */
  lazy val weekday: Option[AvroSide[AvUsesWeekday]] = None

  /** avro4s 5's `FieldMapper` renames schema fields but its record encoders still look fields up by label. */
  lazy val camel: Option[AvroSide[AvCamel]] = None

  /** Instants as timestamp-micros: avro4s 5's encoders/decoders follow the logical type of the schema they get. */
  lazy val instantsMicros: AvroSide[AvInstants] = {
    val micros = SchemaFor[java.time.Instant](
      org.apache.avro.LogicalTypes.timestampMicros().addToSchema(org.apache.avro.SchemaBuilder.builder.longType)
    )
    given SchemaFor[java.time.Instant] = micros
    given SchemaFor[java.sql.Timestamp] = micros.forType
    side[AvInstants]
  }
}
