package hearth.kindlings.oracle.avro

import com.sksamuel.avro4s.{Decoder, Encoder, SchemaFor}

/** avro4s v4 (the Scala 2.13 line): encoders/decoders carry their own schema. */
object AvroUpstream {

  val majorVersion: Int = 4

  def side[A](implicit schemaFor: SchemaFor[A], encoder: Encoder[A], decoder: Decoder[A]): AvroSide[A] =
    new AvroSide[A](schemaFor.schema, a => encoder.encode(a), v => decoder.decode(v))

  lazy val tree: Option[AvroSide[AvTree]] = Some(side[AvTree])

  lazy val usesId: Option[AvroSide[AvUsesId]] = Some(side[AvUsesId])

  lazy val weekday: Option[AvroSide[AvUsesWeekday]] = Some(side[AvUsesWeekday])

  lazy val camel: Option[AvroSide[AvCamel]] = {
    implicit val fieldMapper: com.sksamuel.avro4s.FieldMapper = com.sksamuel.avro4s.SnakeCase
    Some(side[AvCamel])
  }

  /** Instants as timestamp-micros: avro4s 4 takes the logical type from the encoders'/decoders' schema. */
  lazy val instantsMicros: AvroSide[AvInstants] = {
    val micros = SchemaFor[java.time.Instant](
      org.apache.avro.LogicalTypes.timestampMicros().addToSchema(org.apache.avro.SchemaBuilder.builder.longType)
    )
    implicit val instantSchema: SchemaFor[java.time.Instant] = micros
    implicit val timestampSchema: SchemaFor[java.sql.Timestamp] = micros.forType
    implicit val instantEncoder: Encoder[java.time.Instant] = Encoder.InstantEncoder.withSchema(micros)
    implicit val timestampEncoder: Encoder[java.sql.Timestamp] = Encoder.TimestampEncoder.withSchema(micros.forType)
    implicit val instantDecoder: Decoder[java.time.Instant] = Decoder.InstantDecoder.withSchema(micros)
    implicit val timestampDecoder: Decoder[java.sql.Timestamp] = Decoder.TimestampDecoder.withSchema(micros.forType)
    side[AvInstants]
  }
}
