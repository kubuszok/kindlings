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

  // --- avro4s' own tests that only avro4s 4 can derive (value classes, scala.Enumeration, a working FieldMapper) ---

  lazy val docValueType: Option[AvroSide[AuDocUsesValueType]] = Some(side[AuDocUsesValueType])
  lazy val fixedMessage: Option[AvroSide[AuFixedMessage]] = Some(side[AuFixedMessage])
  lazy val optionOfFixed: Option[AvroSide[AuOptionOfFixed]] = Some(side[AuOptionOfFixed])
  lazy val genOuter: Option[AvroSide[AuGenOuter[String, Int]]] = Some(side[AuGenOuter[String, Int]])
  lazy val scalaEnums: Option[AvroSide[AuScalaEnums]] = Some(side[AuScalaEnums])
  lazy val topScalaEnum: Option[AvroSide[AuColours.Value]] = Some(side[AuColours.Value])
  lazy val pascalCase: Option[AvroSide[AuNamingStrategy]] = {
    implicit val fieldMapper: com.sksamuel.avro4s.FieldMapper = com.sksamuel.avro4s.PascalCase
    Some(side[AuNamingStrategy])
  }
  lazy val snakeCase: Option[AvroSide[AuNamingStrategy]] = {
    implicit val fieldMapper: com.sksamuel.avro4s.FieldMapper = com.sksamuel.avro4s.SnakeCase
    Some(side[AuNamingStrategy])
  }
  lazy val mapperWithName: Option[AvroSide[AuMapperWithName]] = {
    implicit val fieldMapper: com.sksamuel.avro4s.FieldMapper = com.sksamuel.avro4s.SnakeCase
    Some(side[AuMapperWithName])
  }
  lazy val javaEnums: Option[AvroSide[AuJavaEnums]] = Some(side[AuJavaEnums])
  lazy val topJavaEnum: Option[AvroSide[AuWine]] = Some(side[AuWine])
  lazy val annotatedJavaEnum: Option[AvroSide[AuWineWithAnnotations]] = Some(side[AuWineWithAnnotations])

  // --- avro4s' streams (AvroOutputStream / AvroInputStream) ---

  import com.sksamuel.avro4s.{AvroInputStream, AvroOutputStream}

  private def write[A](stream: java.io.ByteArrayOutputStream => AvroOutputStream[A], values: Seq[A]): Array[Byte] = {
    val out = new java.io.ByteArrayOutputStream()
    val avro = stream(out)
    values.foreach(avro.write)
    avro.close()
    out.toByteArray
  }

  def writeData[A](values: Seq[A])(implicit s: SchemaFor[A], e: Encoder[A]): Array[Byte] =
    write[A](AvroOutputStream.data[A].to(_).build(), values)
  def writeBinary[A](values: Seq[A])(implicit s: SchemaFor[A], e: Encoder[A]): Array[Byte] =
    write[A](AvroOutputStream.binary[A].to(_).build(), values)
  def writeJson[A](values: Seq[A])(implicit s: SchemaFor[A], e: Encoder[A]): Array[Byte] =
    write[A](AvroOutputStream.json[A].to(_).build(), values)

  def readData[A](bytes: Array[Byte])(implicit s: SchemaFor[A], d: Decoder[A]): List[A] =
    AvroInputStream.data[A].from(bytes).build(s.schema).iterator.toList
  def readBinary[A](bytes: Array[Byte])(implicit s: SchemaFor[A], d: Decoder[A]): List[A] =
    AvroInputStream.binary[A].from(bytes).build(s.schema).iterator.toList
  def readJson[A](bytes: Array[Byte])(implicit s: SchemaFor[A], d: Decoder[A]): List[A] =
    AvroInputStream.json[A].from(bytes).build(s.schema).iterator.toList
}
