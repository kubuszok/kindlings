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

  // --- avro4s' own tests that only avro4s 4 can derive (value classes, scala.Enumeration, a working FieldMapper) ---

  lazy val docValueType: Option[AvroSide[AuDocUsesValueType]] = None
  lazy val fixedMessage: Option[AvroSide[AuFixedMessage]] = None
  lazy val optionOfFixed: Option[AvroSide[AuOptionOfFixed]] = None
  lazy val genOuter: Option[AvroSide[AuGenOuter[String, Int]]] = None
  lazy val scalaEnums: Option[AvroSide[AuScalaEnums]] = None
  lazy val topScalaEnum: Option[AvroSide[AuColours.Value]] = None
  lazy val pascalCase: Option[AvroSide[AuNamingStrategy]] = None
  lazy val snakeCase: Option[AvroSide[AuNamingStrategy]] = None
  lazy val mapperWithName: Option[AvroSide[AuMapperWithName]] = None
  lazy val javaEnums: Option[AvroSide[AuJavaEnums]] = None
  lazy val topJavaEnum: Option[AvroSide[AuWine]] = None
  lazy val annotatedJavaEnum: Option[AvroSide[AuWineWithAnnotations]] = None

  /** Decimals rounded HALF_UP: what avro4s 5 always does (it has no rounding mode setting). */
  lazy val decimalHalfUp: AvroSide[AuDecimal] = side[AuDecimal]

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
