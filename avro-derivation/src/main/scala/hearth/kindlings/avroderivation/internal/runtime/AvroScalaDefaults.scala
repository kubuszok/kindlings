package hearth.kindlings.avroderivation.internal.runtime

import org.apache.avro.{JsonProperties, LogicalTypes, Schema}

import java.util
import scala.jdk.CollectionConverters.*

/** Converts a Scala default value of a case class field into the default value Avro expects for the field's schema (the
  * `AvroConfig.scalaDefaultsInSchema` option, which mirrors avro4s' `DefaultResolver`).
  *
  * Avro requires a union's default to match its FIRST branch, so for a union the branch the default matches is moved to
  * the front (as avro4s does). Values that cannot be represented yield `None`, i.e. no default.
  */
private[avroderivation] object AvroScalaDefaults {

  def resolve(value: Any, schema: Schema): Option[(Schema, AnyRef)] =
    if (schema.getType == Schema.Type.UNION) resolveUnion(value, schema)
    else convert(value, schema).map(schema -> _)

  private def resolveUnion(value: Any, union: Schema): Option[(Schema, AnyRef)] = {
    val branches = union.getTypes.asScala.toList
    val unwrapped = value match {
      case Some(x) => x
      case other   => other
    }
    val candidates =
      if (unwrapped == null || unwrapped == None) branches.filter(_.getType == Schema.Type.NULL)
      else {
        val nonNull = branches.filterNot(_.getType == Schema.Type.NULL)
        unwrapped match {
          // a sealed trait's subtype: try the record/enum named after it first
          case p: Product =>
            val (named, others) = nonNull.partition(b => b.getType != Schema.Type.UNION && isNamed(b, p))
            named ++ others
          case _ => nonNull
        }
      }
    candidates.iterator
      .map(branch => convert(unwrapped, branch).map(branch -> _))
      .collectFirst { case Some((branch, converted)) =>
        val reordered = branch :: branches.filterNot(_ eq branch)
        Schema.createUnion(reordered.asJava) -> converted
      }
  }

  private def convert(value: Any, schema: Schema): Option[AnyRef] = {
    val logicalType = schema.getLogicalType
    (schema.getType, value) match {
      case (Schema.Type.NULL, null | None)            => Some(JsonProperties.NULL_VALUE)
      case (Schema.Type.BOOLEAN, b: Boolean)          => Some(java.lang.Boolean.valueOf(b))
      case (Schema.Type.INT, i: Int)                  => Some(java.lang.Integer.valueOf(i))
      case (Schema.Type.INT, s: Short)                => Some(java.lang.Integer.valueOf(s.toInt))
      case (Schema.Type.INT, b: Byte)                 => Some(java.lang.Integer.valueOf(b.toInt))
      case (Schema.Type.INT, d: java.time.LocalDate)  => Some(java.lang.Integer.valueOf(d.toEpochDay.toInt))
      case (Schema.Type.INT, d: java.sql.Date)        => Some(java.lang.Integer.valueOf(d.toLocalDate.toEpochDay.toInt))
      case (Schema.Type.LONG, l: Long)                => Some(java.lang.Long.valueOf(l))
      case (Schema.Type.LONG, i: Int)                 => Some(java.lang.Long.valueOf(i.toLong))
      case (Schema.Type.LONG, t: java.time.LocalTime) => Some(java.lang.Long.valueOf(t.toNanoOfDay / 1000))
      case (Schema.Type.LONG, i: java.time.Instant)   => Some(timestamp(i.getEpochSecond, i.getNano, logicalType))
      case (Schema.Type.LONG, t: java.sql.Timestamp)  => convert(t.toInstant, schema)
      case (Schema.Type.LONG, o: java.time.OffsetDateTime) => convert(o.toInstant, schema)
      case (Schema.Type.LONG, d: java.time.LocalDateTime)  =>
        Some(timestamp(d.toEpochSecond(java.time.ZoneOffset.UTC), d.getNano, logicalType))
      case (Schema.Type.FLOAT, f: Float)                     => Some(java.lang.Float.valueOf(f))
      case (Schema.Type.DOUBLE, d: Double)                   => Some(java.lang.Double.valueOf(d))
      case (Schema.Type.DOUBLE, f: Float)                    => Some(java.lang.Double.valueOf(f.toDouble))
      case (Schema.Type.STRING, o: java.time.OffsetDateTime) =>
        Some(o.format(java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME))
      case (Schema.Type.STRING, s @ (_: String | _: Char | _: java.util.UUID | _: BigDecimal))   => Some(s.toString)
      case (Schema.Type.BYTES, bd: BigDecimal) if logicalType.isInstanceOf[LogicalTypes.Decimal] =>
        val scale = logicalType.asInstanceOf[LogicalTypes.Decimal].getScale
        Some(bd.bigDecimal.setScale(scale).unscaledValue.toByteArray)
      case (Schema.Type.BYTES | Schema.Type.FIXED, bytes: Array[Byte]) => Some(bytes)
      case (Schema.Type.BYTES, bb: java.nio.ByteBuffer) => Some(AvroDerivationUtils.decodeByteBuffer(bb.duplicate()))
      case (Schema.Type.BYTES, bytes: Seq[?]) if bytes.forall(_.isInstanceOf[Byte]) =>
        Some(bytes.asInstanceOf[Seq[Byte]].toArray)
      case (Schema.Type.ENUM, e: java.lang.Enum[?])              => enumSymbol(e.name, schema)
      case (Schema.Type.ENUM, e: scala.Enumeration#Value)        => enumSymbol(e.toString, schema)
      case (Schema.Type.ENUM, p: Product) if p.productArity == 0 => enumSymbol(simpleName(p), schema)
      case (Schema.Type.ARRAY, items: Iterable[?])               =>
        sequence(items.toList.map(convert(_, schema.getElementType))).map(_.asJava)
      case (Schema.Type.ARRAY, items: Array[?]) =>
        sequence(items.toList.map(convert(_, schema.getElementType))).map(_.asJava)
      case (Schema.Type.MAP, map: collection.Map[?, ?]) =>
        val entries = map.toList.map { case (k, v) => convert(v, schema.getValueType).map(k.toString -> _) }
        sequence(entries).map { converted =>
          val result = new util.LinkedHashMap[String, AnyRef]()
          converted.foreach { case (k, v) => result.put(k, v) }
          result
        }
      case (Schema.Type.RECORD, p: Product) => record(p, schema)
      case (Schema.Type.UNION, _)           => resolveUnion(value, schema).map(_._2)
      // value classes are encoded as their underlying value
      case (_, p: Product) if p.productArity == 1 && !p.isInstanceOf[Option[?]] => convert(p.productElement(0), schema)
      case _                                                                    => None
    }
  }

  private def record(p: Product, schema: Schema): Option[AnyRef] = {
    val fields = schema.getFields.asScala.toList
    val names = p.productElementNames.toList
    val values = p.productIterator.toList
    // Fields are matched by name, falling back to the position when every element has a field (renamed fields).
    val byName = names.zip(values).toMap
    val matched: List[Option[(String, AnyRef)]] =
      if (fields.forall(f => byName.contains(f.name)))
        fields.map(f => convert(byName(f.name), f.schema).map(f.name -> _))
      else if (fields.size == values.size)
        fields.zip(values).map { case (f, v) => convert(v, f.schema).map(f.name -> _) }
      else List(None)
    sequence(matched).map { converted =>
      val result = new util.LinkedHashMap[String, AnyRef]()
      converted.foreach { case (k, v) => result.put(k, v) }
      result
    }
  }

  // A default out of the range of a long (e.g. `Instant.MIN`/`Instant.MAX`) saturates, as avro4s' defaults do.
  private def timestamp(epochSecond: Long, nano: Int, logicalType: org.apache.avro.LogicalType): AnyRef =
    try
      logicalType match {
        case _: LogicalTypes.TimestampMicros =>
          java.lang.Long.valueOf(Math.addExact(Math.multiplyExact(epochSecond, 1000000L), (nano / 1000).toLong))
        case _: LogicalTypes.TimestampNanos =>
          java.lang.Long.valueOf(Math.addExact(Math.multiplyExact(epochSecond, 1000000000L), nano.toLong))
        case _ =>
          java.lang.Long.valueOf(Math.addExact(Math.multiplyExact(epochSecond, 1000L), (nano / 1000000).toLong))
      }
    catch {
      case _: ArithmeticException => java.lang.Long.valueOf(if (epochSecond < 0) Long.MinValue else Long.MaxValue)
    }

  private def enumSymbol(name: String, schema: Schema): Option[AnyRef] =
    if (schema.hasEnumSymbol(name)) Some(name) else None

  private def simpleName(p: Product): String = p.productPrefix

  private def isNamed(schema: Schema, p: Product): Boolean =
    (schema.getType == Schema.Type.RECORD || schema.getType == Schema.Type.ENUM) && schema.getName == simpleName(p)

  private def sequence[A](options: List[Option[A]]): Option[List[A]] =
    options.foldRight(Option(List.empty[A])) { (o, acc) =>
      for { a <- o; rest <- acc } yield a :: rest
    }
}
