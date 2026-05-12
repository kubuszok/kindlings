package hearth.kindlings.avroderivation.internal.runtime

import hearth.kindlings.avroderivation.{AvroDecoder, AvroEncoder, AvroSchemaFor}
import org.apache.avro.Schema

object AvroDerivationFactories {

  def schemaForInstance[A](schemaVal: Schema): AvroSchemaFor[A] =
    new AvroSchemaFor[A] {
      val schema: Schema = schemaVal
    }

  def decoderInstance[A](schemaVal: Schema, decodeFn: Any => A): AvroDecoder[A] =
    new AvroDecoder[A] {
      val schema: Schema = schemaVal
      def decode(value: Any): A = decodeFn(value)
    }

  def encoderInstance[A](schemaVal: Schema, encodeFn: A => Any): AvroEncoder[A] =
    new AvroEncoder[A] {
      val schema: Schema = schemaVal
      def encode(value: A): Any = encodeFn(value)
    }
}
