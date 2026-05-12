package hearth.kindlings.benchmarks

import cats.Show
import cats.kernel.Hash
import io.circe.{Encoder, Json}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}

object AnonClassInstances {

  // --- Show (1 method) ---

  val showSimpleCC: Show[SimpleCC] = new Show[SimpleCC] {
    def show(a: SimpleCC): String = HandWrittenImpls.showSimpleCC(a)
  }

  val showPerson: Show[Person] = new Show[Person] {
    def show(a: Person): String = HandWrittenImpls.showPerson(a)
  }

  val showEvent: Show[Event] = new Show[Event] {
    def show(a: Event): String = HandWrittenImpls.showEvent(a)
  }

  // --- Hash (2 methods) ---

  val hashSimpleCC: Hash[SimpleCC] = new Hash[SimpleCC] {
    def hash(x: SimpleCC): Int = HandWrittenImpls.hashSimpleCC(x)
    def eqv(x: SimpleCC, y: SimpleCC): Boolean = HandWrittenImpls.eqvSimpleCC(x, y)
  }

  val hashPerson: Hash[Person] = new Hash[Person] {
    def hash(x: Person): Int = HandWrittenImpls.hashPerson(x)
    def eqv(x: Person, y: Person): Boolean = HandWrittenImpls.eqvPerson(x, y)
  }

  // --- Encoder (1 method) ---

  val encoderSimpleCC: Encoder[SimpleCC] = new Encoder[SimpleCC] {
    def apply(a: SimpleCC): Json = HandWrittenImpls.encodeSimpleCC(a)
  }

  val encoderPerson: Encoder[Person] = new Encoder[Person] {
    def apply(a: Person): Json = HandWrittenImpls.encodePerson(a)
  }

  // --- Functor (polymorphic, 1 method with type params) ---

  val functorSimpleCCBox: cats.Functor[SimpleCCBox] = new cats.Functor[SimpleCCBox] {
    def map[A, B](fa: SimpleCCBox[A])(f: A => B): SimpleCCBox[B] =
      HandWrittenImpls.mapSimpleCCBox(fa, f)
  }

  // --- JsonValueCodec (3 methods) ---

  val codecSimpleCC: JsonValueCodec[SimpleCC] = new JsonValueCodec[SimpleCC] {
    def nullValue: SimpleCC = null
    def decodeValue(in: JsonReader, default: SimpleCC): SimpleCC =
      HandWrittenImpls.decodeSimpleCCJsoniter(in, default)
    def encodeValue(x: SimpleCC, out: JsonWriter): Unit =
      HandWrittenImpls.encodeSimpleCCJsoniter(x, out)
  }
}
