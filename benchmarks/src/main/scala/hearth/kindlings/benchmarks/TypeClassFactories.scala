package hearth.kindlings.benchmarks

import cats.Show
import cats.kernel.{Eq, Hash}
import io.circe.{Encoder, Json}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonWriter, JsonValueCodec}

object TypeClassFactories {

  sealed trait Witness1
  sealed trait Witness2

  def showInstance[A](f: A => String): Show[A] = new Show[A] {
    def show(a: A): String = f(a)
  }

  def functorInstance[F[_]](
      mapFn: (F[Witness1], Witness1 => Witness2) => F[Witness2]
  ): cats.Functor[F] = new cats.Functor[F] {
    def map[A, B](fa: F[A])(f: A => B): F[B] =
      mapFn.asInstanceOf[(F[A], A => B) => F[B]].apply(fa, f)
  }

  def hashInstance[A](hashFn: A => Int, eqvFn: (A, A) => Boolean): Hash[A] = new Hash[A] {
    def hash(x: A): Int = hashFn(x)
    def eqv(x: A, y: A): Boolean = eqvFn(x, y)
  }

  def encoderInstance[A](f: A => Json): Encoder[A] = new Encoder[A] {
    def apply(a: A): Json = f(a)
  }

  def codecInstance[A](
      nullVal: A,
      decodeFn: (JsonReader, A) => A,
      encodeFn: (A, JsonWriter) => Unit
  ): JsonValueCodec[A] = new JsonValueCodec[A] {
    def nullValue: A = nullVal
    def decodeValue(in: JsonReader, default: A): A = decodeFn(in, default)
    def encodeValue(x: A, out: JsonWriter): Unit = encodeFn(x, out)
  }
}
