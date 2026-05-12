package hearth.kindlings.benchmarks

import cats.Show
import cats.kernel.Hash
import io.circe.{Encoder, Json}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec

object FactoryInstances {

  // --- Show (1 method → 1 lambda) ---

  val showSimpleCC: Show[SimpleCC] =
    TypeClassFactories.showInstance(HandWrittenImpls.showSimpleCC)

  val showPerson: Show[Person] =
    TypeClassFactories.showInstance(HandWrittenImpls.showPerson)

  val showEvent: Show[Event] =
    TypeClassFactories.showInstance(HandWrittenImpls.showEvent)

  // --- Hash (2 methods → 2 lambdas) ---

  val hashSimpleCC: Hash[SimpleCC] =
    TypeClassFactories.hashInstance(HandWrittenImpls.hashSimpleCC, HandWrittenImpls.eqvSimpleCC)

  val hashPerson: Hash[Person] =
    TypeClassFactories.hashInstance(HandWrittenImpls.hashPerson, HandWrittenImpls.eqvPerson)

  // --- Encoder (1 method → 1 lambda) ---

  val encoderSimpleCC: Encoder[SimpleCC] =
    TypeClassFactories.encoderInstance(HandWrittenImpls.encodeSimpleCC)

  val encoderPerson: Encoder[Person] =
    TypeClassFactories.encoderInstance(HandWrittenImpls.encodePerson)

  // --- Functor (polymorphic, erasure-based factory) ---

  val functorSimpleCCBox: cats.Functor[SimpleCCBox] =
    TypeClassFactories.functorInstance[SimpleCCBox]((fa, f) => HandWrittenImpls.mapSimpleCCBox(fa, f))

  // --- JsonValueCodec (3 methods → 2 lambdas + 1 value) ---

  val codecSimpleCC: JsonValueCodec[SimpleCC] =
    TypeClassFactories.codecInstance(
      null,
      HandWrittenImpls.decodeSimpleCCJsoniter,
      HandWrittenImpls.encodeSimpleCCJsoniter
    )
}
