package hearth.kindlings.benchmarks

import hearth.kindlings.tapirschemaderivation.{KindlingsSchema, PreferSchemaConfig}
import hearth.kindlings.circederivation.Configuration
import sttp.tapir.{Schema, SchemaType, Validator}
import sttp.tapir.Schema.annotations.validate
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

object KindlingsTapirInstances {
  implicit val preferCirceConfig: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]

  val simpleCCSchema: Schema[SimpleCC] = KindlingsSchema.derived[SimpleCC].schema
  val personSchema: Schema[Person] = KindlingsSchema.derived[Person].schema
  val eventSchema: Schema[Event] = KindlingsSchema.derived[Event].schema
  val simpleADTSchema: Schema[SimpleADT] = KindlingsSchema.derived[SimpleADT].schema
}

object OriginalTapirSemiAutoInstances {
  implicit val addressSchema: Schema[Address] = Schema.derived[Address]
  implicit val simpleCCSchema: Schema[SimpleCC] = Schema.derived[SimpleCC]
  implicit val personSchema: Schema[Person] = Schema.derived[Person]

  implicit val fooSchema: Schema[SimpleADT.Foo] = Schema.derived[SimpleADT.Foo]
  implicit val barSchema: Schema[SimpleADT.Bar] = Schema.derived[SimpleADT.Bar]
  implicit val simpleADTSchema: Schema[SimpleADT] = Schema.derived[SimpleADT]

  implicit val userCreatedSchema: Schema[Event.UserCreated] = Schema.derived[Event.UserCreated]
  implicit val userUpdatedSchema: Schema[Event.UserUpdated] = Schema.derived[Event.UserUpdated]
  implicit val userDeletedSchema: Schema[Event.UserDeleted] = Schema.derived[Event.UserDeleted]
  implicit val eventSchema: Schema[Event] = Schema.derived[Event]
}

object OriginalTapirAutoInstances {
  import sttp.tapir.generic.auto.*
  val simpleCCSchema: Schema[SimpleCC] = implicitly
  val personSchema: Schema[Person] = implicitly
  val eventSchema: Schema[Event] = implicitly
  val simpleADTSchema: Schema[SimpleADT] = implicitly
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class TapirSchemaBenchmark {

  @Benchmark def kindlingsSimpleCC(): SchemaType[SimpleCC] =
    KindlingsTapirInstances.simpleCCSchema.schemaType

  @Benchmark def originalSemiAutoSimpleCC(): SchemaType[SimpleCC] =
    OriginalTapirSemiAutoInstances.simpleCCSchema.schemaType

  @Benchmark def originalAutoSimpleCC(): SchemaType[SimpleCC] =
    OriginalTapirAutoInstances.simpleCCSchema.schemaType

  @Benchmark def kindlingsPerson(): SchemaType[Person] =
    KindlingsTapirInstances.personSchema.schemaType

  @Benchmark def originalSemiAutoPerson(): SchemaType[Person] =
    OriginalTapirSemiAutoInstances.personSchema.schemaType

  @Benchmark def originalAutoPerson(): SchemaType[Person] =
    OriginalTapirAutoInstances.personSchema.schemaType

  @Benchmark def kindlingsEvent(): SchemaType[Event] =
    KindlingsTapirInstances.eventSchema.schemaType

  @Benchmark def originalSemiAutoEvent(): SchemaType[Event] =
    OriginalTapirSemiAutoInstances.eventSchema.schemaType

  @Benchmark def originalAutoEvent(): SchemaType[Event] =
    OriginalTapirAutoInstances.eventSchema.schemaType
}

/** A sealed hierarchy with validated fields (validation is skipped for schemas without validators). */
sealed trait TapirValidatedShape
object TapirValidatedShape {
  final case class Circle(@validate[Double](Validator.min(0.0)) radius: Double) extends TapirValidatedShape
  final case class Rect(@validate[Int](Validator.min(0)) width: Int, @validate[Int](Validator.min(0)) height: Int)
      extends TapirValidatedShape
  final case class Labelled(@validate[String](Validator.minLength(1)) label: String, rect: Rect)
      extends TapirValidatedShape
  case object Dot extends TapirValidatedShape
}

object KindlingsTapirValidatedInstances {
  implicit val preferCirceConfig: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]
  val shapeSchema: Schema[TapirValidatedShape] = KindlingsSchema.derived[TapirValidatedShape].schema
}

object OriginalTapirValidatedInstances {
  import sttp.tapir.generic.auto.*
  val shapeSchema: Schema[TapirValidatedShape] = implicitly
}

/** `Schema.applyValidation` of sealed-trait values, which reaches the subtype's schema through
  * `SCoproduct.subtypeSchema` (Kindlings: a generated pattern match on the subtypes; tapir: magnolia's `split` / the
  * Mirror's ordinal, then a lookup of the subtype by its name).
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class TapirValidationBenchmark {
  import TapirValidatedShape.*

  private val valid: Array[TapirValidatedShape] =
    Array(Circle(1.0), Rect(1, 2), Labelled("x", Rect(3, 4)), Dot)
  private val invalid: Array[TapirValidatedShape] =
    Array(Circle(-1.0), Rect(-1, 2), Labelled("", Rect(3, -4)), Dot)

  private def validateAll[A](schema: Schema[A], values: Array[A]): Int = {
    var n = 0
    var i = 0
    while (i < values.length) {
      n += schema.applyValidation(values(i)).size
      i += 1
    }
    n
  }

  @Benchmark def kindlingsValidateValid(): Int = validateAll(KindlingsTapirValidatedInstances.shapeSchema, valid)

  @Benchmark def originalValidateValid(): Int = validateAll(OriginalTapirValidatedInstances.shapeSchema, valid)

  @Benchmark def kindlingsValidateInvalid(): Int = validateAll(KindlingsTapirValidatedInstances.shapeSchema, invalid)

  @Benchmark def originalValidateInvalid(): Int = validateAll(OriginalTapirValidatedInstances.shapeSchema, invalid)
}
