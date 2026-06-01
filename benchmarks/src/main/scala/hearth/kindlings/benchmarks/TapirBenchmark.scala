package hearth.kindlings.benchmarks

import hearth.kindlings.tapirschemaderivation.{KindlingsSchema, PreferSchemaConfig}
import hearth.kindlings.circederivation.Configuration
import sttp.tapir.{Schema, SchemaType}
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

object KindlingsTapirInstances {
  implicit val preferCirceConfig: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]

  val simpleCCSchema: Schema[SimpleCC] = KindlingsSchema.derived[SimpleCC]
  val personSchema: Schema[Person] = KindlingsSchema.derived[Person]
  val eventSchema: Schema[Event] = KindlingsSchema.derived[Event]
  val simpleADTSchema: Schema[SimpleADT] = KindlingsSchema.derived[SimpleADT]
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
