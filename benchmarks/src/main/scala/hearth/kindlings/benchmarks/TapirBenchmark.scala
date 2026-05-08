package hearth.kindlings.benchmarks

import hearth.kindlings.tapirschemaderivation.{KindlingsSchema, PreferSchemaConfig}
import hearth.kindlings.circederivation.Configuration
import sttp.tapir.{Schema, SchemaType}
import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

object KindlingsTapirInstances {
  implicit val preferCirceConfig: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]

  val simpleCCSchema: Schema[SimpleCC] = KindlingsSchema.derive[SimpleCC]
  val personSchema: Schema[Person] = KindlingsSchema.derive[Person]
  val eventSchema: Schema[Event] = KindlingsSchema.derive[Event]
  val simpleADTSchema: Schema[SimpleADT] = KindlingsSchema.derive[SimpleADT]
}

object OriginalTapirInstances {
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

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class TapirSchemaBenchmark {

  @Benchmark def kindlingsSimpleCC(): SchemaType[SimpleCC] =
    KindlingsTapirInstances.simpleCCSchema.schemaType

  @Benchmark def originalSimpleCC(): SchemaType[SimpleCC] =
    OriginalTapirInstances.simpleCCSchema.schemaType

  @Benchmark def kindlingsPerson(): SchemaType[Person] =
    KindlingsTapirInstances.personSchema.schemaType

  @Benchmark def originalPerson(): SchemaType[Person] =
    OriginalTapirInstances.personSchema.schemaType

  @Benchmark def kindlingsEvent(): SchemaType[Event] =
    KindlingsTapirInstances.eventSchema.schemaType

  @Benchmark def originalEvent(): SchemaType[Event] =
    OriginalTapirInstances.eventSchema.schemaType
}
