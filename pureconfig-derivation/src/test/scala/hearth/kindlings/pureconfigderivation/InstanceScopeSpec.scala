package hearth.kindlings.pureconfigderivation

import com.typesafe.config.{ConfigFactory, ConfigValueFactory}
import hearth.MacroSuite
import pureconfig.{ConfigCursor, ConfigReader, ConfigWriter}

/** A field type whose reader/writer are `def`s counting how often they are created. */
final case class CountedField(value: Int)
object CountedField {
  @volatile var instancesCreated: Int = 0
  implicit def countedReader: ConfigReader[CountedField] = {
    instancesCreated += 1
    ConfigReader.intConfigReader.map(CountedField(_))
  }
  implicit def countedWriter: ConfigWriter[CountedField] = {
    instancesCreated += 1
    ConfigWriter.intConfigWriter.contramap(_.value)
  }
}
final case class WithCountedField(counted: CountedField, other: Int)

/** Derived readers and writers resolve the instances of their fields once per derived instance, not on every call. */
final class InstanceScopeSpec extends MacroSuite {

  // Calls `op` three times and returns how many field instances the 2nd and 3rd call created.
  private def createdByRepeatedCalls(op: () => Any): Int = {
    op()
    val before = CountedField.instancesCreated
    op()
    op()
    CountedField.instancesCreated - before
  }

  test("KindlingsConfigReader") {
    val reader = KindlingsConfigReader.derived[WithCountedField]
    val cursor = ConfigCursor(ConfigFactory.parseString("{ counted = 1, other = 2 }").root(), Nil)
    reader.from(cursor) ==> Right(WithCountedField(CountedField(1), 2))
    createdByRepeatedCalls(() => reader.from(cursor)) ==> 0
  }

  test("KindlingsConfigReader in strict mode (unknown keys are failures)") {
    implicit val config: PureConfig = PureConfig.default.withStrictDecoding
    val reader = KindlingsConfigReader.derived[WithCountedField]
    val cursor = ConfigCursor(ConfigFactory.parseString("{ counted = 1, other = 2, extra = 3 }").root(), Nil)
    assert(reader.from(cursor).isLeft)
    createdByRepeatedCalls(() => reader.from(cursor)) ==> 0
  }

  test("KindlingsConfigWriter") {
    val writer = KindlingsConfigWriter.derived[WithCountedField]
    val value = WithCountedField(CountedField(1), 2)
    writer.to(value) ==> ConfigValueFactory.fromMap(
      java.util.Map.of("counted", Integer.valueOf(1), "other", Integer.valueOf(2))
    )
    createdByRepeatedCalls(() => writer.to(value)) ==> 0
  }
}
