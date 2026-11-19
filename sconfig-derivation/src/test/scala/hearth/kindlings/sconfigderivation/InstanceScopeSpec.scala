package hearth.kindlings.sconfigderivation

import hearth.MacroSuite
import org.ekrich.config.ConfigFactory

/** A field type whose reader/writer are `def`s counting how often they are created. */
final case class CountedField(value: Int)
object CountedField {
  @volatile var instancesCreated: Int = 0
  implicit def countedReader: ConfigReader[CountedField] = {
    instancesCreated += 1
    ConfigReader.intReader.map(CountedField(_))
  }
  implicit def countedWriter: ConfigWriter[CountedField] = {
    instancesCreated += 1
    ConfigWriter.intWriter.contramap(_.value)
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

  test("ConfigReader") {
    val reader = ConfigReader.derived[WithCountedField]
    val value = ConfigFactory.parseString("{ counted = 1, other = 2 }").root
    reader.from(value) ==> Right(WithCountedField(CountedField(1), 2))
    createdByRepeatedCalls(() => reader.from(value)) ==> 0
  }

  test("ConfigReader in strict mode (unknown keys are failures)") {
    implicit val config: SConfig = SConfig.default.withStrictDecoding
    val reader = ConfigReader.derived[WithCountedField]
    val value = ConfigFactory.parseString("{ counted = 1, other = 2, extra = 3 }").root
    assert(reader.from(value).isLeft)
    createdByRepeatedCalls(() => reader.from(value)) ==> 0
  }

  // A config only known at runtime (a parameter) whose constructor-name transform counts its calls.
  private var transformCalls = 0
  private def countingConfig(base: SConfig): SConfig =
    base.withTransformConstructorNames { name =>
      transformCalls += 1
      name.toLowerCase
    }
  private def runtimeReader(config: SConfig): ConfigReader[Shape] = {
    implicit val runtimeConfig: SConfig = config
    ConfigReader.derived[Shape]
  }
  private def runtimeWriter(config: SConfig): ConfigWriter[Shape] = {
    implicit val runtimeConfig: SConfig = config
    ConfigWriter.derived[Shape]
  }
  private def callsOfRepeated(op: () => Any): Int = {
    op()
    val before = transformCalls
    op()
    op()
    transformCalls - before
  }

  test("sum types with a runtime config transform the subtype names once per instance") {
    List(SConfig.default, SConfig.default.withWrappedSubtypes).foreach { base =>
      val config = countingConfig(base)
      val reader = runtimeReader(config)
      val writer = runtimeWriter(config)
      val written = writer.to(Rectangle(1.0, 2.0))
      reader.from(written) ==> Right(Rectangle(1.0, 2.0))
      callsOfRepeated(() => reader.from(written)) ==> 0
      callsOfRepeated(() => writer.to(Circle(1.0))) ==> 0
    }
  }

  test("sum types with a runtime config read and write as configured") {
    val discriminated = countingConfig(SConfig.default)
    runtimeWriter(discriminated).to(Circle(1.0)) ==> ConfigFactory.parseString("{ type = circle, radius = 1.0 }").root
    val wrapped = countingConfig(SConfig.default.withWrappedSubtypes)
    runtimeWriter(wrapped).to(Circle(1.0)) ==> ConfigFactory.parseString("{ circle { radius = 1.0 } }").root
    runtimeReader(wrapped).from(ConfigFactory.parseString("{ rectangle { width = 1, height = 2 } }").root) ==>
      Right(Rectangle(1.0, 2.0))
    assert(runtimeReader(wrapped).from(ConfigFactory.parseString("{ square { side = 1 } }").root).isLeft)
  }

  test("ConfigWriter") {
    val writer = ConfigWriter.derived[WithCountedField]
    val value = WithCountedField(CountedField(1), 2)
    writer.to(value) ==> ConfigFactory.parseString("{ counted = 1, other = 2 }").root
    createdByRepeatedCalls(() => writer.to(value)) ==> 0
  }
}
