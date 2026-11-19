package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.{Encoder, Json}

/** A field type whose encoder is a `def` counting how often it is created. */
final case class EncCountedField(value: Int)
object EncCountedField {
  @volatile var instancesCreated: Int = 0
  implicit def countedEncoder: Encoder[EncCountedField] = {
    instancesCreated += 1
    Encoder.encodeInt.contramap(_.value)
  }
}
final case class EncWithCountedField(counted: EncCountedField, other: Int)
final case class EncWrapsCounted(counted: EncCountedField)

sealed trait EncCountedSum
object EncCountedSum {
  final case class HasCounted(countedValue: EncCountedField) extends EncCountedSum
  final case class Plain(plainValue: Int) extends EncCountedSum
}

/** Instances derived with a configuration only known at runtime (a parameter the macro cannot evaluate). */
object EncRuntimeConfigInstances {
  def encoder(config: Configuration): KindlingsEncoder[EncWithCountedField] = {
    implicit val runtimeConfig: Configuration = config
    KindlingsEncoder.derived[EncWithCountedField]
  }
  def sumEncoder(config: Configuration): KindlingsEncoder[EncCountedSum] = {
    implicit val runtimeConfig: Configuration = config
    KindlingsEncoder.derived[EncCountedSum]
  }
}

/** Derived encoders resolve the instances of their fields (and, with a runtime configuration, the transformed field and
  * constructor names) once per derived instance, not on every encode.
  */
final class EncoderInstanceScopeSpec extends MacroSuite {

  // Calls `op` three times and returns how many field instances the 2nd and 3rd call created.
  private def createdByRepeatedCalls(op: () => Any): Int = {
    op()
    val before = EncCountedField.instancesCreated
    op()
    op()
    EncCountedField.instancesCreated - before
  }

  private val value = EncWithCountedField(EncCountedField(1), 2)
  private val json = Json.obj("counted" -> Json.fromInt(1), "other" -> Json.fromInt(2))

  test("KindlingsEncoder.derived of a case class (Encoder.AsObject)") {
    val encoder = KindlingsEncoder.derived[EncWithCountedField]
    encoder(value) ==> json
    createdByRepeatedCalls(() => encoder(value)) ==> 0
  }

  test("KindlingsEncoder.derived of a non-object type") {
    val encoder = KindlingsEncoder.derived[(EncCountedField, Int)]
    encoder((EncCountedField(1), 2)) ==> Json.arr(Json.fromInt(1), Json.fromInt(2))
    createdByRepeatedCalls(() => encoder((EncCountedField(1), 2))) ==> 0
  }

  test("KindlingsEncoder.derived of a sealed trait") {
    val encoder = KindlingsEncoder.derived[EncCountedSum]
    val sumValue: EncCountedSum = EncCountedSum.HasCounted(EncCountedField(3))
    encoder(sumValue) ==> Json.obj("HasCounted" -> Json.obj("countedValue" -> Json.fromInt(3)))
    createdByRepeatedCalls(() => encoder(sumValue)) ==> 0
  }

  test("KindlingsEncoder.deriveUnwrapped") {
    val encoder = KindlingsEncoder.deriveUnwrapped[EncWrapsCounted]
    encoder(EncWrapsCounted(EncCountedField(4))) ==> Json.fromInt(4)
    createdByRepeatedCalls(() => encoder(EncWrapsCounted(EncCountedField(4)))) ==> 0
  }

  test("KindlingsCodecAsObject.derived") {
    val codec = KindlingsCodecAsObject.derived[EncWithCountedField]
    codec(value) ==> json
    createdByRepeatedCalls(() => codec(value)) ==> 0
  }

  test("a runtime configuration transforms the field names once per instance") {
    var transformed = 0
    val config = Configuration.default.withTransformMemberNames { name =>
      transformed += 1
      name.toUpperCase
    }
    val encoder = EncRuntimeConfigInstances.encoder(config)
    encoder(value) ==> Json.obj("COUNTED" -> Json.fromInt(1), "OTHER" -> Json.fromInt(2))
    val before = transformed
    val _ = encoder(value)
    val _ = encoder(value)
    transformed ==> before
    createdByRepeatedCalls(() => encoder(value)) ==> 0
  }

  test("a runtime configuration transforms the constructor names once per instance") {
    var transformed = 0
    val config = Configuration.default.withDiscriminator("type").withTransformConstructorNames { name =>
      transformed += 1
      name.toLowerCase
    }
    val encoder = EncRuntimeConfigInstances.sumEncoder(config)
    val sumValue: EncCountedSum = EncCountedSum.Plain(5)
    encoder(sumValue) ==> Json.obj("plainValue" -> Json.fromInt(5), "type" -> Json.fromString("plain"))
    val before = transformed
    val _ = encoder(sumValue)
    val _ = encoder(EncCountedSum.HasCounted(EncCountedField(6)))
    transformed ==> before
  }

  test("CirceDerivationUtils.transformNames transforms every name, in order") {
    import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
    CirceDerivationUtils.transformNames(_.toUpperCase, List("a", "bc", "d")).toList ==> List("A", "BC", "D")
    CirceDerivationUtils.transformNames(identity, Nil).toList ==> Nil
  }
}
