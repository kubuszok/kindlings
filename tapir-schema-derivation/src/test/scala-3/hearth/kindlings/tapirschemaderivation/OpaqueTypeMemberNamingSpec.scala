package hearth.kindlings.tapirschemaderivation

import hearth.MacroSuite
import hearth.kindlings.circederivation.Configuration
import io.circe.KeyEncoder
import sttp.tapir.{Schema, SchemaType}

// The encoding of Iron's `RefinedType` (`object Key extends RefinedType[String, C]; type Key = Key.T`): an opaque type
// member `T` declared in a trait the companion extends, so that all such types share the short name `T`.
trait OtmRefined[A] {
  opaque type T = A
  def apply(value: A): T = value
  extension (t: T) def value: A = t
}
object OtmKeyA extends OtmRefined[String] {
  given KeyEncoder[T] = KeyEncoder.instance(_.value)
}
type OtmKeyA = OtmKeyA.T
object OtmKeyB extends OtmRefined[String] {
  given KeyEncoder[T] = KeyEncoder.instance(_.value)
}
type OtmKeyB = OtmKeyB.T
object OtmValA extends OtmRefined[Int] {
  given Schema[T] = Schema.schemaForInt.as[T]
}
type OtmValA = OtmValA.T
object OtmValB extends OtmRefined[String] {
  given Schema[T] = Schema.schemaForString.as[T]
}
type OtmValB = OtmValB.T

final case class OtmMaps(first: Map[OtmKeyA, OtmValA], second: Map[OtmKeyB, OtmValB], third: Map[String, OtmValB])

/** Opaque type members of objects (Iron `RefinedType`-like) are named after their object: `Map_OtmKeyA_OtmValA` instead
  * of `Map_T_T` (and two different maps no longer share that name).
  */
final class OpaqueTypeMemberNamingSpec extends MacroSuite {

  given Configuration = Configuration.default

  private val pkg = "hearth.kindlings.tapirschemaderivation"

  test("maps keyed and valued by opaque type members of objects are named after the objects") {
    val names = KindlingsSchema.derived[OtmMaps].schema.schemaType match {
      case p: SchemaType.SProduct[?] => p.fields.map(f => f.name.name -> f.schema.name.get).toMap
      case other                     => fail(s"Expected SProduct, got: $other")
    }
    // the short names are what OpenAPI component names are made of (the full ones go through the file's `$package`)
    def shortNames(name: Schema.SName): List[String] =
      (name.fullName :: name.typeParameterShortNames).map(_.split('.').last)
    assertEquals(shortNames(names("first")), List("Map", "OtmKeyA", "OtmValA"))
    assertEquals(shortNames(names("second")), List("Map", "OtmKeyB", "OtmValB"))
    assertEquals(shortNames(names("third")), List("Map", "OtmValB"))
    assert(names("first").typeParameterShortNames.forall(_.startsWith(pkg + ".")))
    assertNotEquals(names("first"), names("second"))
  }
}
