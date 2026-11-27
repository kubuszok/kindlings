package hearth.kindlings.oracle.tapirparity

import hearth.MacroSuite
import hearth.kindlings.circederivation.Configuration as CirceConfig
import hearth.kindlings.oracle.{Oracle, TapirSchemaDescription}
import hearth.kindlings.tapirschemaderivation.KindlingsSchema
import sttp.tapir.Schema
import sttp.tapir.generic.Configuration as TapirConfig

// Mirrors tapir's core/src/test/scala-3/sttp/tapir/SchemaMacroScala3Test.scala and ValidatorScala3EnumTest.scala (v1.13.33)
enum PtFruit {
  case Apple, Banana
}

@Schema.annotations.customise(_.format("fruit"))
enum PtCustomisedFruit {
  case Apple, Banana
}

enum PtShape3 {
  case Dot
  case Circle(radius: Double)
  case Rect(width: Double, height: Double)
}

final case class PtUsesFruit(fruit: PtFruit)

/** Scala 3-only parts of the tapir parity oracle (see [[TapirParityOracleSpec]]). */
final class TapirParityScala3OracleSpec extends MacroSuite {

  private def compare(kindlings: Schema[?], upstream: Schema[?], expectedDifferences: String*): Unit = {
    val found = Oracle.diff(
      TapirSchemaDescription.describe(kindlings, sortSubtypes = true),
      TapirSchemaDescription.describe(upstream, sortSubtypes = true)
    )
    assertEquals(found, expectedDifferences.toList, s"Kindlings vs tapir Schema differences:\n${found.mkString("\n")}")
  }

  private object Kindlings {
    implicit val circe: CirceConfig = CirceConfig.default.withDiscriminator("type")
    def fruitEnumeration: Schema[PtFruit] = KindlingsSchema.derivedEnumeration[PtFruit].schema
    def customisedFruitEnumeration: Schema[PtCustomisedFruit] =
      KindlingsSchema.derivedEnumeration[PtCustomisedFruit].schema
    def shape: Schema[PtShape3] = KindlingsSchema.derived[PtShape3].schema
    def stringOrInt: Schema[String | Int] = KindlingsSchema.derived[String | Int].schema
  }

  private object Upstream {
    implicit val tapir: TapirConfig = TapirConfig.default.withDiscriminator("type")
    import sttp.tapir.generic.auto.*
    def fruitEnumeration: Schema[PtFruit] = Schema.derivedEnumeration[PtFruit].defaultStringBased
    def customisedFruitEnumeration: Schema[PtCustomisedFruit] =
      Schema.derivedEnumeration[PtCustomisedFruit].defaultStringBased
    def shape: Schema[PtShape3] = Schema.derived[PtShape3]
    def stringOrInt: Schema[String | Int] = Schema.derivedUnion[String | Int]
  }

  test("Schema.derivedEnumeration of a Scala 3 enum (ValidatorScala3EnumTest)") {
    compare(Kindlings.fruitEnumeration, Upstream.fruitEnumeration)
  }

  test("@customise on a Scala 3 enum (SchemaMacroScala3Test)") {
    compare(Kindlings.customisedFruitEnumeration, Upstream.customisedFruitEnumeration)
  }

  test("a Scala 3 enum with parameterless and parametrised cases, with a discriminator") {
    // Documented difference: tapir names a parameterless case of a Scala 3 enum without the enum (`pkg.Dot`),
    // Kindlings with it (`pkg.PtShape3.Dot`, like the parametrised cases)
    val prefix = "hearth.kindlings.oracle.tapirparity."
    assertEquals(
      TapirSchemaDescription.subtypeNames(Upstream.shape).flatten.filter(_.endsWith("Dot")),
      List(prefix + "Dot")
    )
    val upstreamRenamed = TapirSchemaDescription.mapNames(TapirSchemaDescription.describe(Upstream.shape)) { name =>
      if name == prefix + "Dot" then prefix + "PtShape3.Dot" else name
    }
    val found = Oracle.diff(
      TapirSchemaDescription.sorted(TapirSchemaDescription.describe(Kindlings.shape)),
      TapirSchemaDescription.sorted(upstreamRenamed)
    )
    assertEquals(found, Nil, found.mkString("\n"))
    List(PtShape3.Dot, PtShape3.Circle(1.0), PtShape3.Rect(1.0, 2.0)).foreach { value =>
      assertEquals(
        TapirSchemaDescription.subtypeOf(Kindlings.shape, value).map(_.map(_.split('.').last)),
        TapirSchemaDescription.subtypeOf(Upstream.shape, value).map(_.map(_.split('.').last)),
        s"for $value"
      )
    }
  }

  test("a schema provided by the user with Schema.oneOfWrapped for a Scala 3 enum is picked up") {
    import sttp.tapir.generic.auto.*
    implicit val fruitSchema: Schema[PtFruit] = Schema.oneOfWrapped[PtFruit]
    val kindlings = {
      implicit val circe: CirceConfig = CirceConfig.default
      KindlingsSchema.derived[PtUsesFruit].schema
    }
    compare(kindlings, implicitly[Schema[PtUsesFruit]])
  }

  // tapir needs `Schema.derivedUnion` (Kindlings derives union types itself); both name the schema `A_or_B`.
  // Documented difference: the members are described the way the JSON codec writes them (circe-derivation: wrapped in
  // an object keyed by the member's type name), tapir describes the plain members.
  test("union types (SchemaMacroScala3Test: derivedUnion)") {
    compare(
      Kindlings.stringOrInt,
      Upstream.stringOrInt,
      "$.subtypes[0].schemaType: kindlings=SProduct upstream=SString",
      "$.subtypes[0].fields: kindlings=List(FieldDescription(java.lang.String,java.lang.String,SchemaDescription(" +
        "SString,None,false,None,None,None,None,false,false,All(),Map(),List(),None,List(),None))) upstream=List()",
      "$.subtypes[1].schemaType: kindlings=SProduct upstream=SInteger",
      "$.subtypes[1].format: kindlings=None upstream=Some(int32)",
      "$.subtypes[1].fields: kindlings=List(FieldDescription(scala.Int,scala.Int,SchemaDescription(SInteger,None," +
        "false,None,None,Some(int32),None,false,false,All(),Map(),List(),None,List(),None))) upstream=List()",
      "$.discriminator: kindlings=Some(type/type -> ) upstream=None"
    )
  }
}
