package hearth.kindlings.tapirschemaderivation

import hearth.MacroSuite
import hearth.kindlings.circederivation.{Configuration, KindlingsEncoder}
import io.circe.{Encoder, Json}
import sttp.tapir.{FieldName, Schema, SchemaType, Validator}

// Types for the circe codec / tapir schema fidelity tests.

final case class FPlate(value: String) extends AnyVal

// A hierarchy whose intermediate has no instance: flattened by circe.
sealed trait FVehicle extends Any
case class FCar(seats: Int, plate: FPlate) extends FVehicle
sealed trait FMotor extends Any with FVehicle
case class FTruck(load: Double) extends FMotor
case object FScooter extends FMotor
final case class FBadge(code: Int) extends AnyVal with FVehicle

case class FGarage(
    ownerName: String,
    vehicles: List[FVehicle],
    spare: Option[FVehicle],
    counts: Map[String, Int],
    plate: FPlate,
    maybePlate: Option[FPlate]
)

// A hierarchy whose intermediate has a Kindlings-derived encoder (written as its leaves, with a discriminator).
sealed trait FPet
case class FDog(name: String) extends FPet
sealed trait FCatLike extends FPet
case class FCat(lives: Int) extends FCatLike
case class FLion(pride: Int) extends FCatLike
object FCatLike {
  implicit val encoder: Encoder[FCatLike] = {
    implicit val config: Configuration = Configuration.default.withDiscriminator("type")
    KindlingsEncoder.derived[FCatLike]
  }
}

// A hierarchy whose intermediate has a hand-written encoder (and schema): kept as one subtype.
sealed trait FShape
case class FSquare(side: Int) extends FShape
sealed trait FRound extends FShape
case class FCircle(r: Int) extends FRound
object FRound {
  implicit val encoder: Encoder[FRound] = Encoder.instance { case FCircle(r) => Json.obj("radius" -> Json.fromInt(r)) }
  implicit val schema: Schema[FRound] = Schema(
    SchemaType.SProduct[FRound](
      List(
        SchemaType.SProductField[FRound, Int](
          FieldName("radius"),
          Schema.schemaForInt,
          (v: FRound) => v match { case FCircle(r) => Some(r) }
        )
      )
    ),
    Some(Schema.SName("FRound"))
  )
}

// Nested case objects (string encoding).
sealed trait FColor
case object FRed extends FColor
sealed trait FDark extends FColor
case object FNavy extends FDark
case object FBlack extends FDark
case class FPalette(main: FColor, others: List[FColor])

/** Checks that a circe JSON value is described by a tapir schema (enough of JSON Schema for these tests). */
object SchemaJsonCheck {

  private def named(schema: Schema[?], acc: Map[String, Schema[?]]): Map[String, Schema[?]] = {
    val key = schema.name.map(_.show)
    if (key.exists(acc.contains)) acc
    else {
      val withThis = key.fold(acc)(k => acc + (k -> schema))
      schema.schemaType match {
        case p: SchemaType.SProduct[?]        => p.fields.foldLeft(withThis)((a, f) => named(f.schema, a))
        case c: SchemaType.SCoproduct[?]      => c.subtypes.foldLeft(withThis)((a, s) => named(s, a))
        case o: SchemaType.SOption[?, ?]      => named(o.element, withThis)
        case a: SchemaType.SArray[?, ?]       => named(a.element, withThis)
        case m: SchemaType.SOpenProduct[?, ?] => named(m.valueSchema, withThis)
        case _                                => withThis
      }
    }
  }

  private def enumValues(validator: Validator[?]): List[String] = validator match {
    case e: Validator.Enumeration[?] =>
      e.possibleValues.map { v =>
        e.encode.flatMap(f => f.asInstanceOf[Any => Option[Any]](v)).getOrElse(v).toString
      }
    case all: Validator.All[?] => all.validators.toList.flatMap(enumValues)
    case _                     => Nil
  }

  def errors(schema: Schema[?], json: Json): List[String] = errors(schema, json, "$", named(schema, Map.empty))

  private def errors(schema: Schema[?], json: Json, path: String, refs: Map[String, Schema[?]]): List[String] = {
    def leaf(ok: Boolean, what: String): List[String] = if (ok) Nil else List(s"$path: expected $what, got $json")
    val typeErrors: List[String] = schema.schemaType match {
      case o: SchemaType.SOption[?, ?] => if (json.isNull) Nil else errors(o.element, json, path, refs)
      case _: SchemaType.SString[?]    =>
        leaf(json.isString, "a string") ++ (enumValues(schema.validator) match {
          case Nil     => Nil
          case allowed =>
            json.asString.filterNot(allowed.contains).toList.map(s => s"$path: $s is not one of $allowed")
        })
      case _: SchemaType.SInteger[?]  => leaf(json.asNumber.exists(_.toLong.isDefined), "an integer")
      case _: SchemaType.SNumber[?]   => leaf(json.isNumber, "a number")
      case _: SchemaType.SBoolean[?]  => leaf(json.isBoolean, "a boolean")
      case a: SchemaType.SArray[?, ?] =>
        json.asArray match {
          case Some(items) =>
            items.toList.zipWithIndex.flatMap { case (j, i) => errors(a.element, j, s"$path[$i]", refs) }
          case None => leaf(ok = false, "an array")
        }
      case m: SchemaType.SOpenProduct[?, ?] =>
        json.asObject match {
          case Some(obj) => obj.toList.flatMap { case (k, v) => errors(m.valueSchema, v, s"$path.$k", refs) }
          case None      => leaf(ok = false, "an object")
        }
      case p: SchemaType.SProduct[?] =>
        json.asObject match {
          case Some(obj) =>
            val known = p.fields.map(_.name.encodedName).toSet
            val unexpected = obj.keys.filterNot(known).map(k => s"$path: unexpected field $k").toList
            unexpected ++ p.fields.flatMap { f =>
              obj(f.name.encodedName) match {
                case Some(v)                     => errors(f.schema, v, s"$path.${f.name.encodedName}", refs)
                case None if f.schema.isOptional => Nil
                case None                        => List(s"$path: missing field ${f.name.encodedName}")
              }
            }
          case None => leaf(ok = false, "an object")
        }
      case c: SchemaType.SCoproduct[?] =>
        val perSubtype = c.subtypes.map(s => errors(s, json, path, refs))
        if (perSubtype.exists(_.isEmpty)) Nil
        else List(s"$path: $json matches none of the subtypes: ${perSubtype.map(_.mkString("; ")).mkString(" | ")}")
      case r: SchemaType.SRef[?] =>
        refs.get(r.name.show) match {
          case Some(target) => errors(target, json, path, refs)
          case None         => List(s"$path: unknown reference ${r.name.show}")
        }
      case other => List(s"$path: unsupported schema type $other")
    }
    typeErrors
  }
}

object FidelityDefault {
  implicit val config: Configuration = Configuration.default
  val garageEncoder: Encoder[FGarage] = KindlingsEncoder.derived[FGarage]
  val garageSchema: Schema[FGarage] = KindlingsSchema.derived[FGarage].schema
  val shapeEncoder: Encoder[FShape] = KindlingsEncoder.derived[FShape]
  val shapeSchema: Schema[FShape] = KindlingsSchema.derived[FShape].schema
  val vehicleSchema: Schema[FVehicle] = KindlingsSchema.derived[FVehicle].schema
}

object FidelityDiscriminator {
  implicit val config: Configuration =
    Configuration.default.withDiscriminator("type").withSnakeCaseMemberNames.withKebabCaseConstructorNames
  val garageEncoder: Encoder[FGarage] = KindlingsEncoder.derived[FGarage]
  val garageSchema: Schema[FGarage] = KindlingsSchema.derived[FGarage].schema
  val shapeEncoder: Encoder[FShape] = KindlingsEncoder.derived[FShape]
  val shapeSchema: Schema[FShape] = KindlingsSchema.derived[FShape].schema
}

object FidelityPetDiscriminator {
  implicit val config: Configuration = Configuration.default.withDiscriminator("type")
  val petEncoder: Encoder[FPet] = KindlingsEncoder.derived[FPet]
  val petSchema: Schema[FPet] = KindlingsSchema.derived[FPet].schema
}

object FidelityNoUnwrapping {
  implicit val config: Configuration = Configuration.default.withoutValueClassUnwrapping
  val garageEncoder: Encoder[FGarage] = KindlingsEncoder.derived[FGarage]
  val garageSchema: Schema[FGarage] = KindlingsSchema.derived[FGarage].schema
}

object FidelityNoUnwrappingDiscriminator {
  implicit val config: Configuration = Configuration.default.withoutValueClassUnwrapping.withDiscriminator("kind")
  val garageEncoder: Encoder[FGarage] = KindlingsEncoder.derived[FGarage]
  val garageSchema: Schema[FGarage] = KindlingsSchema.derived[FGarage].schema
}

object FidelityEnumStrings {
  implicit val config: Configuration = Configuration.default.withEnumAsStrings
  val paletteEncoder: Encoder[FPalette] = KindlingsEncoder.derived[FPalette]
  val paletteSchema: Schema[FPalette] = KindlingsSchema.derived[FPalette].schema
}

final class CirceSchemaFidelitySpec extends MacroSuite {

  private val garages: List[FGarage] = List(
    FGarage(
      "Ann",
      List(FCar(4, FPlate("KR 1")), FTruck(2.5), FScooter, FBadge(7)),
      Some(FScooter),
      Map("a" -> 1),
      FPlate("X"),
      Some(FPlate("Y"))
    ),
    FGarage("Bob", Nil, None, Map.empty, FPlate("Z"), None)
  )

  private def assertDescribes[A](schema: Schema[A], encoder: Encoder[A], values: List[A]): Unit =
    values.foreach { value =>
      val json = encoder(value)
      val errs = SchemaJsonCheck.errors(schema, json)
      assert(errs.isEmpty, s"the schema does not describe ${json.noSpaces}:\n${errs.mkString("\n")}")
    }

  private def subtypeNames(schema: Schema[?]): List[String] = schema.schemaType match {
    case c: SchemaType.SCoproduct[?] =>
      c.subtypes.flatMap { s =>
        // a wrapped subtype (`{"Name": value}`) is named by its single field
        (s.name, s.schemaType) match {
          case (None, p: SchemaType.SProduct[?]) if p.fields.size == 1 => List(p.fields.head.name.encodedName)
          case (name, _)                                               => name.map(_.fullName.split('.').last).toList
        }
      }
    case other => fail(s"Expected SCoproduct, got: $other")
  }

  group("tapir schemas describe what the circe codec writes") {

    test("default configuration (wrapped subtypes, flattened nested hierarchy, unwrapped value classes)") {
      assertDescribes(FidelityDefault.garageSchema, FidelityDefault.garageEncoder, garages)
      assertEquals(subtypeNames(FidelityDefault.vehicleSchema), List("FCar", "FTruck", "FScooter", "FBadge"))
    }

    test("discriminator, member and constructor name transforms") {
      assertDescribes(FidelityDiscriminator.garageSchema, FidelityDiscriminator.garageEncoder, garages)
    }

    test("unwrapValueClasses = false (value classes as objects)") {
      assertDescribes(FidelityNoUnwrapping.garageSchema, FidelityNoUnwrapping.garageEncoder, garages)
      FidelityNoUnwrapping.garageSchema.schemaType match {
        case p: SchemaType.SProduct[?] =>
          val plate = p.fields.find(_.name.name == "plate").map(_.schema.schemaType)
          assert(plate.exists(_.isInstanceOf[SchemaType.SProduct[?]]), s"plate: $plate")
        case other => fail(s"Expected SProduct, got: $other")
      }
    }

    test("unwrapValueClasses = false with a discriminator (an AnyVal subtype carries it)") {
      assertDescribes(
        FidelityNoUnwrappingDiscriminator.garageSchema,
        FidelityNoUnwrappingDiscriminator.garageEncoder,
        garages
      )
    }

    test("a Kindlings-derived intermediate is flattened") {
      val pets: List[FPet] = List(FDog("Rex"), FCat(9), FLion(3))
      assertDescribes(FidelityPetDiscriminator.petSchema, FidelityPetDiscriminator.petEncoder, pets)
      assertEquals(subtypeNames(FidelityPetDiscriminator.petSchema), List("FDog", "FCat", "FLion"))
    }

    test("a hand-written intermediate is kept as one subtype (wrapped and discriminator)") {
      val shapes: List[FShape] = List(FSquare(2), FCircle(1))
      assertDescribes(FidelityDefault.shapeSchema, FidelityDefault.shapeEncoder, shapes)
      assertDescribes(FidelityDiscriminator.shapeSchema, FidelityDiscriminator.shapeEncoder, shapes)
      assertEquals(subtypeNames(FidelityDiscriminator.shapeSchema), List("FSquare", "FRound"))
      FidelityDiscriminator.shapeSchema.schemaType match {
        case c: SchemaType.SCoproduct[?] =>
          assertEquals(c.discriminator.map(_.mapping.keySet), Some(Set("f-square", "f-round")))
        case other => fail(s"Expected SCoproduct, got: $other")
      }
    }

    test("enumAsStrings on a nested hierarchy of case objects") {
      val palettes = List(FPalette(FRed, List(FNavy, FBlack)))
      assertDescribes(FidelityEnumStrings.paletteSchema, FidelityEnumStrings.paletteEncoder, palettes)
    }

    test("the checker rejects a mismatch") {
      val json = FidelityDefault.garageEncoder(garages.head)
      assert(SchemaJsonCheck.errors(FidelityNoUnwrapping.garageSchema, json).nonEmpty)
    }
  }
}
