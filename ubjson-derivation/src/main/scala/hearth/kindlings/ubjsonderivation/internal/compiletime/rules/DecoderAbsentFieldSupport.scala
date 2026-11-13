package hearth.kindlings.ubjsonderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.std.*

import hearth.kindlings.ubjsonderivation.{UBJsonConfig, UBJsonReader}
import hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils

/** Absent-field semantics of the case class decoders, the same as in Kindlings' jsoniter derivation (which follows
  * jsoniter-scala's `JsonCodecMaker.make`). A field missing from the JSON object is filled with, in order of
  * precedence:
  *
  *   1. its default value (unless `requireDefaultFields`),
  *   1. `None` for an `Option` (never required),
  *   1. an empty collection/map (unless `requireCollectionFields`; collections that cannot be empty, like `::` or
  *      non-empty collections from integrations, are always required),
  *
  * otherwise decoding fails with `missing required field "<name>"` (a `UBJsonReaderException`).
  *
  * `transientDefault` / `transientEmpty` / `transientNone` only affect encoding.
  */
trait DecoderAbsentFieldSupport {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  /** What can stand in for an absent field before the config's `require*` flags are applied. */
  final protected case class AbsentFieldFallbacks[Field](
      default: Option[Expr[Field]],
      isOption: Boolean,
      empty: Option[Expr[Field]],
      hasDefault: Boolean = false,
      // `T | Null` (Scala 3): absent is `null`, never required (as `None` for an `Option`)
      isNullable: Boolean = false
  )

  /** @param argumentValues
    *   the values of the other constructor parameters: the default value of a parameter of a non-first parameter list
    *   may depend on the preceding parameters (without them such a default is not available)
    */
  protected def absentFieldFallbacks[Field: Type](
      param: Parameter,
      argumentValues: Map[String, Expr_??] = Map.empty
  ): AbsentFieldFallbacks[Field] = {
    val default: Option[Expr[Field]] =
      parameterDefault(param, argumentValues).map(ee => ee.value.asInstanceOf[Expr[Field]])
    val isOption = Type[Field] match {
      case IsOption(_) => true
      case _           => false
    }
    val isNullable = nullableUnionUnderlying[Field].isDefined
    AbsentFieldFallbacks(
      default,
      isOption,
      if (isOption || isNullable) None else emptyCollectionOf[Field],
      param.hasDefault,
      isNullable
    )
  }

  /** Fallback for an absent field when the config is known at compile time; `None` means the field is required. */
  protected def staticAbsentFallback[Field: Type](
      fallbacks: AbsentFieldFallbacks[Field],
      config: UBJsonConfig
  ): Option[Expr[Field]] =
    fallbacks.default
      .filter(_ => !config.requireDefaultFields)
      .orElse(if (fallbacks.isOption) Some(noneOf[Field]) else None)
      .orElse(if (fallbacks.isNullable) Some(nullOf[Field]) else None)
      .orElse(fallbacks.empty.filter(_ => !config.requireCollectionFields))

  /** Whether the absent value is `None` regardless of the `require*` flags (so a variable can start as `None`). */
  protected def absentIsAlwaysNone[Field](fallbacks: AbsentFieldFallbacks[Field], config: UBJsonConfig): Boolean =
    fallbacks.isOption && (!fallbacks.hasDefault || config.requireDefaultFields)

  /** Fallback for an absent field when the config is only known at runtime (fails for a required field). */
  @scala.annotation.nowarn("msg=is never used")
  protected def runtimeAbsentFallback[Field: Type](
      fallbacks: AbsentFieldFallbacks[Field],
      config: Expr[UBJsonConfig],
      reader: Expr[UBJsonReader],
      mappedName: Expr[String]
  ): Expr[Field] = {
    val required: Expr[Field] = missingRequiredFieldOf[Field](reader, mappedName)
    val withEmpty: Expr[Field] = fallbacks.empty.fold(required) { empty =>
      Expr.quote {
        if (Expr.splice(config).requireCollectionFields) Expr.splice(required) else Expr.splice(empty)
      }
    }
    val withOption: Expr[Field] =
      if (fallbacks.isOption) noneOf[Field] else if (fallbacks.isNullable) nullOf[Field] else withEmpty
    fallbacks.default.fold(withOption) { default =>
      Expr.quote {
        if (Expr.splice(config).requireDefaultFields) Expr.splice(withOption) else Expr.splice(default)
      }
    }
  }

  /** For decoders collecting field values into an `Array[Any]` (absent slots hold `UBJsonDerivationUtils.AbsentField`):
    * replace each absent slot with the field's runtime fallback.
    *
    * @param fields
    *   (field name, parameter, array index, `@fieldName` override)
    */
  @scala.annotation.nowarn("msg=is never used")
  protected def absentFieldFillExprs(
      fields: List[(String, Parameter, Int, Option[String])],
      decodedValuesExpr: Expr[Array[Any]],
      config: Expr[UBJsonConfig],
      reader: Expr[UBJsonReader],
      argumentValues: Map[String, Expr_??] = Map.empty,
      runtimeOnlyTransient: String => Boolean = _ => false
  ): List[Expr[Unit]] = {
    implicit val AnyT: Type[Any] = CTypes.Any
    implicit val StringT: Type[String] = CTypes.String
    fields.map { case (fName, param, idx, nameOverride) =>
      import param.tpe.Underlying as Field
      val mappedName: Expr[String] = nameOverride match {
        case Some(customName) => Expr(customName)
        case None             => Expr.quote(Expr.splice(config).fieldNameMapper(Expr.splice(Expr(fName))))
      }
      val fallbacks = absentFieldFallbacks[Field](param, argumentValues)
      val normalFallback = runtimeAbsentFallback[Field](fallbacks, config, reader, mappedName)
      // A `scala.transient` field (runtime config): its default when `scalaTransientSupport` is on
      val fallback: Expr[Field] = fallbacks.default match {
        case Some(default) if runtimeOnlyTransient(fName) =>
          Expr.quote {
            if (Expr.splice(config).scalaTransientSupport) Expr.splice(default) else Expr.splice(normalFallback)
          }
        case _ => normalFallback
      }
      Expr.quote {
        if (UBJsonDerivationUtils.isAbsentField(Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx)))))
          Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) = Expr.splice(fallback).asInstanceOf[Any]
      }
    }
  }

  /** jsoniter-scala's decoding of a class field with a default value (its `genReadVal(fieldType, default = ...)`):
    *
    *   - a JSON `null` decodes as the default for reference-typed fields (also `Option`s with a default; not nullable
    *     `T | Null` fields, nested options, primitives and value classes);
    *   - an empty array/object for a collection/map field decodes as the default;
    *   - a field with a user-provided codec is decoded by that codec alone (jsoniter-scala passes the default to the
    *     codec, which decides; a `UBJsonValueCodec` has no default argument).
    */
  @scala.annotation.nowarn("msg=is never used")
  protected def withFieldDefault[Field: Type](param: Parameter)(
      decode: Expr[UBJsonReader] => Expr[Field]
  ): Expr[UBJsonReader] => Expr[Field] = {
    implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
    val default: Option[Expr[Field]] = parameterDefault(param, Map.empty).map(_.value.asInstanceOf[Expr[Field]])
    def hasUserCodec: Boolean = CTypes
      .UBJsonValueCodec[Field]
      .summonExprIgnoring(DecoderUseImplicitWhenAvailableRule.ignoredImplicits*)
      .toOption
      .isDefined
    default match {
      case None                    => decode
      case Some(_) if hasUserCodec => decode
      case Some(d)                 =>
        val isCollection = !(Type[Field] =:= CTypes.String) && (Type[Field] match {
          case IsCollection(_) => true
          case _               => false
        })
        val isNestedOption = Type[Field] match {
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            Inner match {
              case IsOption(_) => true
              case _           => false
            }
          case _ => false
        }
        val nullAsDefault =
          !isNestedOption && nullableUnionUnderlying[Field].isEmpty && Type[Field] <:< Type.of[AnyRef]
        val emptyAsDefault: Expr[UBJsonReader] => Expr[Field] =
          if (isCollection)
            reader =>
              Expr.quote(UBJsonDerivationUtils.emptyAsDefault[Field](Expr.splice(decode(reader)), Expr.splice(d)))
          else decode
        if (!nullAsDefault) emptyAsDefault
        else
          reader =>
            Expr.quote {
              UBJsonDerivationUtils.readNullAsDefault[Field](Expr.splice(reader), Expr.splice(d)) { (r: UBJsonReader) =>
                Expr.splice(emptyAsDefault(Expr.quote(r)))
              }
            }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  protected def missingRequiredFieldOf[Field: Type](reader: Expr[UBJsonReader], mappedName: Expr[String]): Expr[Field] =
    Expr.quote(UBJsonDerivationUtils.missingRequiredField[Field](Expr.splice(reader), Expr.splice(mappedName)))

  @scala.annotation.nowarn("msg=is never used")
  private def noneOf[Field: Type]: Expr[Field] = Expr.quote(scala.None.asInstanceOf[Field])

  @scala.annotation.nowarn("msg=is never used")
  private def nullOf[Field: Type]: Expr[Field] = Expr.quote(null.asInstanceOf[Field])

  /** An empty instance of a collection/map type, if it has one (`String` and `::` are not treated as collections). */
  @scala.annotation.nowarn("msg=is never used")
  private def emptyCollectionOf[Field: Type]: Option[Expr[Field]] =
    if (Type[Field] =:= CTypes.String) None
    else if (Type[Field] <:< Type.of[scala.collection.immutable.::[Any]]) None
    else
      Type[Field] match {
        case IsCollection(isCollection) =>
          import isCollection.Underlying as Item
          val coll = isCollection.value
          import coll.CtorResult
          coll.build match {
            case _: CtorLikeOf.PlainValue[?, ?] =>
              val emptyBuilder: Expr[scala.collection.mutable.Builder[Item, CtorResult]] =
                Expr.quote(Expr.splice(coll.factory).newBuilder)
              Some(coll.build.ctor(emptyBuilder).asInstanceOf[Expr[Field]])
            case _ => None
          }
        case _ => None
      }
}
