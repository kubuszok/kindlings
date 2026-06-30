package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils
import com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter

trait EncoderHandleAsEnumRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsEnumRule extends EncoderDerivationRule("handle as enum when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            // Note: caching is handled by deriveEncoderRecursively — do NOT call setHelper here.
            encodeEnumCases[A](enumm).map(Rule.matched)
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def encodeEnumCases[A: EncoderCtx](
        enumm: Enum[A]
    ): MIO[Expr[Unit]] = {
      implicit val UnitT: Type[Unit] = CTypes.Unit
      implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
      implicit val StringT: Type[String] = CTypes.String

      // Use exhaustive (leaf) children to flatten nested sealed trait hierarchies.
      // This ensures intermediate sealed traits like MotorVehicle in Vehicle > MotorVehicle > Truck
      // are skipped — only concrete leaf types (Truck, Motorcycle, Bicycle) are encoded.
      val childrenList: List[(String, ??<:[A])] = enumm.exhaustiveChildren match {
        case Some(ec) => ec.toList
        case None     => enumm.directChildren.toList
      }
      val isJavaEnum = Type[A].isJavaEnum
      val isEnumerationOrJavaEnum = Type[A].isEnumeration || isJavaEnum
      val allCaseObjects = isEnumerationOrJavaEnum || childrenList.forall { case (_, child) =>
        SingletonValue.unapply(child.Underlying).isDefined
      }

      // Build a type match on exhaustive (leaf) children instead of using
      // enumm.parMatchOn, which dispatches on directChildren and would match
      // intermediate sealed traits, causing nested wrapper encoding.
      NonEmptyList.fromList(childrenList) match {
        case None =>
          val err = CodecDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint)
          Log.error(err.message) >> MIO.fail(err)
        case Some(children) =>
          children
            .parTraverse { case (caseName, child) =>
              import child.Underlying as EnumCase
              MIO.scoped { runSafe =>
                val mc: MatchCase[Expr[EnumCase]] = MatchCase.typeMatch[EnumCase](caseName)
                mc.map { (enumCaseValue: Expr[EnumCase]) =>
                  runSafe {
                    Log.namedScope(s"Encoding enum case $caseName: ${Type[EnumCase].prettyPrint}") {

                      // For discriminator mode, we need fields-only encoding to avoid double wrapping.
                      // Parse child as case class to get field-level access.
                      val fieldsOnlyMIO: MIO[Expr[Unit]] =
                        if (isEnumerationOrJavaEnum) MIO.pure(Expr.quote(()): Expr[Unit])
                        else
                          SingletonValue.unapply(Type[EnumCase]) match {
                            case Some(_) =>
                              // Singleton — no fields
                              MIO.pure(Expr.quote(()): Expr[Unit])
                            case None =>
                              CaseClass.parse[EnumCase].toOption match {
                                case Some(caseClass) =>
                                  EncoderHandleAsCaseClassRule.encodeCaseClassFieldsOnly[EnumCase](caseClass)(using
                                    ectx.nest(enumCaseValue)
                                  )
                                case None =>
                                  // Not a case class (e.g. case object) — no fields
                                  MIO.pure(Expr.quote(()): Expr[Unit])
                              }
                          }

                      // Also derive the full encoding for wrapper mode
                      val fullEncMIO: MIO[Expr[Unit]] =
                        if (isEnumerationOrJavaEnum) MIO.pure(Expr.quote(()): Expr[Unit])
                        else deriveEncoderRecursively[EnumCase](using ectx.nest(enumCaseValue))

                      for {
                        fieldsOnly <- fieldsOnlyMIO
                        fullEnc <- fullEncMIO
                      } yield
                        if (allCaseObjects) {
                          if (isEnumerationOrJavaEnum)
                            Expr.quote {
                              val config = Expr.splice(ectx.config)
                              if (config.useScalaEnumValueId)
                                JsoniterDerivationUtils.writeScalaEnumValueId(
                                  Expr.splice(ectx.writer),
                                  Expr.splice(enumCaseValue)
                                )
                              else {
                                val baseName = config.adtLeafClassNameMapper(Expr.splice(Expr(caseName)))
                                val name =
                                  if (Expr.splice(Expr(isJavaEnum))) config.javaEnumValueNameMapper(baseName)
                                  else baseName
                                if (config.enumAsStrings)
                                  JsoniterDerivationUtils.writeEnumAsString(Expr.splice(ectx.writer), name)
                                else
                                  config.discriminatorFieldName match {
                                    case Some(discriminatorField) =>
                                      Expr.splice(ectx.writer).writeObjectStart()
                                      Expr.splice(ectx.writer).writeKey(discriminatorField)
                                      Expr.splice(ectx.writer).writeVal(name)
                                      Expr.splice(ectx.writer).writeObjectEnd()
                                    case None =>
                                      JsoniterDerivationUtils.writeWrapped(Expr.splice(ectx.writer), name) {}
                                  }
                              }
                            }
                          else
                            Expr.quote {
                              val config = Expr.splice(ectx.config)
                              val name = config.adtLeafClassNameMapper(Expr.splice(Expr(caseName)))
                              if (config.enumAsStrings) {
                                JsoniterDerivationUtils.writeEnumAsString(Expr.splice(ectx.writer), name)
                              } else {
                                config.discriminatorFieldName match {
                                  case Some(discriminatorField) =>
                                    Expr.splice(ectx.writer).writeObjectStart()
                                    Expr.splice(ectx.writer).writeKey(discriminatorField)
                                    Expr.splice(ectx.writer).writeVal(name)
                                    Expr.splice(fieldsOnly)
                                    Expr.splice(ectx.writer).writeObjectEnd()
                                  case None =>
                                    JsoniterDerivationUtils.writeWrapped(Expr.splice(ectx.writer), name) {
                                      Expr.splice(fullEnc)
                                    }
                                }
                              }
                            }
                        } else {
                          val isSingletonCase = SingletonValue.unapply(Type[EnumCase]).isDefined
                          Expr.quote {
                            val config = Expr.splice(ectx.config)
                            val name = config.adtLeafClassNameMapper(Expr.splice(Expr(caseName)))
                            if (config.circeLikeObjectEncoding && Expr.splice(Expr(isSingletonCase)))
                              JsoniterDerivationUtils.writeWrapped(Expr.splice(ectx.writer), name) {
                                Expr.splice(ectx.writer).writeObjectStart()
                                Expr.splice(ectx.writer).writeObjectEnd()
                              }
                            else
                              config.discriminatorFieldName match {
                                case Some(discriminatorField) =>
                                  Expr.splice(ectx.writer).writeObjectStart()
                                  Expr.splice(ectx.writer).writeKey(discriminatorField)
                                  Expr.splice(ectx.writer).writeVal(name)
                                  Expr.splice(fieldsOnly)
                                  Expr.splice(ectx.writer).writeObjectEnd()
                                case None =>
                                  JsoniterDerivationUtils.writeWrapped(Expr.splice(ectx.writer), name) {
                                    Expr.splice(fullEnc)
                                  }
                              }
                          }
                        }
                    }
                  }
                }
              }
            }
            .map { matchCases =>
              ectx.value.matchOn[Unit](matchCases.toNonEmptyVector)
            }
      }
    }
  }
}
