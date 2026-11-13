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
      val childrenList: List[(String, ??<:[A])] = adtLeaves[A](enumm)
      val isJavaEnum = isJavaEnumType[A]
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
          val collisionCheck: MIO[Unit] = ectx.evaluatedConfig match {
            case Some(config) if !Type[A].isEnumeration =>
              checkDiscriminatorValueCollisions[A](childrenList.map { case (childName, child) =>
                import child.Underlying as Child
                if (isJavaEnum) config.javaEnumValueNameMapper(childName)
                else
                  namedAnnotationOf[Child].getOrElse(
                    config.adtLeafClassNameMapper(adtLeafFullName[A, Child](childName))
                  )
              })
            case _ => MIO.pure(())
          }
          collisionCheck >> children
            .parTraverse { case (caseName, child) =>
              import child.Underlying as EnumCase
              MIO.scoped { runSafe =>
                val mc: MatchCase[Expr[EnumCase]] = MatchCase.typeMatch[EnumCase](caseName)
                mc.map { (enumCaseValue: Expr[EnumCase]) =>
                  runSafe {
                    Log.namedScope(s"Encoding enum case $caseName: ${Type[EnumCase].prettyPrint}") {

                      // As in jsoniter-scala, a user-provided codec of a leaf encodes it whole, also with a
                      // discriminator (the codec is responsible for writing one, if wanted).
                      val userCodec: Option[Expr[com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[EnumCase]]] =
                        if (isEnumerationOrJavaEnum || SingletonValue.unapply(Type[EnumCase]).isDefined) None
                        else summonJsonValueCodecCached[EnumCase].toOption
                      // For discriminator mode, we need fields-only encoding to avoid double wrapping.
                      // Parse child as case class to get field-level access.
                      val fieldsOnlyMIO: MIO[Expr[Unit]] =
                        if (isEnumerationOrJavaEnum || userCodec.isDefined) MIO.pure(Expr.quote(()): Expr[Unit])
                        else
                          SingletonValue.unapply(Type[EnumCase]) match {
                            case Some(_) =>
                              // Singleton — no fields
                              MIO.pure(Expr.quote(()): Expr[Unit])
                            case None =>
                              ProductType.asLeaf[EnumCase] match {
                                case Some(caseClass) =>
                                  EncoderHandleAsCaseClassRule.encodeCaseClassFieldsOnly[EnumCase](
                                    caseClass,
                                    viaSealedParent = true
                                  )(using ectx.nest(enumCaseValue))
                                case None =>
                                  // Not a case class (e.g. case object) — no fields
                                  MIO.pure(Expr.quote(()): Expr[Unit])
                              }
                          }

                      // Also derive the full encoding for wrapper mode
                      val fullEncMIO: MIO[Expr[Unit]] =
                        if (isEnumerationOrJavaEnum || SingletonValue.unapply(Type[EnumCase]).isDefined)
                          MIO.pure(Expr.quote(()): Expr[Unit])
                        else if (Type[EnumCase] =:= Type[A])
                          // A non-abstract sealed base as its own leaf: deriving it recursively would be this codec.
                          ProductType.asLeaf[EnumCase] match {
                            case Some(product) =>
                              EncoderHandleAsCaseClassRule
                                .encodeCaseClassFields[EnumCase](product)(using ectx.nest(enumCaseValue))
                            case None => MIO.pure(Expr.quote(()): Expr[Unit])
                          }
                        else deriveEncoderRecursively[EnumCase](using ectx.nest(enumCaseValue))

                      for {
                        fieldsOnly <- fieldsOnlyMIO
                        fullEnc <- fullEncMIO
                      } yield {
                        val nameExpr = enumLeafNameExpr[A, EnumCase](caseName, ectx.config)
                        val isSingletonCase = SingletonValue.unapply(Type[EnumCase]).isDefined
                        if (isEnumerationOrJavaEnum) {
                          // As in jsoniter-scala: always a JSON string (or the id with useScalaEnumValueId).
                          if (Type[A].isEnumeration)
                            Expr.quote {
                              if (Expr.splice(ectx.config).useScalaEnumValueId)
                                JsoniterDerivationUtils.writeScalaEnumValueId(
                                  Expr.splice(ectx.writer),
                                  Expr.splice(enumCaseValue),
                                  Expr.splice(ectx.stringifiedExpr)
                                )
                              else Expr.splice(ectx.writer).writeVal(Expr.splice(nameExpr))
                            }
                          else Expr.quote(Expr.splice(ectx.writer).writeVal(Expr.splice(nameExpr)))
                        } else if (isSingletonCase) {
                          // As in jsoniter-scala: a case object is `"Leaf"` without a discriminator (`{"Leaf":{}}`
                          // with circeLikeObjectEncoding) and `{"type":"Leaf"}` with one. enumAsStrings (Kindlings)
                          // writes ADTs of case objects only as strings with a discriminator too.
                          val enumAsStrings: Expr[Boolean] =
                            if (allCaseObjects) Expr.quote(Expr.splice(ectx.config).enumAsStrings) else Expr(false)
                          Expr.quote {
                            val config = Expr.splice(ectx.config)
                            val name = Expr.splice(nameExpr)
                            if (Expr.splice(enumAsStrings)) Expr.splice(ectx.writer).writeVal(name)
                            else
                              config.discriminatorFieldName match {
                                case Some(discriminatorField) =>
                                  Expr.splice(ectx.writer).writeObjectStart()
                                  Expr.splice(ectx.writer).writeKey(discriminatorField)
                                  Expr.splice(ectx.writer).writeVal(name)
                                  Expr.splice(ectx.writer).writeObjectEnd()
                                case None =>
                                  if (config.circeLikeObjectEncoding)
                                    JsoniterDerivationUtils.writeWrapped(Expr.splice(ectx.writer), name) {
                                      Expr.splice(ectx.writer).writeObjectStart()
                                      Expr.splice(ectx.writer).writeObjectEnd()
                                    }
                                  else Expr.splice(ectx.writer).writeVal(name)
                              }
                          }
                        } else if (userCodec.isDefined)
                          Expr.quote {
                            val config = Expr.splice(ectx.config)
                            val name = Expr.splice(nameExpr)
                            config.discriminatorFieldName match {
                              case Some(_) =>
                                Expr
                                  .splice(userCodec.get)
                                  .encodeValue(Expr.splice(enumCaseValue), Expr.splice(ectx.writer))
                              case None =>
                                JsoniterDerivationUtils.writeWrapped(Expr.splice(ectx.writer), name) {
                                  Expr.splice(fullEnc)
                                }
                            }
                          }
                        else
                          Expr.quote {
                            val config = Expr.splice(ectx.config)
                            val name = Expr.splice(nameExpr)
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
