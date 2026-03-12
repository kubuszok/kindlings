package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import io.circe.Json

trait EncoderHandleAsEnumRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsEnumRule extends EncoderDerivationRule("handle as enum when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            for {
              _ <- ectx.setHelper[A] { (value, config) =>
                encodeEnumCases[A](enumm)(using ectx.nestInCache(value, config))
              }
              result <- ectx.getHelper[A].flatMap {
                case Some(helperCall) => MIO.pure(Rule.matched(helperCall(ectx.value, ectx.config)))
                case None             => MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    private def encodeEnumCases[A: EncoderCtx](
        enumm: Enum[A]
    ): MIO[Expr[Json]] = {
      implicit val JsonT: Type[Json] = Types.Json

      // Check at compile time if all children are singletons (case objects with no fields)
      val childrenList = enumm.directChildren.toList
      val isEnumerationOrJavaEnum = Type[A].isEnumeration || Type[A].isJavaEnum
      val allCaseObjects = isEnumerationOrJavaEnum || childrenList.forall { case (_, child) =>
        SingletonValue.unapply(child.Underlying).isDefined
      }

      enumm
        .parMatchOn[MIO, Json](ectx.value) { matched =>
          import matched.{value as enumCaseValue, Underlying as EnumCase}
          Log.namedScope(s"Encoding enum case ${enumCaseValue.prettyPrint}: ${EnumCase.prettyPrint}") {
            val caseJsonMIO: MIO[Expr[Json]] =
              if (isEnumerationOrJavaEnum) MIO.pure(Expr.quote(Json.obj()))
              else deriveEncoderRecursively[EnumCase](using ectx.nest(enumCaseValue))
            caseJsonMIO.map { caseJson =>
              val caseName: String = childrenList
                .find { case (_, child) =>
                  import child.Underlying as ChildType
                  Type[EnumCase] <:< Type[ChildType]
                }
                .map(_._1)
                .getOrElse(Type[EnumCase].shortName)
              if (allCaseObjects) {
                Expr.quote {
                  val config = Expr.splice(ectx.config)
                  val name = config.transformConstructorNames(Expr.splice(Expr(caseName)))
                  if (config.enumAsStrings) {
                    CirceDerivationUtils.encodeEnumAsString(name)
                  } else {
                    val json = Expr.splice(caseJson)
                    config.discriminator match {
                      case Some(discriminatorField) =>
                        CirceDerivationUtils.addDiscriminator(discriminatorField, name, json)
                      case None =>
                        CirceDerivationUtils.wrapWithTypeName(name, json)
                    }
                  }
                }
              } else {
                Expr.quote {
                  val name = Expr.splice(ectx.config).transformConstructorNames(Expr.splice(Expr(caseName)))
                  val json = Expr.splice(caseJson)
                  Expr.splice(ectx.config).discriminator match {
                    case Some(discriminatorField) =>
                      CirceDerivationUtils.addDiscriminator(discriminatorField, name, json)
                    case None =>
                      CirceDerivationUtils.wrapWithTypeName(name, json)
                  }
                }
              }
            }
          }
        }
        .flatMap {
          case Some(result) => MIO.pure(result)
          case None         =>
            val err = EncoderDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint)
            Log.error(err.message) >> MIO.fail(err)
        }
    }
  }
}
