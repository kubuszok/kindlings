package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils
import org.apache.avro.Schema

trait AvroEncoderHandleAsEnumRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  object AvroEncoderHandleAsEnumRule extends EncoderDerivationRule("handle as enum when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Any]]] =
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

    @scala.annotation.nowarn("msg=is never used")
    private def encodeEnumCases[A: EncoderCtx](
        enumm: Enum[A]
    ): MIO[Expr[Any]] = {
      implicit val AnyT: Type[Any] = EncTypes.Any
      implicit val SchemaT: Type[Schema] = EncTypes.Schema

      val childrenList = enumm.directChildren.toList

      val allCaseObjects = Type[A].isEnumeration || Type[A].isJavaEnum ||
        childrenList.forall { case (_, child) =>
          SingletonValue.unapply(child.Underlying).isDefined
        }

      if (allCaseObjects) {
        // Pure enum → encode as GenericData.EnumSymbol
        deriveSelfContainedSchema[A](ectx.config).flatMap { schemaExpr =>
          enumm
            .parMatchOn[MIO, Any](ectx.value) { matched =>
              import matched.Underlying as EnumCase
              val caseName: String = childrenList
                .find { case (_, child) =>
                  import child.Underlying as ChildType
                  Type[EnumCase] <:< Type[ChildType]
                }
                .map(_._1)
                .getOrElse(Type[EnumCase].shortName)
              MIO.pure(Expr.quote {
                val name = Expr.splice(ectx.config).transformConstructorNames(Expr.splice(Expr(caseName)))
                AvroDerivationUtils.encodeEnumSymbol(Expr.splice(schemaExpr), name): Any
              })
            }
            .flatMap {
              case Some(result) => MIO.pure(result)
              case None         =>
                val err = EncoderDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint)
                Log.error(err.message) >> MIO.fail(err)
            }
        }
      } else {
        // Mixed sealed trait → encode as the appropriate record subtype
        enumm
          .parMatchOn[MIO, Any](ectx.value) { matched =>
            import matched.{value as enumCaseValue, Underlying as EnumCase}
            Log.namedScope(s"Encoding enum case ${Type[EnumCase].prettyPrint}") {
              deriveEncoderRecursively[EnumCase](using ectx.nest(enumCaseValue))
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
}
