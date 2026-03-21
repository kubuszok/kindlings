package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils
import org.apache.avro.generic.GenericRecord

trait AvroDecoderHandleAsEnumRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  object AvroDecoderHandleAsEnumRule extends DecoderDerivationRule("handle as enum when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            decodeEnumCases[A](enumm).map(Rule.matched)
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeEnumCases[A: DecoderCtx](
        enumm: Enum[A]
    ): MIO[Expr[A]] = {
      implicit val StringT: Type[String] = DecTypes.String
      implicit val AnyT: Type[Any] = DecTypes.Any

      val childrenList = enumm.directChildren.toList

      NonEmptyList.fromList(childrenList) match {
        case None =>
          val err = DecoderDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint)
          Log.error(err.message) >> MIO.fail(err)

        case Some(children) =>
          val allCaseObjects = Type[A].isEnumeration || Type[A].isJavaEnum ||
            children.toList.forall { case (_, child) =>
              SingletonValue.unapply(child.Underlying).isDefined
            }

          if (allCaseObjects) {
            // Pure enum → decode from GenericData.EnumSymbol string
            val knownNames: List[String] = children.toList.map(_._1)

            // Build dispatch chain: if name matches → return case object singleton
            children
              .parTraverse { case (childName, child) =>
                import child.Underlying as ChildType
                Log.namedScope(s"Deriving decoder for enum case $childName") {
                  SingletonValue.unapply(Type[ChildType]) match {
                    case Some(sv) =>
                      MIO.pure((childName, sv.singletonExpr.asInstanceOf[Expr[A]]))
                    case None =>
                      // Fallback to construct for non-singleton zero-arg case classes
                      CaseClass.parse[ChildType].toOption match {
                        case Some(cc) =>
                          cc.construct[MIO](new CaseClass.ConstructField[MIO] {
                            def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] = {
                              val err =
                                DecoderDerivationError.EnumChildError(childName, "Unexpected parameter in case object")
                              Log.error(err.message) >> MIO.fail(err)
                            }
                          }).flatMap {
                            case Some(expr) => MIO.pure((childName, expr.asInstanceOf[Expr[A]]))
                            case None       =>
                              val err = DecoderDerivationError.EnumChildError(childName, "Cannot construct")
                              Log.error(err.message) >> MIO.fail(err)
                          }
                        case None =>
                          val err = DecoderDerivationError.EnumChildError(childName, "is not parseable as a case class")
                          Log.error(err.message) >> MIO.fail(err)
                      }
                  }
                }
              }
              .map { dispatchers =>
                val errorExpr: Expr[A] = Expr.quote {
                  AvroDerivationUtils.failedToMatchSubtype(
                    AvroDerivationUtils.decodeEnumSymbol(Expr.splice(dctx.avroValue)),
                    Expr.splice(Expr(knownNames))
                  )
                }

                dispatchers.toList.foldRight(errorExpr) { case ((childName, childExpr), elseExpr) =>
                  Expr.quote {
                    val name = AvroDerivationUtils.decodeEnumSymbol(Expr.splice(dctx.avroValue))
                    if (Expr.splice(dctx.config).transformConstructorNames(Expr.splice(Expr(childName))) == name)
                      Expr.splice(childExpr)
                    else
                      Expr.splice(elseExpr)
                  }
                }
              }
          } else {
            // Mixed sealed trait → dispatch based on record schema name
            // For union types, Hearth returns FQN child names (e.g., "pkg.Parrot"), but Avro schema
            // getName returns simple names (e.g., "Parrot"). Extract simple names for comparison.
            def simpleName(fqn: String): String = fqn.lastIndexOf('.') match {
              case -1 => fqn
              case i  => fqn.substring(i + 1)
            }
            val knownNames: List[String] = children.toList.map(c => simpleName(c._1))

            children
              .parTraverse { case (childName, child) =>
                import child.Underlying as ChildType
                val simpleChildName = simpleName(childName)
                Log.namedScope(s"Deriving decoder for enum case $childName: ${Type[ChildType].prettyPrint}") {
                  deriveDecoderRecursively[ChildType](using dctx.nest[ChildType](dctx.avroValue)).flatMap {
                    decodedExpr =>
                      dctx.getHelper[ChildType].map {
                        case Some(helper) =>
                          (
                            simpleChildName,
                            (valueExpr: Expr[Any], elseExpr: Expr[A]) =>
                              Expr.quote {
                                val record = Expr.splice(valueExpr).asInstanceOf[GenericRecord]
                                val recordName = record.getSchema.getName
                                if (
                                  Expr
                                    .splice(dctx.config)
                                    .transformConstructorNames(
                                      Expr.splice(Expr(simpleChildName))
                                    ) == recordName
                                )
                                  Expr.splice(helper(valueExpr, dctx.config)).asInstanceOf[A]
                                else
                                  Expr.splice(elseExpr)
                              }
                          )
                        case None =>
                          // No helper registered (e.g., built-in types) — use the derived expression directly
                          (
                            simpleChildName,
                            (valueExpr: Expr[Any], elseExpr: Expr[A]) =>
                              Expr.quote {
                                val record = Expr.splice(valueExpr).asInstanceOf[GenericRecord]
                                val recordName = record.getSchema.getName
                                if (
                                  Expr
                                    .splice(dctx.config)
                                    .transformConstructorNames(
                                      Expr.splice(Expr(simpleChildName))
                                    ) == recordName
                                )
                                  Expr.splice(decodedExpr).asInstanceOf[A]
                                else
                                  Expr.splice(elseExpr)
                              }
                          )
                      }
                  }
                }
              }
              .map { dispatchers =>
                val errorExpr: Expr[A] = Expr.quote {
                  val record = Expr.splice(dctx.avroValue).asInstanceOf[GenericRecord]
                  AvroDerivationUtils.failedToMatchSubtype(
                    record.getSchema.getName,
                    Expr.splice(Expr(knownNames))
                  )
                }

                dispatchers.toList.foldRight(errorExpr) { case ((_, dispatch), elseExpr) =>
                  dispatch(dctx.avroValue, elseExpr)
                }
              }
          }
      }
    }
  }
}
