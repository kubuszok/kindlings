package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import org.scalacheck.Gen

trait ArbitraryHandleAsCaseClassRuleImpl { this: ArbitraryMacrosImpl & MacroCommons & StdExtensions =>

  object ArbitraryHandleAsCaseClassRule extends ArbitraryDerivationRule("handle as case class when possible") {
    def apply[A: ArbitraryCtx]: MIO[Rule.Applicability[Expr[Gen[A]]]] =
      CaseClass.parse[A].toEither match {
        case Right(caseClass) =>
          implicit val UnitT: Type[Unit] = ArbitraryTypes.Unit
          implicit val GenA: Type[Gen[A]] = ArbitraryTypes.Gen[A]
          val defBuilder = ValDefBuilder.ofDef1[Unit, Gen[A]](s"genCaseClass_${Type[A].shortName}")
          for {
            _ <- arbctx.cache.forwardDeclare("cached-arbitrary-method", defBuilder)
            _ <- MIO.scoped { runSafe =>
              runSafe(arbctx.cache.buildCachedWith("cached-arbitrary-method", defBuilder) { case (_, _) =>
                runSafe(deriveCaseClassArbitrary[A](caseClass))
              })
            }
            result <- ArbitraryUseCachedRule[A]
          } yield result
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }

    @scala.annotation.nowarn
    private def deriveCaseClassArbitrary[A: ArbitraryCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[Gen[A]]] = {
      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.totalParameters.flatten.toList

      NonEmptyList.fromList(fieldsList) match {
        case None =>
          // Zero-parameter case class
          foldInstanceFree(caseClass.primaryConstructor, "Constructor")(
            onTypes = _ => Map.empty,
            onValues = _ => Map.empty
          ) match {
            case Right(constructExpr) =>
              MIO.pure[Expr[Gen[A]]](
                Expr.quote(_root_.org.scalacheck.Gen.const[A](Expr.splice(constructExpr.value.asInstanceOf[Expr[A]])))
              )
            case Left(error) =>
              val err = ArbitraryDerivationError.CannotConstructType(Type[A].prettyPrint, error)
              Log.error(err.message) >> MIO.fail(err)
          }

        case Some(fields) =>
          // Multi-parameter case class
          // Following circe's pattern: create accessor functions outside lambda builder
          implicit val ListAnyType: Type[List[Any]] = ArbitraryTypes.ListAny
          implicit val ListGenAnyType: Type[List[Gen[Any]]] = ArbitraryTypes.ListGenAny
          implicit val GenAType: Type[Gen[A]] = ArbitraryTypes.Gen[A]

          fields
            .parTraverse { case (fieldName, param) =>
              import param.tpe.Underlying as Field
              val idx = param.index
              Log.namedScope(s"Deriving Arbitrary for field $fieldName: ${Type[Field].prettyPrint}") {
                deriveArbitraryRecursively[Field](using arbctx.nest[Field]).map { genExpr =>
                  // Check if the field's type matches the entry-point derived type (direct recursion)
                  val isRecursive: Boolean = arbctx.derivedType.exists { dt =>
                    import dt.Underlying as DT
                    Type[Field] =:= Type[DT]
                  }

                  // If recursive, wrap the Gen in size halving to ensure termination
                  val finalGenExpr: Expr[Gen[Field]] = if (isRecursive) {
                    Expr.quote {
                      _root_.org.scalacheck.Gen.sized { n =>
                        _root_.org.scalacheck.Gen.resize(
                          _root_.java.lang.Math.max(n / 2, 0),
                          Expr.splice(genExpr)
                        )
                      }
                    }
                  } else genExpr

                  // finalGenExpr is Expr[Gen[Field]] - the type witness
                  // Cast to Gen[Any] for the list literal
                  val genAny: Expr[Gen[Any]] = Expr.quote(Expr.splice(finalGenExpr).asInstanceOf[Gen[Any]])

                  // Create accessor function - closes over finalGenExpr (type witness) and idx
                  val makeAccessor: Expr[List[Any]] => (String, Expr_??) = { listExpr =>
                    val typedExpr = Expr.quote {
                      hearth.kindlings.scalacheckderivation.internal.runtime.ScalaCheckUtils
                        .unsafeCast(
                          Expr.splice(listExpr)(Expr.splice(Expr(idx))),
                          Expr.splice(finalGenExpr) // Type witness: Gen[Field]
                        )
                    }
                    (fieldName, typedExpr.as_??)
                  }

                  (genAny, makeAccessor)
                }
              }
            }
            .flatMap { fieldData =>
              val genAnyList = fieldData.toList.map(_._1)
              val makeAccessors = fieldData.toList.map(_._2)

              // Build list of generators
              val genListExpr: Expr[List[Gen[Any]]] =
                genAnyList.foldRight(Expr.quote(List.empty[Gen[Any]])) { (elem, acc) =>
                  Expr.quote(Expr.splice(elem) :: Expr.splice(acc))
                }

              // Build constructor lambda using LambdaBuilder
              LambdaBuilder
                .of1[List[Any]]("values")
                .traverse { valuesExpr =>
                  // Apply accessor functions to get typed field expressions
                  val fieldMap: Map[String, Expr_??] = makeAccessors.map(_(valuesExpr)).toMap

                  // Build constructor
                  foldInstanceFree(caseClass.primaryConstructor, "Constructor")(
                    onTypes = _ => Map.empty,
                    onValues = _ => fieldMap
                  ) match {
                    case Right(constructExpr) => MIO.pure(constructExpr.value.asInstanceOf[Expr[A]])
                    case Left(error)          =>
                      val err = ArbitraryDerivationError.CannotConstructType(Type[A].prettyPrint, error)
                      Log.error(err.message) >> MIO.fail(err)
                  }
                }
                .map { builder =>
                  val constructFn = builder.build[A]
                  Expr.quote {
                    hearth.kindlings.scalacheckderivation.internal.runtime.ScalaCheckUtils
                      .sequenceGens(Expr.splice(genListExpr))(Expr.splice(constructFn))
                  }
                }
            }
      }
    }
  }
}
