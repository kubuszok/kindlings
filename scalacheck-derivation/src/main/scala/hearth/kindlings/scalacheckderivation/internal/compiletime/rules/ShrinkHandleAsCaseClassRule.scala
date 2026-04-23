package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import org.scalacheck.Shrink

trait ShrinkHandleAsCaseClassRuleImpl { this: ShrinkMacrosImpl & MacroCommons & StdExtensions =>

  object ShrinkHandleAsCaseClassRule extends ShrinkDerivationRule("handle as case class when possible") {
    def apply[A: ShrinkCtx]: MIO[Rule.Applicability[Expr[Shrink[A]]]] =
      CaseClass.parse[A].toEither match {
        case Right(caseClass) =>
          implicit val UnitT: Type[Unit] = ShrinkTypes.Unit
          implicit val ShrinkA: Type[Shrink[A]] = ShrinkTypes.Shrink[A]
          val defBuilder = ValDefBuilder.ofDef1[Unit, Shrink[A]](s"shrinkCaseClass_${Type[A].shortName}")
          for {
            _ <- shrinkctx.cache.forwardDeclare("cached-shrink-method", defBuilder)
            _ <- MIO.scoped { runSafe =>
              runSafe(shrinkctx.cache.buildCachedWith("cached-shrink-method", defBuilder) { case (_, _) =>
                runSafe(deriveCaseClassShrink[A](caseClass))
              })
            }
            result <- ShrinkUseCachedRule[A]
          } yield result
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }

    @scala.annotation.nowarn
    private def deriveCaseClassShrink[A: ShrinkCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[Shrink[A]]] = {
      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.parameters.flatten.toList
      implicit val ShrinkA: Type[Shrink[A]] = ShrinkTypes.Shrink[A]
      implicit val AnyT: Type[Any] = ShrinkTypes.Any
      implicit val ArrayAnyT: Type[Array[Any]] = ShrinkTypes.ArrayAny

      NonEmptyList.fromList(fieldsList) match {
        case None =>
          MIO.pure[Expr[Shrink[A]]](Expr.quote(_root_.org.scalacheck.Shrink.shrinkAny[A]))

        case Some(fields) =>
          // Derive Shrink for each field, capturing type-safe cast builders
          fields
            .parTraverse { case (fieldName, param) =>
              import param.tpe.Underlying as Field
              Log.namedScope(s"Deriving Shrink for field $fieldName: ${Type[Field].prettyPrint}") {
                deriveShrinkRecursively[Field](using shrinkctx.nest[Field]).map { shrinkExpr =>
                  val shrinkAny: Expr[Shrink[Any]] = Expr.quote(Expr.splice(shrinkExpr).asInstanceOf[Shrink[Any]])

                  // Build a cast function that uses the shrinkExpr as a type witness
                  // (same pattern as ScalaCheckUtils.unsafeCast in the Arbitrary rule)
                  val castFn: Expr[Any] => Expr_?? = { (anyExpr: Expr[Any]) =>
                    Expr.quote {
                      hearth.kindlings.scalacheckderivation.internal.runtime.ScalaCheckUtils
                        .unsafeCastViaShrink(Expr.splice(anyExpr), Expr.splice(shrinkExpr))
                    }.as_??
                  }
                  (fieldName, shrinkAny, castFn)
                }
              }
            }
            .flatMap { fieldData =>
              val shrinkList = fieldData.toList.map(_._2)
              val castFns = fieldData.toList.map(d => (d._1, d._3))
              val numFields = fieldsList.size
              val numFieldsExpr = Expr(numFields)

              // Build list of Shrink[Any]
              val shrinksListExpr: Expr[List[Shrink[Any]]] =
                shrinkList.foldRight(Expr.quote(List.empty[Shrink[Any]])) { (shrinkExpr, acc) =>
                  Expr.quote(Expr.splice(shrinkExpr) :: Expr.splice(acc))
                }

              // Build reconstruct: Array[Any] => A using LambdaBuilder
              LambdaBuilder
                .of1[Array[Any]]("fields")
                .traverse { arrExpr =>
                  val fieldMap: Map[String, Expr_??] = castFns.zipWithIndex.map { case ((name, castFn), idx) =>
                    val elemExpr: Expr[Any] = Expr.quote(Expr.splice(arrExpr)(Expr.splice(Expr(idx))))
                    (name, castFn(elemExpr))
                  }.toMap
                  caseClass.primaryConstructor(fieldMap) match {
                    case Right(constructExpr) => MIO.pure(constructExpr)
                    case Left(error) =>
                      MIO.fail(new RuntimeException(s"Cannot construct ${Type[A].prettyPrint}: $error"))
                  }
                }
                .map { reconstructBuilder =>
                  val reconstructFn: Expr[Array[Any] => A] = reconstructBuilder.build[A]

                  Expr.quote {
                    hearth.kindlings.scalacheckderivation.internal.runtime.ShrinkUtils.shrinkCaseClass[A](
                      Expr.splice(shrinksListExpr),
                      (v: A) => {
                        val p = v.asInstanceOf[Product]
                        val arr = new Array[Any](Expr.splice(numFieldsExpr))
                        var i = 0
                        while (i < Expr.splice(numFieldsExpr)) {
                          arr(i) = p.productElement(i)
                          i += 1
                        }
                        arr
                      },
                      Expr.splice(reconstructFn)
                    )
                  }
                }
            }
      }
    }
  }
}
