package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.xmlderivation.{XmlConfig, XmlDecodingError}
import hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils

trait DecoderHandleAsEnumRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsEnumRule extends DecoderDerivationRule("handle as sealed trait/enum when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a sealed trait/enum") >> {
        Enum.parse[A].toEither match {
          case Right(parsedEnum) =>
            for {
              _ <- dctx.setHelper[A] { (elem, config) =>
                decodeEnumCases[A](parsedEnum)(using dctx.nestInCache(elem, config))
              }
              result <- dctx.getHelper[A].flatMap {
                case Some(helperCall) =>
                  MIO.pure(Rule.matched(helperCall(dctx.elem, dctx.config)))
                case None =>
                  MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def decodeEnumCases[A: DecoderCtx](
        enumType: Enum[A]
    ): MIO[Expr[Either[XmlDecodingError, A]]] = {
      implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
      implicit val StringT: Type[String] = DTypes.String
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val EitherAnyT: Type[Either[XmlDecodingError, Any]] = DTypes.DecoderResultAny
      val childrenList = enumType.directChildren.toList

      NonEmptyList.fromList(childrenList) match {
        case None =>
          val knownNames: List[String] = Nil
          MIO.pure(Expr.quote {
            Left(
              XmlDerivationUtils.failedToMatchSubtype("", Expr.splice(Expr(knownNames)))
            ): Either[XmlDecodingError, A]
          })

        case Some(children) =>
          // Derive all child decoders, collecting their helpers as lambdas
          children
            .traverse { case (childName, child) =>
              import child.Underlying as ChildType
              deriveChildDecoderLambda[A, ChildType](childName)
            }
            .flatMap { childLambdas =>
              val childNames: List[String] = childLambdas.toList.map(_._1)
              val childDecoderExprs: List[Expr[scala.xml.Elem => Either[XmlDecodingError, Any]]] =
                childLambdas.toList.map(_._2)

              // Wrap the final assembly in a LambdaBuilder to get a proper runSafe context
              // for creating Expr(childNames) — this avoids Scala 3 splice isolation issues
              LambdaBuilder
                .of1[scala.xml.Elem]("dispatchElem")
                .traverse { elemExpr =>
                  // Create the names list inside runSafe context
                  val namesListExpr: Expr[List[String]] = Expr(childNames)

                  // Build the decoders list expression using foldRight
                  val decodersListExpr: Expr[List[scala.xml.Elem => Either[XmlDecodingError, Any]]] =
                    childDecoderExprs.foldRight(
                      Expr.quote(List.empty[scala.xml.Elem => Either[XmlDecodingError, Any]])
                    ) { (decoder, acc) =>
                      Expr.quote(Expr.splice(decoder) :: Expr.splice(acc))
                    }

                  MIO.pure(Expr.quote {
                    XmlDerivationUtils.getAttribute(Expr.splice(elemExpr), "type") match {
                      case Right(typeName) =>
                        XmlDerivationUtils.dispatchByName[A](
                          typeName,
                          Expr.splice(elemExpr),
                          Expr.splice(namesListExpr),
                          Expr.splice(decodersListExpr)
                        )
                      case Left(_) =>
                        Left(XmlDecodingError.MissingDiscriminator("type", Expr.splice(elemExpr).label))
                    }
                  })
                }
                .map { builder =>
                  val dispatchLambda = builder.build[Either[XmlDecodingError, A]]
                  Expr.quote {
                    Expr.splice(dispatchLambda)(Expr.splice(dctx.elem))
                  }
                }
            }
      }
    }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveChildDecoderLambda[A: DecoderCtx, ChildType: Type](
        childName: String
    ): MIO[(String, Expr[scala.xml.Elem => Either[XmlDecodingError, Any]])] = {
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val EitherAnyT: Type[Either[XmlDecodingError, Any]] = DTypes.DecoderResultAny

      // Derive via full rules chain (singletons use setHelper too), then use cached helper
      implicit val EitherChildT: Type[Either[XmlDecodingError, ChildType]] = DTypes.DecoderResult[ChildType]
      implicit val ConfigT: Type[XmlConfig] = DTypes.XmlConfig
      deriveDecoderRecursively[ChildType](using dctx.nest[ChildType](dctx.elem)).flatMap { _ =>
        dctx.getHelper[ChildType].flatMap {
          case Some(helper) =>
            // Build a lambda that calls the cached helper
            LambdaBuilder
              .of1[scala.xml.Elem]("childElem")
              .traverse { childElemExpr =>
                val helperCallExpr: Expr[Either[XmlDecodingError, ChildType]] =
                  helper(childElemExpr, dctx.config)
                MIO.pure(Expr.quote {
                  Expr.splice(helperCallExpr).asInstanceOf[Either[XmlDecodingError, Any]]
                })
              }
              .map { builder =>
                val lambda = builder.build[Either[XmlDecodingError, Any]]
                (childName, lambda)
              }
          case None =>
            MIO.fail(
              new RuntimeException(s"No helper found for enum case ${Type[ChildType].prettyPrint}")
            )
        }
      }
    }
  }

}
