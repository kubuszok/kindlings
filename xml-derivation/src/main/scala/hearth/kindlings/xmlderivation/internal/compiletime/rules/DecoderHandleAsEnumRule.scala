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
            decodeEnumCases[A](parsedEnum).map(Rule.matched)
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
      implicit val XmlConfigT: Type[XmlConfig] = DTypes.XmlConfig
      val childrenList = enumType.directChildren.toList

      // Pre-compute discriminator attribute name from config
      val discriminatorAttr: Option[String] = dctx.evaluatedConfig.flatMap(_.discriminatorAttribute)
      val discriminatorAttrName: String = discriminatorAttr.getOrElse("type")
      val hasDiscriminator: Option[Boolean] = dctx.evaluatedConfig.map(_.discriminatorAttribute.isDefined)

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
              // Apply constructorNameMapper when evaluatedConfig is available
              val mappedChildName: String = dctx.evaluatedConfig match {
                case Some(cfg) => cfg.constructorNameMapper(childName)
                case None      => childName
              }
              deriveChildDecoderLambda[A, ChildType](mappedChildName)
            }
            .map { childLambdas =>
              val childNames: List[String] = childLambdas.toList.map(_._1)
              val childDecoderExprs: List[Expr[scala.xml.Elem => Either[XmlDecodingError, Any]]] =
                childLambdas.toList.map(_._2)

              // When evaluatedConfig is not available, the child names are unmapped.
              // Build a runtime-mapped names list using config.constructorNameMapper.
              val namesListExpr: Expr[List[String]] = dctx.evaluatedConfig match {
                case Some(_) => Expr(childNames) // Already pre-mapped at compile time
                case None    =>
                  // Map child names at runtime via config.constructorNameMapper
                  val rawNamesExpr: Expr[List[String]] = Expr(childNames)
                  Expr.quote {
                    Expr.splice(rawNamesExpr).map(Expr.splice(dctx.config).constructorNameMapper)
                  }
              }
              val decodersListExpr: Expr[List[scala.xml.Elem => Either[XmlDecodingError, Any]]] =
                childDecoderExprs.foldRight(
                  Expr.quote(List.empty[scala.xml.Elem => Either[XmlDecodingError, Any]])
                ) { (decoder, acc) =>
                  Expr.quote(Expr.splice(decoder) :: Expr.splice(acc))
                }

              hasDiscriminator match {
                case Some(true) =>
                  // Discriminator is statically known to be present
                  Expr.quote {
                    XmlDerivationUtils.getAttribute(
                      Expr.splice(dctx.elem),
                      Expr.splice(Expr(discriminatorAttrName))
                    ) match {
                      case Right(typeName) =>
                        XmlDerivationUtils.dispatchByName[A](
                          typeName,
                          Expr.splice(dctx.elem),
                          Expr.splice(namesListExpr),
                          Expr.splice(decodersListExpr)
                        )
                      case Left(_) =>
                        Left(
                          XmlDecodingError
                            .MissingDiscriminator(
                              Expr.splice(Expr(discriminatorAttrName)),
                              Expr.splice(dctx.elem).label
                            )
                        )
                    }
                  }
                case Some(false) =>
                  // No discriminator — try matching by wrapper element name
                  Expr.quote {
                    XmlDerivationUtils.decodeWrapped(Expr.splice(dctx.elem)) match {
                      case Right((typeName, innerElem)) =>
                        XmlDerivationUtils.dispatchByName[A](
                          typeName,
                          innerElem,
                          Expr.splice(namesListExpr),
                          Expr.splice(decodersListExpr)
                        )
                      case Left(err) => Left(err)
                    }
                  }
                case None =>
                  // evaluatedConfig not available — use runtime check
                  Expr.quote {
                    val config = Expr.splice(dctx.config)
                    config.discriminatorAttribute match {
                      case scala.Some(attr) =>
                        XmlDerivationUtils.getAttribute(Expr.splice(dctx.elem), attr) match {
                          case Right(typeName) =>
                            XmlDerivationUtils.dispatchByName[A](
                              typeName,
                              Expr.splice(dctx.elem),
                              Expr.splice(namesListExpr),
                              Expr.splice(decodersListExpr)
                            )
                          case Left(_) =>
                            Left(XmlDecodingError.MissingDiscriminator(attr, Expr.splice(dctx.elem).label))
                        }
                      case scala.None =>
                        XmlDerivationUtils.decodeWrapped(Expr.splice(dctx.elem)) match {
                          case Right((typeName, innerElem)) =>
                            XmlDerivationUtils.dispatchByName[A](
                              typeName,
                              innerElem,
                              Expr.splice(namesListExpr),
                              Expr.splice(decodersListExpr)
                            )
                          case Left(err) => Left(err)
                        }
                    }
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
            val configExpr = dctx.config
            val lambda: Expr[scala.xml.Elem => Either[XmlDecodingError, Any]] =
              Expr.quote { (childElem: scala.xml.Elem) =>
                Expr
                  .splice(helper(Expr.quote(childElem), configExpr))
                  .asInstanceOf[Either[XmlDecodingError, Any]]
              }
            MIO.pure((childName, lambda))
          case None =>
            MIO.fail(
              new RuntimeException(s"No helper found for enum case ${Type[ChildType].prettyPrint}")
            )
        }
      }
    }
  }

}
