package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.xmlderivation.XmlDecodingError

trait DecoderHandleAsCollectionRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsCollectionRule extends DecoderDerivationRule("handle as collection when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
        implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
        implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            import isCollection.value.CtorResult
            implicit val EitherItemT: Type[Either[XmlDecodingError, Item]] = DTypes.DecoderResult[Item]
            LambdaBuilder
              .of1[scala.xml.Elem]("collItemElem")
              .traverse { itemElemExpr =>
                deriveDecoderRecursively[Item](using dctx.nest[Item](itemElemExpr))
              }
              .map { builder =>
                val lambda = builder.build[Either[XmlDecodingError, Item]]
                val factoryExpr = isCollection.value.factory
                val buildStep = isCollection.value.build

                val readLoop: Expr[scala.collection.mutable.Builder[Item, CtorResult]] = Expr.quote {
                  hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils
                    .decodeCollectionBuilder(
                      Expr.splice(dctx.elem),
                      Expr.splice(lambda),
                      Expr.splice(factoryExpr)
                    )
                }
                val buildResultExpr = buildStep.ctor(readLoop)

                buildStep match {
                  case _: CtorLikeOf.PlainValue[?, ?] =>
                    Rule.matched(Expr.quote {
                      try Right(Expr.splice(buildResultExpr.asInstanceOf[Expr[A]]))
                      catch {
                        case e: hearth.kindlings.xmlderivation.internal.runtime.XmlCollectionBuildException =>
                          Left(XmlDecodingError.Multiple(e.errors))
                      }
                    })

                  case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[String, A]]]
                    Rule.matched(Expr.quote {
                      try
                        Expr.splice(eitherExpr) match {
                          case Right(value) => Right(value)
                          case Left(err)    => Left(XmlDecodingError.General(err))
                        }
                      catch {
                        case e: hearth.kindlings.xmlderivation.internal.runtime.XmlCollectionBuildException =>
                          Left(XmlDecodingError.Multiple(e.errors))
                      }
                    })

                  case _: CtorLikeOf.EitherIterableStringOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[String], A]]]
                    Rule.matched(Expr.quote {
                      try
                        Expr.splice(eitherExpr) match {
                          case Right(value) => Right(value)
                          case Left(errs)   => Left(XmlDecodingError.General(errs.mkString("\n")))
                        }
                      catch {
                        case e: hearth.kindlings.xmlderivation.internal.runtime.XmlCollectionBuildException =>
                          Left(XmlDecodingError.Multiple(e.errors))
                      }
                    })

                  case _: CtorLikeOf.EitherThrowableOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Throwable, A]]]
                    Rule.matched(Expr.quote {
                      try
                        Expr.splice(eitherExpr) match {
                          case Right(value) => Right(value)
                          case Left(err)    => Left(XmlDecodingError.General(err.getMessage))
                        }
                      catch {
                        case e: hearth.kindlings.xmlderivation.internal.runtime.XmlCollectionBuildException =>
                          Left(XmlDecodingError.Multiple(e.errors))
                      }
                    })

                  case _: CtorLikeOf.EitherIterableThrowableOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[Throwable], A]]]
                    Rule.matched(Expr.quote {
                      try
                        Expr.splice(eitherExpr) match {
                          case Right(value) => Right(value)
                          case Left(errs)   =>
                            Left(XmlDecodingError.General(errs.map(_.getMessage).mkString("\n")))
                        }
                      catch {
                        case e: hearth.kindlings.xmlderivation.internal.runtime.XmlCollectionBuildException =>
                          Left(XmlDecodingError.Multiple(e.errors))
                      }
                    })
                }
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }

}
