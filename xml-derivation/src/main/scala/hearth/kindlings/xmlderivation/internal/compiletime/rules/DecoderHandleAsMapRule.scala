package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.xmlderivation.XmlDecodingError

trait DecoderHandleAsMapRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsMapRule extends DecoderDerivationRule("handle as map when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a map") >> {
        Type[A] match {
          case IsMap(isMap) =>
            import isMap.Underlying as Pair
            decodeMapEntries[A, Pair](isMap.value)

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a map"))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def decodeMapEntries[A: DecoderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] = {
      import isMap.{Key, Value, CtorResult}
      implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
      implicit val StringT: Type[String] = DTypes.String
      if (!(Key <:< Type[String]))
        MIO.pure(Rule.yielded(s"Map key type ${Key.prettyPrint} is not String"))
      else {
        LambdaBuilder
          .of1[scala.xml.Elem]("mapEntryElem")
          .traverse { entryElemExpr =>
            deriveDecoderRecursively[Value](using dctx.nest[Value](entryElemExpr))
          }
          .map { builder =>
            implicit val EitherValueT: Type[Either[XmlDecodingError, Value]] = DTypes.DecoderResult[Value]
            val lambda = builder.build[Either[XmlDecodingError, Value]]
            val factoryExpr = isMap.factory
            val buildStep = isMap.build

            val readLoop: Expr[scala.collection.mutable.Builder[Pair, CtorResult]] = Expr.quote {
              hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils
                .decodeMapBuilder(
                  Expr.splice(dctx.elem),
                  Expr.splice(lambda),
                  Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[(String, Value), CtorResult]]
                )
                .asInstanceOf[scala.collection.mutable.Builder[Pair, CtorResult]]
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
      }
    }
  }

}
