package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.xmlderivation.XmlDecodingError

trait DecoderHandleAsSingletonRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsSingletonRule extends DecoderDerivationRule("handle as singleton when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a singleton") >> {
        implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
        implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
        Expr.singletonOf[A] match {
          case Some(_) =>
            // Use setHelper/getHelper so the singleton Expr is created inside the helper's Quotes scope,
            // avoiding splice isolation errors on Scala 3
            for {
              _ <- dctx.setHelper[A] { (_, _) =>
                Expr.singletonOf[A] match {
                  case Some(singletonExpr) =>
                    MIO.pure(Expr.quote(Right(Expr.splice(singletonExpr))))
                  case None =>
                    MIO.fail(new RuntimeException(s"Singleton disappeared for ${Type[A].prettyPrint}"))
                }
              }
              result <- dctx.getHelper[A].flatMap {
                case Some(helperCall) =>
                  MIO.pure(Rule.matched(helperCall(dctx.elem, dctx.config)))
                case None =>
                  MIO.pure(Rule.yielded(s"Failed to build helper for singleton ${Type[A].prettyPrint}"))
              }
            } yield result
          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a singleton"))
        }
      }
  }

}
