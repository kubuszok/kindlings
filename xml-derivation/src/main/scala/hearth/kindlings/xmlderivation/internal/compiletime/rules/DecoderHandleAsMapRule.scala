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
            Rule.matched(Expr.quote {
              val parentElem = Expr.splice(dctx.elem)
              val entries =
                hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils.getChildElems(parentElem, "entry")
              val results: List[Either[XmlDecodingError, (String, Value)]] = entries.map { entryElem =>
                hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils
                  .getAttribute(entryElem, "key")
                  .flatMap { key =>
                    val valueElems = (entryElem \ "value").collect { case e: scala.xml.Elem => e }.toList
                    valueElems.headOption match {
                      case Some(valueElem) =>
                        Expr.splice(lambda).apply(valueElem).map(v => (key, v))
                      case None =>
                        Left(XmlDecodingError.MissingElement("value", entryElem.label))
                    }
                  }
              }
              val errors = results.collect { case Left(e) => e }
              if (errors.nonEmpty) Left(XmlDecodingError.Multiple(errors))
              else {
                val pairs = results.collect { case Right(p) => p }
                Right(pairs.toMap.asInstanceOf[A])
              }
            })
          }
      }
    }
  }

}
