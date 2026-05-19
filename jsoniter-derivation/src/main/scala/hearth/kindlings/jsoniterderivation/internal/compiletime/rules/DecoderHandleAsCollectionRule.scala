package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils
import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader

trait DecoderHandleAsCollectionRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsCollectionRule extends DecoderDerivationRule("handle as collection when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader

            if (dctx.evaluatedConfig.isDefined) decodeCollectionInline[A, Item](isCollection.value)
            else decodeCollectionViaHelper[A, Item](isCollection.value)

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def decodeCollectionInline[A: DecoderCtx, Item: Type](
        isCollection: IsCollectionOf[A, Item]
    )(implicit JsonReaderT: Type[JsonReader]): MIO[Rule.Applicability[Expr[A]]] = {
      implicit val IntT: Type[Int] = CTypes.Int
      val maxInserts = dctx.evaluatedConfig.get.setMaxInsertNumber
      import isCollection.CtorResult
      deriveDecoderRecursively[Item](using dctx.nest[Item](dctx.reader)).map { decodeItemExpr =>
        val factoryExpr = isCollection.factory
        Rule.matched(Expr.quote {
          val reader = Expr.splice(dctx.reader)
          val builder = Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[Item, A]].newBuilder
          if (!reader.isNextToken('['.toByte)) reader.decodeError("expected '['")
          if (!reader.isNextToken(']'.toByte)) {
            reader.rollbackToken()
            var count: Int = 1
            builder += Expr.splice(decodeItemExpr)
            while (reader.isNextToken(','.toByte)) {
              count += 1
              if (count > Expr.splice(Expr(maxInserts)))
                reader.decodeError("too many collection items (max: " + Expr.splice(Expr(maxInserts)) + ")")
              builder += Expr.splice(decodeItemExpr)
            }
            if (!reader.isCurrentToken(']'.toByte)) reader.decodeError("expected ']' or ','")
          }
          builder.result().asInstanceOf[A]
        })
      }
    }

    private def decodeCollectionViaHelper[A: DecoderCtx, Item: Type](
        isCollection: IsCollectionOf[A, Item]
    )(implicit JsonReaderT: Type[JsonReader]): MIO[Rule.Applicability[Expr[A]]] = {
      import isCollection.CtorResult
      LambdaBuilder
        .of1[JsonReader]("itemReader")
        .traverse { itemReaderExpr =>
          deriveDecoderRecursively[Item](using dctx.nest[Item](itemReaderExpr))
        }
        .map { builder =>
          val decodeFn = builder.build[Item]
          val factoryExpr = isCollection.factory
          Rule.matched(Expr.quote {
            JsoniterDerivationUtils
              .readCollection[Item, A](
                Expr.splice(dctx.reader),
                Expr.splice(decodeFn),
                Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[Item, A]],
                Expr.splice(dctx.config).setMaxInsertNumber
              )
              .asInstanceOf[A]
          })
        }
    }
  }

}
