package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils
import com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter

trait EncoderHandleAsCollectionRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsCollectionRule extends EncoderDerivationRule("handle as collection when possible") {
    implicit val UnitT: Type[Unit] = CTypes.Unit

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            if (ectx.evaluatedConfig.isDefined) encodeCollectionInline[A, Item](isCollection.value)
            else encodeCollectionViaHelper[A, Item](isCollection.value)

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def encodeCollectionInline[A: EncoderCtx, Item: Type](
        isCollection: IsCollectionOf[A, Item]
    ): MIO[Rule.Applicability[Expr[Unit]]] = {
      implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
      val dummyItem: Expr[Item] = Expr.quote(null.asInstanceOf[Item])
      deriveEncoderRecursively[Item](using ectx.nest(dummyItem)).flatMap { _ =>
        ectx.getHelper[Item].map { helperOpt =>
          val helper = helperOpt.get
          val iterableExpr = isCollection.asIterable(ectx.value)
          Rule.matched(Expr.quote {
            Expr.splice(ectx.writer).writeArrayStart()
            val iter = Expr.splice(iterableExpr).iterator
            while (iter.hasNext) {
              val item: Item = iter.next()
              Expr.splice(helper(Expr.quote(item), ectx.writer, ectx.config))
            }
            Expr.splice(ectx.writer).writeArrayEnd()
          })
        }
      }
    }

    private def encodeCollectionViaHelper[A: EncoderCtx, Item: Type](
        isCollection: IsCollectionOf[A, Item]
    ): MIO[Rule.Applicability[Expr[Unit]]] =
      LambdaBuilder
        .of1[Item]("item")
        .traverse { itemExpr =>
          deriveEncoderRecursively[Item](using ectx.nest(itemExpr))
        }
        .map { builder =>
          val lambda = builder.build[Unit]
          val iterableExpr = isCollection.asIterable(ectx.value)
          Rule.matched(Expr.quote {
            JsoniterDerivationUtils.writeArray[Item](
              Expr.splice(ectx.writer),
              Expr.splice(iterableExpr),
              Expr.splice(lambda)
            )
          })
        }
  }

}
