package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait AvroEncoderHandleAsCollectionRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  object AvroEncoderHandleAsCollectionRule extends EncoderDerivationRule("handle as collection or map when possible") {
    implicit val AnyT: Type[Any] = EncTypes.Any

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Any]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection or map") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            // A map is an `IsCollectionOf` whose proof is an `IsMapOf`. Parse `IsCollection` ONCE and dispatch on
            // that, instead of a separate map rule that re-parsed `IsCollection` for every non-map field.
            import isCollection.Underlying as Item
            isCollection.value.asMap match {
              case Some(isMapOf) =>
                AvroEncoderHandleAsMapRule.deriveMapEntries[A, Item](isMapOf)
              case None =>
                encodeCollectionInline[A, Item](isCollection.value)
            }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection or map"))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def encodeCollectionInline[A: EncoderCtx, Item: Type](
        isCollection: IsCollectionOf[A, Item]
    ): MIO[Rule.Applicability[Expr[Any]]] = {
      val dummyItem: Expr[Item] = Expr.quote(null.asInstanceOf[Item])
      deriveEncoderRecursively[Item](using ectx.nest(dummyItem)).flatMap { _ =>
        ectx.getHelper[Item].map { helperOpt =>
          val helper = helperOpt.get
          val iterableExpr = isCollection.asIterable(ectx.value)
          Rule.matched(Expr.quote {
            val list = new java.util.ArrayList[Any]()
            val iter = Expr.splice(iterableExpr).iterator
            while (iter.hasNext) {
              val item: Item = iter.next()
              val _ = list.add(Expr.splice(helper(Expr.quote(item), ectx.config)))
            }
            list: Any
          })
        }
      }
    }
  }
}
