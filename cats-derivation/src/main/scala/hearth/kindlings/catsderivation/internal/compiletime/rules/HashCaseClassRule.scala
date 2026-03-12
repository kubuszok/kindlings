package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

trait HashCaseClassRuleImpl {
  this: HashMacrosImpl & MacroCommons & StdExtensions =>

  object HashCaseClassRule extends HashDerivationRule("Hash as case class") {

    def apply[A: HashCtx]: MIO[Rule.Applicability[Expr[Int]]] =
      CaseClass.parse[A].toEither match {
        case Right(caseClass) =>
          implicit val IntType: Type[Int] = HashTypes.Int
          val defBuilder = ValDefBuilder.ofDef1[A, Int](s"hash_${Type[A].shortName}")
          for {
            _ <- hctx.cache.forwardDeclare("cached-hash-method", defBuilder)
            _ <- MIO.scoped { runSafe =>
              runSafe(hctx.cache.buildCachedWith("cached-hash-method", defBuilder) { case (_, value) =>
                runSafe(deriveCaseClassHash[A](caseClass, value))
              })
            }
            result <- HashUseCachedRule[A]
          } yield result
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }

    private def deriveCaseClassHash[A: HashCtx](
        caseClass: CaseClass[A],
        value: Expr[A]
    ): MIO[Expr[Int]] = {
      implicit val IntType: Type[Int] = HashTypes.Int
      val fields = caseClass.caseFieldValuesAt(value).toList

      NonEmptyList.fromList(fields) match {
        case Some(fieldValues) =>
          fieldValues
            .traverse { case (fieldName, fieldValue) =>
              import fieldValue.{Underlying as Field, value as fieldExpr}
              Log.namedScope(s"Deriving Hash for field $fieldName: ${Field.prettyPrint}") {
                deriveHashRecursively[Field](using hctx.nest(fieldExpr)).map(r => (fieldName, r))
              }
            }
            .map { hashes =>
              val seed = Expr(scala.util.hashing.MurmurHash3.productSeed)
              val mixed = hashes.toList.foldLeft(seed) { case (acc, (_, h)) =>
                Expr.quote(scala.util.hashing.MurmurHash3.mix(Expr.splice(acc), Expr.splice(h)))
              }
              val size = Expr(fields.size)
              Expr.quote {
                scala.util.hashing.MurmurHash3.finalizeHash(Expr.splice(mixed), Expr.splice(size))
              }
            }
        case None =>
          MIO.pure(Expr(Type[A].shortName.hashCode))
      }
    }
  }
}
