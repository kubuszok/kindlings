package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

/** Hash derivation: combines Eq (field-by-field equality) with Hash (MurmurHash3-based hashing). */
trait HashMacrosImpl extends EqMacrosImpl { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  def deriveHash[A: Type]: Expr[cats.kernel.Hash[A]] = {
    val macroName = "Hash.derived"
    implicit val HashA: Type[cats.kernel.Hash[A]] = HashTypes.Hash[A]

    deriveHashEntrypoint[A, cats.kernel.Hash[A]](macroName)
  }

  private def deriveHashEntrypoint[A: Type, Out: Type](macroName: String): Expr[Out] = {
    implicit val BooleanType: Type[Boolean] = EqTypes.Boolean
    implicit val IntType: Type[Int] = HashTypes.Int
    val selfType: Option[??] = Some(Type[A].as_??)

    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended."
      )

    Log
      .namedScope(s"Deriving $macroName[${Type[A].prettyPrint}] at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          // Build hash lambda: A => Int
          val hashLambdaExpr: Expr[A => Int] = runSafe {
            LambdaBuilder
              .of1[A]("hashValue")
              .traverse { valueExpr =>
                for {
                  _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                  result <- deriveHashForValue[A](valueExpr, selfType)
                } yield result
              }
              .map(_.build[Int])
          }

          // Build eqv lambda: (A, A) => Boolean
          val eqLambdaExpr: Expr[(A, A) => Boolean] = runSafe {
            val eqCache = ValDefsCache.mlocal
            LambdaBuilder
              .of2[A, A]("eqX", "eqY")
              .traverse { case (xExpr, yExpr) =>
                val ctx = EqCtx[A](Type[A], xExpr, yExpr, eqCache, selfType)
                for {
                  _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                  result <- deriveEqRecursively[A](using ctx)
                  cache <- ctx.cache.get
                } yield cache.toValDefs.use(_ => result)
              }
              .map(_.build[Boolean])
          }

          // Single Expr.quote with both lambdas spliced — avoids sibling splice isolation
          Expr
            .quote {
              new cats.kernel.Hash[A] {
                def hash(value: A): Int = Expr.splice(hashLambdaExpr).apply(value)
                def eqv(x: A, y: A): Boolean = Expr.splice(eqLambdaExpr).apply(x, y)
              }
            }
            .asInstanceOf[Expr[Out]]
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogEqDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogEqDerivation) RenderFrom(Log.Level.Info) else DontRender
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  private def deriveHashForValue[A: Type](
      value: Expr[A],
      derivedType: Option[??]
  ): MIO[Expr[Int]] = {
    implicit val IntType: Type[Int] = HashTypes.Int

    val skipSelf = derivedType.exists(_.Underlying =:= Type[A])
    val implicitResult = if (skipSelf) None else HashTypes.Hash[A].summonExprIgnoring().toOption

    implicitResult match {
      case Some(instanceExpr) =>
        MIO.pure(Expr.quote(Expr.splice(instanceExpr).hash(Expr.splice(value))))
      case None =>
        deriveHashDirect[A](value, derivedType)
    }
  }

  private def deriveHashDirect[A: Type](
      value: Expr[A],
      derivedType: Option[??]
  ): MIO[Expr[Int]] = {
    implicit val IntType: Type[Int] = HashTypes.Int

    CaseClass.parse[A].toEither match {
      case Right(caseClass) =>
        val fields = caseClass.caseFieldValuesAt(value).toList
        NonEmptyList.fromList(fields) match {
          case Some(fieldValues) =>
            fieldValues
              .traverse { case (fieldName, fieldValue) =>
                import fieldValue.{Underlying as Field, value as fieldExpr}
                Log.namedScope(s"Deriving hash for $fieldName: ${Field.prettyPrint}") {
                  deriveHashForValue[Field](fieldExpr, derivedType).map(r => (fieldName, r))
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
      case Left(_) =>
        MIO.pure(Expr.quote(Expr.splice(value).hashCode()))
    }
  }

  protected object HashTypes {
    def Hash: Type.Ctor1[cats.kernel.Hash] = Type.Ctor1.of[cats.kernel.Hash]
    val Int: Type[Int] = Type.of[Int]
  }
}
