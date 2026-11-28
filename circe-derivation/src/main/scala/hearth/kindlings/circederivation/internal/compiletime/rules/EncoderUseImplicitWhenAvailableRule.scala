package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.circederivation.{KindlingsCodecAsObject, KindlingsEncoder}
import io.circe.{Encoder, Json}

trait EncoderUseImplicitWhenAvailableRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderUseImplicitWhenAvailableRule extends EncoderDerivationRule("use implicit when available") {

    // Ignore ALL implicit/given methods from Kindlings companion objects (our auto-derivation).
    // For circe's own companions, only ignore specific auto-derivation methods (not built-in
    // instances like encodeInt, encodeString, etc.).
    lazy val ignoredImplicits: Seq[UntypedMethod] =
      Type.of[KindlingsEncoder.type].unsortedMethods.collect {
        case method if method.isImplicit => method.asUntyped
      } ++ Type.of[KindlingsCodecAsObject.type].unsortedMethods.collect {
        case method if method.isImplicit => method.asUntyped
      } ++ Type.of[Encoder.type].unsortedMethods.collect {
        case method if method.name == "derived" || method.name.startsWith("encodeLiteral") =>
          method.asUntyped
      }

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Attempting to use implicit Encoder for ${Type[A].prettyPrint}") >> {
        // Skip implicit search for the self type being derived to prevent self-referential loops
        // (e.g., `implicit val enc: Encoder[X] = KindlingsEncoder.derived[X]` would otherwise
        // find `enc` itself during macro expansion, generating code that calls itself infinitely).
        if (ectx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else {
          implicit val EncoderA: Type[Encoder[A]] = Types.Encoder[A]
          summonUserInstance[Encoder[A]](ignoredImplicits) match {
            case UserInstanceSearch.Found(instanceExpr) => cacheAndUse[A](instanceExpr)
            case UserInstanceSearch.Crashed(reason)     =>
              failOnCrashedUserInstance[Encoder[A], Rule.Applicability[Expr[Json]]](reason)
            case UserInstanceSearch.NotFound(reason) =>
              // a user's generic instance that needs instances Kindlings derives (e.g. `Encoder[Page[Item]]` built
              // from a derived `Encoder[Item]`)
              completeUserInstance[Encoder, A](Types.Encoder, ignoredImplicits)(dependencyEncoder[A]).flatMap {
                case Some(instanceExpr) => cacheAndUse[A](instanceExpr)
                case None               => yieldUnsupported[A](reason)
              }
          }
        }
      }

    /** The derived `Encoder[B]` that a user's instance needs: the cached encoding helper of `B` wrapped in an instance
      * (a lazy val of the derived instance), so a recursive type passes its own encoding.
      */
    @scala.annotation.nowarn("msg=is never used")
    private def dependencyEncoder[A: EncoderCtx]: DependencyInstance[Encoder] = new DependencyInstance[Encoder] {
      def apply[B: Type]: MIO[Expr[Encoder[B]]] = {
        implicit val JsonT: Type[Json] = Types.Json
        implicit val EncoderB: Type[Encoder[B]] = Types.Encoder[B]
        implicit val KindlingsEncoderB: Type[hearth.kindlings.circederivation.KindlingsEncoder[B]] =
          Types.KindlingsEncoder[B]
        val ctx = ectx.nest[B](Expr.quote(null.asInstanceOf[B]))
        for {
          _ <- deriveEncoderRecursively[B](using ctx)
          helper <- ctx.getHelper[B]
          encode = helper.get
          instance <- ctx.cachedInstance[B](
            Expr
              .quote {
                hearth.kindlings.circederivation.internal.runtime.CirceDerivationFactories.encoderInstance[B] {
                  (b: B) =>
                    val _ = b
                    Expr.splice(encode(Expr.quote(b), ctx.config))
                }
              }
              .upcast[Encoder[B]]
          )
        } yield instance
      }
    }

    private def cacheAndUse[A: EncoderCtx](
        instanceExpr: Expr[Encoder[A]]
    ): MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Found implicit encoder ${instanceExpr.prettyPrint}, caching it in the instance") >>
        ectx.cachedInstance[A](instanceExpr).map { cached =>
          Rule.matched(Expr.quote {
            Expr.splice(cached).apply(Expr.splice(ectx.value))
          })
        }

    private def yieldUnsupported[A: EncoderCtx](reason: String): MIO[Rule.Applicability[Expr[Json]]] =
      MIO.pure(
        Rule.yielded(
          s"The type ${Type[A].prettyPrint} does not have an implicit Encoder instance: $reason"
        )
      )
  }
}
