package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.yamlderivation.{KindlingsYamlCodec, KindlingsYamlDecoder}
import org.virtuslab.yaml.ConstructError

trait DecoderUseImplicitWhenAvailableRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderUseImplicitWhenAvailableRule extends DecoderDerivationRule("use implicit when available") {

    lazy val ignoredImplicits: Seq[UntypedMethod] =
      Type.of[KindlingsYamlDecoder.type].unsortedMethods.collect {
        case method if method.isImplicit => method.asUntyped
      } ++ Type.of[KindlingsYamlCodec.type].unsortedMethods.collect {
        case method if method.isImplicit => method.asUntyped
      }

    /** scala-yaml's `YamlDecoder.forMap` is not used for a map: Kindlings derives maps itself, so that keys go through
      * their key codecs (`YamlKeyDecoder`) instead of scala-yaml's `YamlDecoder[K]` (a user-provided instance for the
      * whole map type is still used).
      *
      * Nor are its `forList`/`forSeq`/`forSet`: the derived collection decoding reports the same failure (the first
      * failing item, or scala-yaml's failure for a node that is not a sequence), but stops at it instead of decoding
      * every item first (and with `YamlConfig.withAccumulatingErrors` reports every failing item with its index).
      */
    lazy val ignoredImplicitsAndScalaYamlMaps: Seq[UntypedMethod] =
      ignoredImplicits ++ List("forMap", "forList", "forSeq", "forSet").flatMap { name =>
        Type.of[org.virtuslab.yaml.YamlDecoder.type].unsortedMethodsNamed(name).map(_.asUntyped)
      }

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Attempting to use implicit YamlDecoder for ${Type[A].prettyPrint}") >> {
        if (dctx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          DTypes.YamlDecoder[A].summonExprIgnoring(ignoredImplicitsAndScalaYamlMaps*).toEither match {
            case Right(instanceExpr) => cacheAndUse[A](instanceExpr)
            case Left(reason)        => yieldUnsupported[A](reason)
          }
      }

    private def cacheAndUse[A: DecoderCtx](
        instanceExpr: Expr[org.virtuslab.yaml.YamlDecoder[A]]
    ): MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Found implicit decoder ${instanceExpr.prettyPrint}, using directly") >>
        MIO.pure(Rule.matched(Expr.quote {
          Expr.splice(instanceExpr).construct(Expr.splice(dctx.node))(org.virtuslab.yaml.LoadSettings.empty)
        }))

    private def yieldUnsupported[A: DecoderCtx](
        reason: String
    ): MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      MIO.pure(
        Rule.yielded(
          s"The type ${Type[A].prettyPrint} does not have an implicit YamlDecoder instance: $reason"
        )
      )
  }
}
