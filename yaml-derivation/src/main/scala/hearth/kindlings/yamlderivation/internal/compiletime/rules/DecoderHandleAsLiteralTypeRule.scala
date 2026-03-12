package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.virtuslab.yaml.{ConstructError, Node}

trait DecoderHandleAsLiteralTypeRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsLiteralTypeRule extends DecoderDerivationRule("handle as literal type when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a literal type") >> {
        implicit val NodeT: Type[Node] = DTypes.Node
        implicit val CET: Type[ConstructError] = DTypes.ConstructError
        implicit val EitherCEA: Type[Either[ConstructError, A]] = DTypes.DecoderResult[A]
        extractLiteralDecoder[A] match {
          case Some(expr) => MIO.pure(Rule.matched(expr))
          case None       => MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a literal type"))
        }
      }

    private def decodeLiteralFromScalar[A: DecoderCtx, U](
        codec: TypeCodec[U]
    )(implicit
        exprCodec: ExprCodec[U],
        ut: Type[U],
        NodeT: Type[Node],
        CET: Type[ConstructError],
        EitherCEA: Type[Either[ConstructError, A]]
    ): Option[Expr[Either[ConstructError, A]]] =
      codec.fromType(Type[A]).map { e =>
        val constant: U = e.value
        val constantStr: String = constant.toString
        Expr.quote {
          Expr.splice(dctx.node) match {
            case Node.ScalarNode(raw, _) =>
              if (raw == Expr.splice(Expr(constantStr)))
                Right(Expr.splice(Expr(constant)).asInstanceOf[A])
              else
                Left(
                  ConstructError.from(
                    "Expected literal value " + Expr.splice(Expr(constantStr)) + " but got " + raw,
                    Expr.splice(dctx.node)
                  )
                )
            case other =>
              Left(ConstructError.from("Expected scalar node for literal type", other))
          }
        }
      }

    private def extractLiteralDecoder[A: DecoderCtx](implicit
        NodeT: Type[Node],
        CET: Type[ConstructError],
        EitherCEA: Type[Either[ConstructError, A]]
    ): Option[Expr[Either[ConstructError, A]]] = {
      implicit val StringT: Type[String] = DTypes.String
      implicit val IntT: Type[Int] = DTypes.Int
      implicit val LongT: Type[Long] = DTypes.Long
      implicit val BooleanT: Type[Boolean] = DTypes.Boolean
      implicit val DoubleT: Type[Double] = DTypes.Double
      decodeLiteralFromScalar(Type.StringCodec)
        .orElse(decodeLiteralFromScalar(Type.IntCodec))
        .orElse(decodeLiteralFromScalar(Type.LongCodec))
        .orElse(decodeLiteralFromScalar(Type.BooleanCodec))
        .orElse(decodeLiteralFromScalar(Type.DoubleCodec))
    }
  }
}
