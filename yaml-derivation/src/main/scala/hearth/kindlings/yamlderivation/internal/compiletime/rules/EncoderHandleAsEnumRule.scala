package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.yamlderivation.YamlConfig
import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils
import org.virtuslab.yaml.Node
import hearth.kindlings.derivation.compiletime.StableChildrenOrder

trait EncoderHandleAsEnumRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsEnumRule extends EncoderDerivationRule("handle as enum when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Node]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        parseEnum[A].toEither match {
          case Right(enumm) =>
            encodeEnumCases[A](enumm).map(Rule.matched)
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def encodeEnumCases[A: EncoderCtx](
        enumm: Enum[A]
    ): MIO[Expr[Node]] = {
      implicit val NodeT: Type[Node] = Types.Node
      implicit val StringT: Type[String] = Types.String

      val childrenList = StableChildrenOrder.directChildren(EncoderHandleAsEnumRuleImpl.this)(enumm)
      val isEnumerationOrJavaEnum = Type[A].isEnumeration || Type[A].isJavaEnum
      // Union types have no scala-yaml counterpart: their members stay wrapped (untagged members would be ambiguous)
      val alwaysWrapped: Boolean = Type[A].isUnionType
      val allCaseObjects = isEnumerationOrJavaEnum || childrenList.forall { case (_, child) =>
        SingletonValue.unapply(child.Underlying).isDefined
      }
      val evaluated: Option[YamlConfig] = ectx.evaluatedConfig

      // The subtype names: constants with a configuration known at compile time, otherwise computed once per instance.
      val namesMIO: MIO[String => Expr[String]] = evaluated match {
        case Some(config) => MIO.pure((name: String) => Expr(config.transformConstructorNames(name)))
        case None         => ectx.transformedConstructorNames(childrenList.map(_._1))
      }

      namesMIO.flatMap { nameOf =>
        enumm
          .parMatchOn[MIO, Node](ectx.value) { matched =>
            import matched.{value as enumCaseValue, Underlying as EnumCase}
            Log.namedScope(s"Encoding enum case ${enumCaseValue.prettyPrint}: ${EnumCase.prettyPrint}") {
              val caseName: String = childrenList
                .find { case (_, child) =>
                  import child.Underlying as ChildType
                  Type[EnumCase] <:< Type[ChildType]
                }
                .map(_._1)
                .getOrElse(Type[EnumCase].shortName)
              val nameExpr: Expr[String] = nameOf(caseName)
              def caseNodeMIO: MIO[Expr[Node]] =
                if (isEnumerationOrJavaEnum) MIO.pure(Expr.quote(YamlDerivationUtils.nodeFromFields(Nil): Node))
                else deriveEncoderRecursively[EnumCase](using ectx.nest(enumCaseValue))
              def asString: Expr[Node] = Expr.quote(YamlDerivationUtils.encodeEnumAsString(Expr.splice(nameExpr)))

              evaluated match {
                // With a configuration known at compile time only the applicable encoding is generated (and the
                // case's node is not derived when it is written as its name).
                case Some(config) if allCaseObjects && config.enumAsStrings => MIO.pure(asString)
                case Some(config)                                           =>
                  caseNodeMIO.map { caseNode =>
                    config.discriminator match {
                      case Some(field) =>
                        Expr.quote(
                          YamlDerivationUtils
                            .addDiscriminator(Expr.splice(Expr(field)), Expr.splice(nameExpr), Expr.splice(caseNode))
                        )
                      case None if config.wrappedSubtypes || alwaysWrapped =>
                        Expr.quote(YamlDerivationUtils.wrapWithTypeName(Expr.splice(nameExpr), Expr.splice(caseNode)))
                      // scala-yaml's untagged encoding (a case object is `{}`); Java enum / Enumeration values, which
                      // scala-yaml does not support, are written as their names
                      case None if isEnumerationOrJavaEnum => asString
                      case None                            => caseNode
                    }
                  }
                // A configuration only known at runtime: the tagging is a runtime helper call, so that the `match` over
                // many subtypes stays small. Java enum / Enumeration values (not supported by scala-yaml) are written
                // as their names when untagged.
                case None if allCaseObjects =>
                  caseNodeMIO.map { caseNode =>
                    Expr.quote {
                      val config = Expr.splice(ectx.config)
                      val name = Expr.splice(nameExpr)
                      if (config.enumAsStrings) YamlDerivationUtils.encodeEnumAsString(name)
                      else
                        YamlDerivationUtils.tagSubtype(
                          config,
                          name,
                          Expr.splice(caseNode),
                          Expr.splice(Expr(alwaysWrapped)),
                          Expr.splice(Expr(isEnumerationOrJavaEnum))
                        )
                    }
                  }
                case None =>
                  caseNodeMIO.map { caseNode =>
                    Expr.quote {
                      YamlDerivationUtils.tagSubtype(
                        Expr.splice(ectx.config),
                        Expr.splice(nameExpr),
                        Expr.splice(caseNode),
                        Expr.splice(Expr(alwaysWrapped)),
                        false
                      )
                    }
                  }
              }
            }
          }
          .flatMap {
            case Some(result) => MIO.pure(result)
            case None         =>
              val err = EncoderDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint)
              Log.error(err.message) >> MIO.fail(err)
          }
      }
    }
  }
}
