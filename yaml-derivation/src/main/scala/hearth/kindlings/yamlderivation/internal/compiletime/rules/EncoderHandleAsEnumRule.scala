package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils
import org.virtuslab.yaml.Node
import hearth.kindlings.derivation.compiletime.StableChildrenOrder

trait EncoderHandleAsEnumRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsEnumRule extends EncoderDerivationRule("handle as enum when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Node]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            encodeEnumCases[A](enumm).map(Rule.matched)
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    private def encodeEnumCases[A: EncoderCtx](
        enumm: Enum[A]
    ): MIO[Expr[Node]] = {
      implicit val NodeT: Type[Node] = Types.Node

      val childrenList = StableChildrenOrder.directChildren(EncoderHandleAsEnumRuleImpl.this)(enumm)
      val isEnumerationOrJavaEnum = Type[A].isEnumeration || Type[A].isJavaEnum
      // Union types have no scala-yaml counterpart: their members stay wrapped (untagged members would be ambiguous)
      val alwaysWrapped: Boolean = Type[A].isUnionType
      val allCaseObjects = isEnumerationOrJavaEnum || childrenList.forall { case (_, child) =>
        SingletonValue.unapply(child.Underlying).isDefined
      }

      enumm
        .parMatchOn[MIO, Node](ectx.value) { matched =>
          import matched.{value as enumCaseValue, Underlying as EnumCase}
          Log.namedScope(s"Encoding enum case ${enumCaseValue.prettyPrint}: ${EnumCase.prettyPrint}") {
            val caseNodeMIO: MIO[Expr[Node]] =
              if (isEnumerationOrJavaEnum) MIO.pure(Expr.quote(YamlDerivationUtils.nodeFromFields(Nil): Node))
              else deriveEncoderRecursively[EnumCase](using ectx.nest(enumCaseValue))
            caseNodeMIO.map { caseNode =>
              val caseName: String = childrenList
                .find { case (_, child) =>
                  import child.Underlying as ChildType
                  Type[EnumCase] <:< Type[ChildType]
                }
                .map(_._1)
                .getOrElse(Type[EnumCase].shortName)
              if (allCaseObjects) {
                Expr.quote {
                  val config = Expr.splice(ectx.config)
                  val name = config.transformConstructorNames(Expr.splice(Expr(caseName)))
                  if (config.enumAsStrings) {
                    YamlDerivationUtils.encodeEnumAsString(name)
                  } else {
                    val node = Expr.splice(caseNode)
                    config.discriminator match {
                      case Some(discriminatorField) =>
                        YamlDerivationUtils.addDiscriminator(discriminatorField, name, node)
                      case None =>
                        if (config.wrappedSubtypes || Expr.splice(Expr(alwaysWrapped)))
                          YamlDerivationUtils.wrapWithTypeName(name, node)
                        // scala-yaml's untagged encoding (a case object is `{}`); Java enum / Enumeration values,
                        // which scala-yaml does not support, are written as their names
                        else if (Expr.splice(Expr(isEnumerationOrJavaEnum)))
                          YamlDerivationUtils.encodeEnumAsString(name)
                        else node
                    }
                  }
                }
              } else {
                Expr.quote {
                  val name = Expr.splice(ectx.config).transformConstructorNames(Expr.splice(Expr(caseName)))
                  val node = Expr.splice(caseNode)
                  Expr.splice(ectx.config).discriminator match {
                    case Some(discriminatorField) =>
                      YamlDerivationUtils.addDiscriminator(discriminatorField, name, node)
                    case None =>
                      // scala-yaml's untagged encoding unless subtypes are wrapped
                      if (Expr.splice(ectx.config).wrappedSubtypes || Expr.splice(Expr(alwaysWrapped)))
                        YamlDerivationUtils.wrapWithTypeName(name, node)
                      else node
                  }
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
