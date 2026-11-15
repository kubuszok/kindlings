package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils
import org.virtuslab.yaml.{ConstructError, Node}

trait DecoderHandleAsEnumRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsEnumRule extends DecoderDerivationRule("handle as enum when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            decodeEnumCases[A](enumm).map(Rule.matched)
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeEnumCases[A: DecoderCtx](
        enumm: Enum[A]
    ): MIO[Expr[Either[ConstructError, A]]] = {
      implicit val NodeT: Type[Node] = DTypes.Node
      implicit val ConstructErrorT: Type[ConstructError] = DTypes.ConstructError
      implicit val StringT: Type[String] = DTypes.String
      implicit val ListStringT: Type[List[String]] = DTypes.ListString
      implicit val TupleT: Type[(String, Node)] = DTypes.StringNodeTuple
      implicit val EitherCEA: Type[Either[ConstructError, A]] = DTypes.DecoderResult[A]

      val childrenList = enumm.directChildren.toList

      val allCaseObjects = Type[A].isEnumeration || Type[A].isJavaEnum ||
        childrenList.forall { case (_, child) =>
          SingletonValue.unapply(child.Underlying).isDefined
        }

      NonEmptyList.fromList(childrenList) match {
        case None =>
          MIO.pure(Expr.quote {
            Left(
              ConstructError.from(
                s"Enum ${Expr.splice(Expr(Type[A].prettyPrint))} has no subtypes"
              )
            ): Either[ConstructError, A]
          })

        case Some(children) =>
          val knownNames: List[String] = children.toList.map(_._1)

          children
            .parTraverse { case (childName, child) =>
              import child.Underlying as ChildType
              Log.namedScope(s"Deriving decoder for enum case $childName: ${Type[ChildType].prettyPrint}") {
                deriveChildDecoder[A, ChildType](childName)
              }
            }
            .flatMap { childDecoders =>
              val childDispatchers = childDecoders.toList.map(_.dispatch)
              val isValueEnum = Type[A].isEnumeration || Type[A].isJavaEnum
              // Union types have no scala-yaml counterpart: their members stay wrapped
              val alwaysWrapped: Boolean = Type[A].isUnionType
              // scala-yaml's sum decoder: every subtype is attempted on the node, in order; the first success wins
              def untagged(nodeExpr: Expr[Node]): Expr[Either[ConstructError, A]] =
                childDecoders.toList.foldRight(Expr.quote {
                  Left(YamlDerivationUtils.cannotParse(Expr.splice(nodeExpr))): Either[ConstructError, A]
                }) { (child, elseExpr) =>
                  Expr.quote(Expr.splice(child.untagged(nodeExpr)).orElse(Expr.splice(elseExpr)))
                }
              LambdaBuilder
                .of1[(String, Node)]("readResult")
                .traverse { readResultExpr =>
                  val typeNameExpr: Expr[String] = Expr.quote(Expr.splice(readResultExpr)._1)
                  val innerNodeExpr: Expr[Node] = Expr.quote(Expr.splice(readResultExpr)._2)

                  val errorExpr: Expr[Either[ConstructError, A]] = Expr.quote {
                    Left(
                      YamlDerivationUtils.failedToMatchSubtype(
                        Expr.splice(typeNameExpr),
                        Expr.splice(innerNodeExpr),
                        Expr.splice(Expr(knownNames))
                      )
                    ): Either[ConstructError, A]
                  }

                  MIO.pure(childDispatchers.foldRight(errorExpr) { case (dispatcher, elseExpr) =>
                    dispatcher(typeNameExpr, innerNodeExpr, elseExpr)
                  })
                }
                .map { builder =>
                  val dispatchFn = builder.build[Either[ConstructError, A]]
                  Expr.quote {
                    val config = Expr.splice(dctx.config)
                    val node = Expr.splice(dctx.node)
                    if (Expr.splice(Expr(allCaseObjects)) && config.enumAsStrings) {
                      YamlDerivationUtils.decodeEnumFromString[A](node, Expr.splice(Expr(knownNames))) { typeName =>
                        Expr.splice(dispatchFn)((typeName, node))
                      }
                    } else {
                      config.discriminator match {
                        case Some(field) =>
                          YamlDerivationUtils.decodeDiscriminator(node, field).flatMap(Expr.splice(dispatchFn))
                        case None =>
                          if (config.wrappedSubtypes || Expr.splice(Expr(alwaysWrapped)))
                            YamlDerivationUtils.decodeWrapped(node).flatMap(Expr.splice(dispatchFn))
                          // Java enum / Enumeration values (not supported by scala-yaml) are their names
                          else if (Expr.splice(Expr(isValueEnum)))
                            YamlDerivationUtils.decodeEnumFromString[A](node, Expr.splice(Expr(knownNames))) {
                              typeName => Expr.splice(dispatchFn)((typeName, node))
                            }
                          else Expr.splice(untagged(Expr.quote(node)))
                      }
                    }
                  }
                }
            }
      }
    }

    /** How to decode one subtype: `dispatch` decodes it when the type name matches (else the `else` branch), `untagged`
      * decodes it from the node as scala-yaml's sum decoder attempts it.
      */
    final private case class ChildDecoder[A](
        dispatch: (Expr[String], Expr[Node], Expr[Either[ConstructError, A]]) => Expr[Either[ConstructError, A]],
        untagged: Expr[Node] => Expr[Either[ConstructError, A]]
    )

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveChildDecoder[A: DecoderCtx, ChildType: Type](
        childName: String
    ): MIO[ChildDecoder[A]] = {
      implicit val NodeT: Type[Node] = DTypes.Node
      implicit val ConstructErrorT: Type[ConstructError] = DTypes.ConstructError
      implicit val StringT: Type[String] = DTypes.String

      DTypes
        .YamlDecoder[ChildType]
        .summonExprIgnoring(DecoderUseImplicitWhenAvailableRule.ignoredImplicits*)
        .toEither match {
        case Right(decoderExpr) =>
          Log.info(s"Found implicit YamlDecoder[$childName], using it") >>
            MIO.pure {
              ChildDecoder[A](
                (
                    typeNameExpr: Expr[String],
                    innerNodeExpr: Expr[Node],
                    elseExpr: Expr[Either[ConstructError, A]]
                ) =>
                  Expr.quote {
                    if (
                      Expr.splice(dctx.config).transformConstructorNames(Expr.splice(Expr(childName))) == Expr
                        .splice(typeNameExpr)
                    )
                      Expr
                        .splice(decoderExpr)
                        .construct(Expr.splice(innerNodeExpr))(org.virtuslab.yaml.LoadSettings.empty)
                        .asInstanceOf[Either[ConstructError, A]]
                    else
                      Expr.splice(elseExpr)
                  },
                (nodeExpr: Expr[Node]) =>
                  Expr.quote {
                    Expr
                      .splice(decoderExpr)
                      .construct(Expr.splice(nodeExpr))(org.virtuslab.yaml.LoadSettings.empty)
                      .asInstanceOf[Either[ConstructError, A]]
                  }
              )
            }

        case Left(_) =>
          Expr.singletonOf[ChildType] match {
            case Some(singleton) =>
              Log.info(s"Using singleton for $childName") >>
                MIO.pure {
                  ChildDecoder[A](
                    (
                        typeNameExpr: Expr[String],
                        _: Expr[Node],
                        elseExpr: Expr[Either[ConstructError, A]]
                    ) =>
                      Expr.quote {
                        if (
                          Expr.splice(dctx.config).transformConstructorNames(Expr.splice(Expr(childName))) == Expr
                            .splice(typeNameExpr)
                        )
                          Right(Expr.splice(singleton).asInstanceOf[A]): Either[ConstructError, A]
                        else
                          Expr.splice(elseExpr)
                      },
                    // as scala-yaml's decoder of a product without fields: any mapping
                    (nodeExpr: Expr[Node]) =>
                      Expr.quote {
                        YamlDerivationUtils
                          .decodeAsEmptyProduct(Expr.splice(nodeExpr), Expr.splice(singleton).asInstanceOf[A])
                      }
                  )
                }
            case None =>
              deriveDecoderRecursively[ChildType](using dctx.nest[ChildType](dctx.node)).flatMap { _ =>
                dctx.getHelper[ChildType].flatMap {
                  case Some(helper) =>
                    MIO.pure(
                      ChildDecoder[A](
                        (
                            typeNameExpr: Expr[String],
                            innerNodeExpr: Expr[Node],
                            elseExpr: Expr[Either[ConstructError, A]]
                        ) => {
                          val helperCallExpr = helper(innerNodeExpr, dctx.config)
                          Expr.quote {
                            if (
                              Expr.splice(dctx.config).transformConstructorNames(Expr.splice(Expr(childName))) == Expr
                                .splice(typeNameExpr)
                            )
                              Expr.splice(helperCallExpr).asInstanceOf[Either[ConstructError, A]]
                            else
                              Expr.splice(elseExpr)
                          }
                        },
                        (nodeExpr: Expr[Node]) => {
                          val helperCallExpr = helper(nodeExpr, dctx.config)
                          Expr.quote(Expr.splice(helperCallExpr).asInstanceOf[Either[ConstructError, A]])
                        }
                      )
                    )

                  // unreachable: deriveDecoderRecursively has just cached the subtype's decoder
                  case None => MIO.fail(DecoderDerivationError.CannotConstructType(Type[ChildType].prettyPrint, false))
                }
              }
          }
      }
    }
  }
}
