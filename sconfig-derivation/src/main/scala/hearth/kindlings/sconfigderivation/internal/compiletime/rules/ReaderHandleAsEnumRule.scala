package hearth.kindlings.sconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.sconfigderivation.{ConfigDecodingError, ConfigReader, CoproductHint}
import hearth.kindlings.sconfigderivation.internal.runtime.SConfigDerivationUtils
import org.ekrich.config.{ConfigValue, ConfigValueType}

trait ReaderHandleAsEnumRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object ReaderHandleAsEnumRule extends ReaderDerivationRule("handle as enum when possible") {

    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            decodeEnumCases[A](enumm).map(Rule.matched)
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeEnumCases[A: ReaderCtx](
        enumm: Enum[A]
    ): MIO[Expr[Either[ConfigDecodingError, A]]] = {
      implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
      implicit val ErrorT: Type[ConfigDecodingError] = RTypes.ConfigDecodingError
      implicit val StringT: Type[String] = RTypes.String
      implicit val ListStringT: Type[List[String]] = RTypes.ListString
      implicit val OptionStringT: Type[Option[String]] = RTypes.OptionString
      implicit val FuncT: Type[String => String] = RTypes.StringToString
      implicit val EitherT: Type[Either[ConfigDecodingError, A]] = RTypes.ReaderResult[A]
      implicit val FieldHintT: Type[CoproductHint.Field[A]] = RTypes.fieldCoproductHintType[A]
      implicit val WrappedHintT: Type[CoproductHint.Wrapped[A]] = RTypes.wrappedCoproductHintType[A]

      val maybeFieldHint: Option[Expr[CoproductHint.Field[A]]] =
        Expr.summonImplicit[CoproductHint.Field[A]].toOption
      val maybeWrappedHint: Option[Expr[CoproductHint.Wrapped[A]]] =
        Expr.summonImplicit[CoproductHint.Wrapped[A]].toOption

      val transformConstructorNamesExpr: Expr[String => String] = maybeFieldHint match {
        case Some(h) => Expr.quote(Expr.splice(h).transformConstructorNames)
        case None    =>
          maybeWrappedHint match {
            case Some(h) => Expr.quote(Expr.splice(h).transformConstructorNames)
            case None    => Expr.quote(Expr.splice(rctx.config).transformConstructorNames)
          }
      }

      val discrFieldExpr: Expr[Option[String]] = maybeFieldHint match {
        case Some(h) => Expr.quote((Some(Expr.splice(h).fieldName): Option[String]))
        case None    =>
          maybeWrappedHint match {
            case Some(_) => Expr.quote(None: Option[String])
            case None    => Expr.quote(Expr.splice(rctx.config).discriminator)
          }
      }

      val childrenList = enumm.directChildren.toList
      val allCaseObjects = Type[A].isEnumeration || Type[A].isJavaEnum ||
        childrenList.forall { case (_, child) =>
          SingletonValue.unapply(child.Underlying).isDefined
        }

      NonEmptyList.fromList(childrenList) match {
        case None =>
          MIO.pure(Expr.quote {
            Left(
              SConfigDerivationUtils.failedToMatchSubtype(
                Expr.splice(Expr(Type[A].prettyPrint)),
                List.empty
              )
            ): Either[ConfigDecodingError, A]
          })

        case Some(children) =>
          val knownNames: List[String] = children.toList.map(_._1)

          children
            .parTraverse { case (childName, child) =>
              import child.Underlying as ChildType
              Log.namedScope(s"Deriving reader for enum case $childName: ${Type[ChildType].prettyPrint}") {
                deriveChildReader[A, ChildType](childName, transformConstructorNamesExpr)
              }
            }
            .map { childDispatchers =>
              val errorExpr: Expr[String] => Expr[Either[ConfigDecodingError, A]] = typeNameExpr =>
                Expr.quote {
                  Left(
                    SConfigDerivationUtils.failedToMatchSubtype(
                      Expr.splice(typeNameExpr),
                      Expr.splice(Expr(knownNames))
                    )
                  ): Either[ConfigDecodingError, A]
                }

              val dispatchTree: (Expr[String], Expr[ConfigValue]) => Expr[Either[ConfigDecodingError, A]] =
                (typeNameExpr, innerValueExpr) =>
                  childDispatchers.toList.foldRight(errorExpr(typeNameExpr)) { case (dispatcher, elseExpr) =>
                    dispatcher(typeNameExpr, innerValueExpr, elseExpr)
                  }

              if (allCaseObjects) {
                Expr.quote {
                  val v = Expr.splice(rctx.value)
                  if (v.valueType == ConfigValueType.STRING) {
                    val typeName = v.unwrapped.asInstanceOf[String]
                    val _ = typeName
                    Expr.splice(dispatchTree(Expr.quote(typeName), Expr.quote(v)))
                  } else {
                    Expr.splice(discrFieldExpr) match {
                      case Some(field) =>
                        SConfigDerivationUtils.asObject(v).flatMap { obj =>
                          SConfigDerivationUtils.readDiscriminator(obj, field).flatMap { typeName =>
                            Expr.splice(dispatchTree(Expr.quote(typeName), Expr.quote(v)))
                          }
                        }
                      case None =>
                        SConfigDerivationUtils.asObject(v).flatMap { obj =>
                          SConfigDerivationUtils.readWrapped(obj).flatMap { case (typeName, inner) =>
                            Expr.splice(dispatchTree(Expr.quote(typeName), Expr.quote(inner)))
                          }
                        }
                    }
                  }
                }
              } else {
                Expr.quote {
                  val v = Expr.splice(rctx.value)
                  SConfigDerivationUtils.asObject(v).flatMap { obj =>
                    Expr.splice(discrFieldExpr) match {
                      case Some(field) =>
                        SConfigDerivationUtils.readDiscriminator(obj, field).flatMap { typeName =>
                          Expr.splice(dispatchTree(Expr.quote(typeName), Expr.quote(v)))
                        }
                      case None =>
                        SConfigDerivationUtils.readWrapped(obj).flatMap { case (typeName, inner) =>
                          Expr.splice(dispatchTree(Expr.quote(typeName), Expr.quote(inner)))
                        }
                    }
                  }
                }
              }
            }
      }
    }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveChildReader[A: ReaderCtx, ChildType: Type](
        childName: String,
        transformConstructorNamesExpr: Expr[String => String]
    ): MIO[
      (Expr[String], Expr[ConfigValue], Expr[Either[ConfigDecodingError, A]]) => Expr[
        Either[ConfigDecodingError, A]
      ]
    ] = {
      implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
      implicit val ErrorT: Type[ConfigDecodingError] = RTypes.ConfigDecodingError
      implicit val StringT: Type[String] = RTypes.String
      implicit val FuncT: Type[String => String] = RTypes.StringToString
      implicit val ReaderResultA: Type[Either[ConfigDecodingError, A]] = RTypes.ReaderResult[A]
      implicit val ReaderResultChild: Type[Either[ConfigDecodingError, ChildType]] = RTypes.ReaderResult[ChildType]
      implicit val ReaderChild: Type[ConfigReader[ChildType]] = RTypes.ConfigReader[ChildType]

      RTypes
        .ConfigReader[ChildType]
        .summonExprIgnoring(ReaderUseImplicitWhenAvailableRule.ignoredImplicits*)
        .toEither match {
        case Right(readerExpr) =>
          Log.info(s"Found implicit ConfigReader[$childName], using it") >>
            MIO.pure {
              (
                  typeNameExpr: Expr[String],
                  innerValueExpr: Expr[ConfigValue],
                  elseExpr: Expr[Either[ConfigDecodingError, A]]
              ) =>
                Expr.quote {
                  if (
                    Expr.splice(transformConstructorNamesExpr)(Expr.splice(Expr(childName))) ==
                      Expr.splice(typeNameExpr)
                  )
                    Expr
                      .splice(readerExpr)
                      .from(Expr.splice(innerValueExpr))
                      .asInstanceOf[Either[ConfigDecodingError, A]]
                  else
                    Expr.splice(elseExpr)
                }
            }

        case Left(_) =>
          Expr.singletonOf[ChildType] match {
            case Some(singleton) =>
              Log.info(s"Using singleton for $childName") >>
                MIO.pure {
                  (
                      typeNameExpr: Expr[String],
                      _: Expr[ConfigValue],
                      elseExpr: Expr[Either[ConfigDecodingError, A]]
                  ) =>
                    Expr.quote {
                      if (
                        Expr.splice(transformConstructorNamesExpr)(Expr.splice(Expr(childName))) ==
                          Expr.splice(typeNameExpr)
                      )
                        Right(Expr.splice(singleton).asInstanceOf[A]): Either[ConfigDecodingError, A]
                      else
                        Expr.splice(elseExpr)
                    }
                }
            case None =>
              deriveReaderRecursively[ChildType](using rctx.nest[ChildType](rctx.value)).flatMap { _ =>
                rctx.getHelper[ChildType].map {
                  case Some(helper) =>
                    (
                        typeNameExpr: Expr[String],
                        innerValueExpr: Expr[ConfigValue],
                        elseExpr: Expr[Either[ConfigDecodingError, A]]
                    ) => {
                      val helperCallExpr = helper(innerValueExpr, rctx.config)
                      Expr.quote {
                        if (
                          Expr.splice(transformConstructorNamesExpr)(Expr.splice(Expr(childName))) ==
                            Expr.splice(typeNameExpr)
                        )
                          Expr.splice(helperCallExpr).asInstanceOf[Either[ConfigDecodingError, A]]
                        else
                          Expr.splice(elseExpr)
                      }
                    }

                  case None =>
                    (
                        _: Expr[String],
                        _: Expr[ConfigValue],
                        elseExpr: Expr[Either[ConfigDecodingError, A]]
                    ) => elseExpr
                }
              }
          }
      }
    }
  }
}
