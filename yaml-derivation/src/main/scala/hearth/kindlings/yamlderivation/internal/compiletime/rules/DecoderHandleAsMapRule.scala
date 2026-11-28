package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.derivation.compiletime.StableChildrenOrder
import hearth.kindlings.yamlderivation.YamlKeyDecoder
import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils
import org.virtuslab.yaml.{ConstructError, LoadSettings, Node, YamlDecoder}

trait DecoderHandleAsMapRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  @scala.annotation.nowarn("msg=Infinite loop")
  object DecoderHandleAsMapRule extends DecoderDerivationRule("handle as map when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a map") >> {
        Type[A] match {
          case IsMap(isMap) =>
            import isMap.Underlying as Pair
            decodeMapEntries[A, Pair](isMap.value)
          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a map"))
        }
      }

    /** The built-in key decoders of `YamlKeyDecoder`'s companion: not user-provided (the macro reads these types
      * itself).
      */
    private lazy val builtInKeyDecoders: Seq[UntypedMethod] =
      Type.of[YamlKeyDecoder.type].unsortedMethods.collect {
        case method if method.isImplicit => method.asUntyped
      }

    /** A user-provided `YamlKeyDecoder[K]` in scope (not one of the built-in ones). */
    private def userKeyDecoder[K: Type]: Option[Expr[YamlKeyDecoder[K]]] =
      DTypes.YamlKeyDecoder[K].summonExprIgnoring(builtInKeyDecoders*).toOption

    /** The decoding of a map key: with its key decoder (user-provided, built-in or derived for enums of case objects
      * and value types); when it has none, a complex key, decoded from its node with its `YamlDecoder` (user-provided
      * or derived), as scala-yaml decodes every key.
      */
    private def deriveKeyDecoder[K: Type](
        keyNode: Expr[Node]
    )(implicit ctx: DecoderCtx[?]): MIO[Expr[Either[ConstructError, K]]] =
      deriveScalarKeyDecoder[K](keyNode).flatMap {
        case Some(decoded) => MIO.pure(decoded)
        case None          =>
          Log.info(s"No key decoder for ${Type[K].prettyPrint}, decoding it as a complex key") >>
            deriveDecoderRecursively[K](using ctx.nest[K](keyNode)).handleErrorWith { errors =>
              MIO.fail(DecoderDerivationError.NoKeyDecoder(Type[K].prettyPrint, errors.toList.map(_.getMessage)))
            }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveScalarKeyDecoder[K: Type](
        keyNode: Expr[Node]
    )(implicit ctx: DecoderCtx[?]): MIO[Option[Expr[Either[ConstructError, K]]]] = {
      implicit val StringT: Type[String] = DTypes.String
      implicit val NodeT: Type[Node] = DTypes.Node
      implicit val EitherCEK: Type[Either[ConstructError, K]] = DTypes.DecoderResult[K]
      implicit val KeyDecoderK: Type[YamlKeyDecoder[K]] = DTypes.YamlKeyDecoder[K]
      val className: Expr[String] = Expr(runtimeClassName[K])
      def withCodec(codec: Expr[YamlKeyDecoder[K]]): Expr[Either[ConstructError, K]] =
        Expr.quote(
          YamlDerivationUtils.decodeKeyWithCodec(Expr.splice(keyNode), Expr.splice(className), Expr.splice(codec))
        )
      // as scala-yaml's decoder of the type reads a scalar node (failures included)
      def withScalarDecoder[B](decoder: Expr[YamlDecoder[B]]): Option[Expr[Either[ConstructError, K]]] = {
        val decoderK = decoder.asInstanceOf[Expr[YamlDecoder[K]]]
        Some(Expr.quote(Expr.splice(decoderK).construct(Expr.splice(keyNode))(LoadSettings.empty)))
      }

      userKeyDecoder[K] match {
        case Some(codec) =>
          Log.info(s"Using the user-provided YamlKeyDecoder[${Type[K].prettyPrint}]") >>
            MIO.pure(Some(withCodec(codec)))
        case None =>
          if (Type[K] =:= Type[String])
            MIO.pure(
              Some(
                Expr
                  .quote(YamlDerivationUtils.keyScalar(Expr.splice(keyNode), "java.lang.String"))
                  .asInstanceOf[Expr[Either[ConstructError, K]]]
              )
            )
          else if (Type[K] =:= DTypes.Int) MIO.pure(withScalarDecoder(Expr.quote(YamlDecoder.forInt)))
          else if (Type[K] =:= DTypes.Long) MIO.pure(withScalarDecoder(Expr.quote(YamlDecoder.forLong)))
          else if (Type[K] =:= DTypes.Short) MIO.pure(withScalarDecoder(Expr.quote(YamlDecoder.forShort)))
          else if (Type[K] =:= DTypes.Byte) MIO.pure(withScalarDecoder(Expr.quote(YamlDecoder.forByte)))
          else if (Type[K] =:= DTypes.Double) MIO.pure(withScalarDecoder(Expr.quote(YamlDecoder.forDouble)))
          else if (Type[K] =:= DTypes.Float) MIO.pure(withScalarDecoder(Expr.quote(YamlDecoder.forFloat)))
          else if (Type[K] =:= DTypes.Boolean) MIO.pure(withScalarDecoder(Expr.quote(YamlDecoder.forBoolean)))
          else if (Type[K] =:= DTypes.BigInt) MIO.pure(withScalarDecoder(Expr.quote(YamlDecoder.forBigInt)))
          else if (Type[K] =:= DTypes.BigDecimal) MIO.pure(withScalarDecoder(Expr.quote(YamlDecoder.forBigDecimal)))
          else if (Type[K] =:= DTypes.Char)
            MIO.pure(Some(withCodec(Expr.quote(YamlKeyDecoder.forChar).asInstanceOf[Expr[YamlKeyDecoder[K]]])))
          else if (Type[K] =:= DTypes.UUID)
            MIO.pure(Some(withCodec(Expr.quote(YamlKeyDecoder.forUUID).asInstanceOf[Expr[YamlKeyDecoder[K]]])))
          else
            Type[K] match {
              case IsValueType(isValueType) if !Type[K].isNamedTuple =>
                import isValueType.Underlying as Inner
                deriveScalarKeyDecoder[Inner](keyNode).flatMap {
                  case Some(innerDecoded) =>
                    DecoderHandleAsValueTypeRule
                      .buildWrapLambda[K, Inner](isValueType.value)(Type[K], Type[Inner], ctx.nest[K](keyNode))
                      .map { builder =>
                        val wrap = builder.build[Either[ConstructError, K]]
                        Some(Expr.quote(Expr.splice(innerDecoded).flatMap(Expr.splice(wrap))))
                      }
                  case None => MIO.pure(None)
                }
              case _ =>
                Enum.parse[K].toOption match {
                  case Some(enumm) => deriveEnumKeyDecoder[K](enumm, keyNode, className)
                  case None        => MIO.pure(None)
                }
            }
      }
    }

    /** An enum of case objects (Scala 3 enum, Java enum, `scala.Enumeration`, sealed trait of case objects) read from
      * the (configured) name of the case.
      */
    @scala.annotation.nowarn("msg=is never used")
    private def deriveEnumKeyDecoder[K: Type](enumm: Enum[K], keyNode: Expr[Node], className: Expr[String])(implicit
        ctx: DecoderCtx[?]
    ): MIO[Option[Expr[Either[ConstructError, K]]]] = {
      implicit val StringT: Type[String] = DTypes.String
      implicit val NodeT: Type[Node] = DTypes.Node
      implicit val ListStringT: Type[List[String]] = DTypes.ListString
      implicit val EitherCEK: Type[Either[ConstructError, K]] = DTypes.DecoderResult[K]
      val children = StableChildrenOrder.directChildren(DecoderHandleAsMapRuleImpl.this)(enumm)
      val singletons = children.map { case (name, child) =>
        import child.Underlying as Child
        Expr.singletonOf[Child].map(value => name -> value.asInstanceOf[Expr[K]])
      }
      if (children.isEmpty || singletons.exists(_.isEmpty)) MIO.pure(None)
      else {
        val knownNames = children.map(_._1)
        // (sorted: the order of the children is not the same on every Scala version)
        val namesInMessage =
          ctx.evaluatedConfig.fold(knownNames)(config => knownNames.map(config.transformConstructorNames)).sorted
        val namesMIO: MIO[String => Expr[String]] = ctx.evaluatedConfig match {
          case Some(config) => MIO.pure((name: String) => Expr(config.transformConstructorNames(name)))
          case None         => ctx.transformedConstructorNames(knownNames)
        }
        namesMIO.map { nameOf =>
          def dispatch(name: Expr[String]): Expr[Either[ConstructError, K]] =
            singletons.flatten.foldRight(Expr.quote {
              Left(
                YamlDerivationUtils
                  .unknownEnumKey(Expr.splice(name), Expr.splice(keyNode), Expr.splice(Expr(namesInMessage)))
              ): Either[ConstructError, K]
            }) { case ((caseName, value), elseExpr) =>
              Expr.quote {
                if (Expr.splice(nameOf(caseName)) == Expr.splice(name))
                  Right(Expr.splice(value)): Either[ConstructError, K]
                else Expr.splice(elseExpr)
              }
            }
          Some(Expr.quote {
            YamlDerivationUtils.keyScalar(Expr.splice(keyNode), Expr.splice(className)) match {
              case Right(name) => Expr.splice(dispatch(Expr.quote(name)))
              case Left(error) => Left(error): Either[ConstructError, K]
            }
          })
        }
      }
    }

    // Exposed so the combined collection-or-map rule can call it after a single IsCollection parse.
    @scala.annotation.nowarn("msg=is never used")
    private[rules] def decodeMapEntries[A: DecoderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] = {
      import isMap.{Key, Value, CtorResult}
      implicit val StringT: Type[String] = DTypes.String
      implicit val NodeT: Type[Node] = DTypes.Node
      implicit val EitherCEValue: Type[Either[ConstructError, Value]] = DTypes.DecoderResult[Value]
      implicit val EitherCEA: Type[Either[ConstructError, A]] = DTypes.DecoderResult[A]

      implicit val EitherCEKey: Type[Either[ConstructError, Key]] = DTypes.DecoderResult[Key]
      implicit val KeyFnT: Type[Node => Either[ConstructError, Key]] = Type.of[Node => Either[ConstructError, Key]]

      // Keys are read with their key decoder (`YamlKeyDecoder`: user-provided, built-in or derived), or, for a complex
      // key type without one, with their `YamlDecoder`. Only keys of exactly `String` (not its subtypes: opaque types,
      // Iron types, ... keep their own decoding and validation) without a user-provided `YamlKeyDecoder[String]` take
      // the fast path below (the built-in `String` key decoder: a scalar that is not a null).
      val keyDecodeFnMIO: MIO[Option[Expr[Node => Either[ConstructError, Key]]]] =
        if (Key =:= Type[String] && userKeyDecoder[String].isEmpty) MIO.pure(None)
        else
          LambdaBuilder
            .of1[Node]("keyNode")
            .traverse(keyNodeExpr => deriveKeyDecoder[Key](keyNodeExpr))
            .map(builder => Some(builder.build[Either[ConstructError, Key]]))

      keyDecodeFnMIO.flatMap { keyDecodeFn =>
        LambdaBuilder
          .of1[Node]("valueNode")
          .traverse { valueNodeExpr =>
            deriveDecoderRecursively[Value](using dctx.nest[Value](valueNodeExpr))
          }
          .map { builder =>
            val decodeFn = builder.build[Either[ConstructError, Value]]
            val factoryExpr = isMap.factory
            val buildStep = isMap.build
            val classNameExpr: Expr[String] = Expr(runtimeClassName[A])

            def readLoopWith(
                keyOf: Expr[Node] => Expr[Key]
            ): Expr[scala.collection.mutable.Builder[Pair, CtorResult]] = Expr.quote {
              val mapBuilder = Expr.splice(factoryExpr).newBuilder
              val decoder = YamlDerivationUtils.decoderFromFn(Expr.splice(decodeFn))
              // anything but a mapping (a null included) fails as scala-yaml's `YamlDecoder.forMap`
              val iter = YamlDerivationUtils
                .mappingEntries(Expr.splice(dctx.node), Expr.splice(classNameExpr))
                .iterator
              while (iter.hasNext) {
                val entry = iter.next()
                val keyNode = entry._1
                val valueNode = entry._2
                val key = Expr.splice(keyOf(Expr.quote(keyNode)))
                decoder.construct(valueNode)(LoadSettings.empty) match {
                  case Right(v) =>
                    mapBuilder += Expr.splice(isMap.pair(Expr.quote(key), Expr.quote(v)))
                  case Left(err) =>
                    throw new YamlDerivationUtils.CollectionBuildException(err)
                }
              }
              mapBuilder
            }
            val readLoop: Expr[scala.collection.mutable.Builder[Pair, CtorResult]] = keyDecodeFn match {
              case None =>
                readLoopWith(keyNode =>
                  Expr.quote(YamlDerivationUtils.stringKey(Expr.splice(keyNode)).asInstanceOf[Key])
                )
              case Some(fn) =>
                ValDefs
                  .createVal[Node => Either[ConstructError, Key]](fn, FreshName.FromPrefix("decodeKey"))
                  .use { decodeKey =>
                    readLoopWith(keyNode =>
                      Expr.quote(YamlDerivationUtils.decodeKeyWith(Expr.splice(keyNode), Expr.splice(decodeKey)))
                    )
                  }
            }
            val buildResultExpr = buildStep.ctor(readLoop)

            buildStep match {
              case _: CtorLikeOf.PlainValue[?, ?] =>
                Rule.matched(Expr.quote {
                  try Right(Expr.splice(buildResultExpr.asInstanceOf[Expr[A]]))
                  catch {
                    case e: YamlDerivationUtils.CollectionBuildException => Left(e.error)
                  }
                })

              case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
                val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[String, A]]]
                Rule.matched(Expr.quote {
                  try
                    Expr.splice(eitherExpr) match {
                      case Right(value) => Right(value)
                      case Left(err)    => Left(ConstructError.from(err, Expr.splice(dctx.node)))
                    }
                  catch {
                    case e: YamlDerivationUtils.CollectionBuildException => Left(e.error)
                  }
                })

              case _: CtorLikeOf.EitherIterableStringOrValue[?, ?] =>
                val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[String], A]]]
                Rule.matched(Expr.quote {
                  try
                    Expr.splice(eitherExpr) match {
                      case Right(value) => Right(value)
                      case Left(errs)   =>
                        Left(ConstructError.from(errs.mkString("\n"), Expr.splice(dctx.node)))
                    }
                  catch {
                    case e: YamlDerivationUtils.CollectionBuildException => Left(e.error)
                  }
                })

              case _: CtorLikeOf.EitherThrowableOrValue[?, ?] =>
                val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Throwable, A]]]
                Rule.matched(Expr.quote {
                  try
                    Expr.splice(eitherExpr) match {
                      case Right(value) => Right(value)
                      case Left(err)    =>
                        Left(ConstructError.from(err.getMessage, Expr.splice(dctx.node)))
                    }
                  catch {
                    case e: YamlDerivationUtils.CollectionBuildException => Left(e.error)
                  }
                })

              case _: CtorLikeOf.EitherIterableThrowableOrValue[?, ?] =>
                val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[Throwable], A]]]
                Rule.matched(Expr.quote {
                  try
                    Expr.splice(eitherExpr) match {
                      case Right(value) => Right(value)
                      case Left(errs)   =>
                        Left(
                          ConstructError.from(errs.map(_.getMessage).mkString("\n"), Expr.splice(dctx.node))
                        )
                    }
                  catch {
                    case e: YamlDerivationUtils.CollectionBuildException => Left(e.error)
                  }
                })
            }
          }
      }
    }
  }
}
