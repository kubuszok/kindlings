package hearth.kindlings.xmlderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.xmlderivation.{KindlingsXmlEncoder, XmlConfig, XmlFieldMode}
import hearth.kindlings.xmlderivation.annotations.{
  transientField,
  xmlAttribute,
  xmlContent,
  xmlElement,
  xmlName,
  xmlWrapper
}
import hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils

trait EncoderMacrosImpl { this: MacroCommons & StdExtensions & AnnotationSupport =>

  // Entrypoints

  def deriveInlineEncode[A: Type](
      valueExpr: Expr[A],
      elementNameExpr: Expr[String],
      configExpr: Expr[XmlConfig]
  ): Expr[scala.xml.Elem] = {
    implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
    implicit val ConfigT: Type[XmlConfig] = Types.XmlConfig
    implicit val StringT: Type[String] = Types.String

    deriveEncoderFromCtxAndAdaptForEntrypoint[A, scala.xml.Elem]("KindlingsXmlEncoder.encode") { fromCtx =>
      ValDefs.createVal[A](valueExpr).use { valueVal =>
        ValDefs.createVal[XmlConfig](configExpr).use { configVal =>
          ValDefs.createVal[String](elementNameExpr).use { nameVal =>
            Expr.quote {
              val _ = Expr.splice(valueVal)
              val _ = Expr.splice(configVal)
              val _ = Expr.splice(nameVal)
              Expr.splice(fromCtx(EncoderCtx.from(valueVal, nameVal, configVal, derivedType = None)))
            }
          }
        }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  def deriveInlineToXmlString[A: Type](
      valueExpr: Expr[A],
      elementNameExpr: Expr[String],
      configExpr: Expr[XmlConfig]
  ): Expr[String] = {
    implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
    implicit val ConfigT: Type[XmlConfig] = Types.XmlConfig
    implicit val StringT: Type[String] = Types.String

    deriveEncoderFromCtxAndAdaptForEntrypoint[A, String]("KindlingsXmlEncoder.toXmlString") { fromCtx =>
      ValDefs.createVal[A](valueExpr).use { valueVal =>
        ValDefs.createVal[XmlConfig](configExpr).use { configVal =>
          ValDefs.createVal[String](elementNameExpr).use { nameVal =>
            Expr.quote {
              val _ = Expr.splice(valueVal)
              val _ = Expr.splice(configVal)
              val _ = Expr.splice(nameVal)
              XmlDerivationUtils.elemToString(
                Expr.splice(fromCtx(EncoderCtx.from(valueVal, nameVal, configVal, derivedType = None)))
              )
            }
          }
        }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  def deriveEncoderTypeClass[A: Type](configExpr: Expr[XmlConfig]): Expr[KindlingsXmlEncoder[A]] = {
    implicit val EncoderA: Type[XmlEncoder[A]] = Types.XmlEncoder[A]
    implicit val KindlingsEncoderA: Type[KindlingsXmlEncoder[A]] = Types.KindlingsXmlEncoder[A]
    implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
    implicit val ConfigT: Type[XmlConfig] = Types.XmlConfig
    implicit val StringT: Type[String] = Types.String
    val selfType: Option[??] = Some(Type[A].as_??)

    deriveEncoderFromCtxAndAdaptForEntrypoint[A, KindlingsXmlEncoder[A]]("KindlingsXmlEncoder.derived") { fromCtx =>
      ValDefs.createVal[XmlConfig](configExpr).use { configVal =>
        Expr.quote {
          val cfg = Expr.splice(configVal)
          new KindlingsXmlEncoder[A] {
            def encode(value: A, elementName: String): scala.xml.Elem = {
              val _ = value
              val _ = elementName
              val _ = cfg
              Expr.splice {
                fromCtx(EncoderCtx.from(Expr.quote(value), Expr.quote(elementName), Expr.quote(cfg), derivedType = selfType))
              }
            }
          }
        }
      }
    }
  }

  // Handles logging, error reporting and prepending "cached" defs and vals to the result.

  def deriveEncoderFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (EncoderCtx[A] => Expr[scala.xml.Elem]) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          s"Provide an explicit type parameter, e.g.: $macroName[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )
    Log
      .namedScope(
        s"Deriving XML encoder for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          val fromCtx: (EncoderCtx[A] => Expr[scala.xml.Elem]) = (ctx: EncoderCtx[A]) =>
            runSafe {
              for {
                _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                result <- deriveEncoderRecursively[A](using ctx)
                cache <- ctx.cache.get
              } yield cache.toValDefs.use(_ => result)
            }

          provideCtxAndAdapt(fromCtx)
        }
      }
      .flatTap { result =>
        Log.info(s"Derived final XML encoder result: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogEncoderDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogEncoderDerivation) RenderFrom(Log.Level.Info) else DontRender
      ) { (errorLogs, errors) =>
        val errorsRendered = errors
          .map { e =>
            e.getMessage.split("\n").toList match {
              case head :: tail => (("  - " + head) :: tail.map("    " + _)).mkString("\n")
              case _            => "  - " + e.getMessage
            }
          }
          .mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.xmlderivation.debug.logDerivationForKindlingsXmlEncoder or scalac option -Xmacro-settings:xmlDerivation.logDerivation=true"
        if (errorLogs.nonEmpty)
          s"""Macro derivation failed with the following errors:
             |$errorsRendered
             |and the following logs:
             |$errorLogs
             |$hint""".stripMargin
        else
          s"""Macro derivation failed with the following errors:
             |$errorsRendered
             |$hint""".stripMargin
      }
  }

  def shouldWeLogEncoderDerivation: Boolean = {
    implicit val LogDerivation: Type[KindlingsXmlEncoder.LogDerivation] = Types.EncoderLogDerivation
    def logDerivationImported = Expr.summonImplicit[KindlingsXmlEncoder.LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      xmlDerivation <- data.get("xmlDerivation")
      shouldLog <- xmlDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)

    logDerivationImported || logDerivationSetGlobally
  }

  // Context

  final case class EncoderCtx[A](
      tpe: Type[A],
      value: Expr[A],
      elementName: Expr[String],
      config: Expr[XmlConfig],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {

    def nest[B: Type](newValue: Expr[B]): EncoderCtx[B] = copy[B](
      tpe = Type[B],
      value = newValue
    )

    def nestInCache(
        newValue: Expr[A],
        newElementName: Expr[String],
        newConfig: Expr[XmlConfig]
    ): EncoderCtx[A] = copy(
      value = newValue,
      elementName = newElementName,
      config = newConfig
    )

    def getInstance[B: Type]: MIO[Option[Expr[XmlEncoder[B]]]] = {
      implicit val EncoderB: Type[XmlEncoder[B]] = Types.XmlEncoder[B]
      cache.get0Ary[XmlEncoder[B]]("cached-encoder-instance")
    }
    def setInstance[B: Type](instance: Expr[XmlEncoder[B]]): MIO[Unit] = {
      implicit val EncoderB: Type[XmlEncoder[B]] = Types.XmlEncoder[B]
      Log.info(s"Caching XmlEncoder instance for ${Type[B].prettyPrint}") >>
        cache.buildCachedWith(
          "cached-encoder-instance",
          ValDefBuilder.ofLazy[XmlEncoder[B]](s"encoder_${Type[B].shortName}")
        )(_ => instance)
    }

    def getHelper[B: Type]: MIO[Option[(Expr[B], Expr[String], Expr[XmlConfig]) => Expr[scala.xml.Elem]]] = {
      implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
      implicit val ConfigT: Type[XmlConfig] = Types.XmlConfig
      implicit val StringT: Type[String] = Types.String
      cache.get3Ary[B, String, XmlConfig, scala.xml.Elem]("cached-encode-method")
    }
    def setHelper[B: Type](
        helper: (Expr[B], Expr[String], Expr[XmlConfig]) => MIO[Expr[scala.xml.Elem]]
    ): MIO[Unit] = {
      implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
      implicit val ConfigT: Type[XmlConfig] = Types.XmlConfig
      implicit val StringT: Type[String] = Types.String
      val defBuilder =
        ValDefBuilder.ofDef3[B, String, XmlConfig, scala.xml.Elem](s"encode_${Type[B].shortName}")
      for {
        _ <- Log.info(s"Forward-declaring encode helper for ${Type[B].prettyPrint}")
        _ <- cache.forwardDeclare("cached-encode-method", defBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(cache.buildCachedWith("cached-encode-method", defBuilder) { case (_, (value, name, config)) =>
            runSafe(helper(value, name, config))
          })
        }
        _ <- Log.info(s"Defined encode helper for ${Type[B].prettyPrint}")
      } yield ()
    }

    override def toString: String =
      s"encode[${tpe.prettyPrint}](value = ${value.prettyPrint}, elementName = ${elementName.prettyPrint}, config = ${config.prettyPrint})"
  }
  object EncoderCtx {

    def from[A: Type](
        value: Expr[A],
        elementName: Expr[String],
        config: Expr[XmlConfig],
        derivedType: Option[??]
    ): EncoderCtx[A] = EncoderCtx(
      tpe = Type[A],
      value = value,
      elementName = elementName,
      config = config,
      cache = ValDefsCache.mlocal,
      derivedType = derivedType
    )
  }

  def ectx[A](implicit A: EncoderCtx[A]): EncoderCtx[A] = A

  implicit def currentEncoderValueType[A: EncoderCtx]: Type[A] = ectx.tpe

  abstract class EncoderDerivationRule(val name: String) extends Rule {
    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[scala.xml.Elem]]]
  }

  // The actual derivation logic

  def deriveEncoderRecursively[A: EncoderCtx]: MIO[Expr[scala.xml.Elem]] =
    Log
      .namedScope(s"Deriving XML encoder for type ${Type[A].prettyPrint}") {
        Rules(
          EncUseCachedDefWhenAvailableRule,
          EncUseImplicitWhenAvailableRule,
          EncHandleAsBuiltInRule,
          EncHandleAsValueTypeRule,
          EncHandleAsOptionRule,
          EncHandleAsMapRule,
          EncHandleAsCollectionRule,
          EncHandleAsSingletonRule,
          EncHandleAsCaseClassRule,
          EncHandleAsEnumRule
        )(_[A]).flatMap {
          case Right(result) =>
            Log.info(s"Derived XML encoder for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
              MIO.pure(result)
          case Left(reasons) =>
            val reasonsStrings = reasons.toListMap
              .removed(EncUseCachedDefWhenAvailableRule)
              .view
              .map { case (rule, reasons) =>
                if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
                else
                  s" - The rule ${rule.name} was not applicable, for the following reasons: ${reasons.mkString(", ")}"
              }
              .toList
            val err = EncoderDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
            Log.error(err.message) >> MIO.fail(err)
        }
      }

  // Rules

  object EncUseCachedDefWhenAvailableRule extends EncoderDerivationRule("use cached def when available") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[scala.xml.Elem]]] =
      Log.info(s"Attempting to use cached XML encoder for ${Type[A].prettyPrint}") >>
        ectx.getInstance[A].flatMap {
          case Some(instance) => callCachedInstance[A](instance)
          case None           =>
            ectx.getHelper[A].flatMap {
              case Some(helperCall) => callCachedHelper[A](helperCall)
              case None             => yieldUnsupported[A]
            }
        }

    @scala.annotation.nowarn("msg=is never used")
    private def callCachedInstance[A: EncoderCtx](
        instance: Expr[XmlEncoder[A]]
    ): MIO[Rule.Applicability[Expr[scala.xml.Elem]]] = {
      implicit val EncoderAT: Type[hearth.kindlings.xmlderivation.XmlEncoder[A]] = Types.XmlEncoder[A]
      val publicInstance: Expr[hearth.kindlings.xmlderivation.XmlEncoder[A]] =
        instance.upcast[hearth.kindlings.xmlderivation.XmlEncoder[A]]
      Log.info(s"Found cached XML encoder instance for ${Type[A].prettyPrint}") >> MIO.pure(Rule.matched(Expr.quote {
        Expr.splice(publicInstance).encode(Expr.splice(ectx.value), Expr.splice(ectx.elementName))
      }))
    }

    private def callCachedHelper[A: EncoderCtx](
        helperCall: (Expr[A], Expr[String], Expr[XmlConfig]) => Expr[scala.xml.Elem]
    ): MIO[Rule.Applicability[Expr[scala.xml.Elem]]] =
      Log.info(s"Found cached XML encoder helper for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(helperCall(ectx.value, ectx.elementName, ectx.config))
      )

    private def yieldUnsupported[A: EncoderCtx]: MIO[Rule.Applicability[Expr[scala.xml.Elem]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached XML encoder"))
  }

  object EncUseImplicitWhenAvailableRule extends EncoderDerivationRule("use implicit when available") {

    lazy val ignoredImplicits: Seq[UntypedMethod] = {
      val ours = Type.of[KindlingsXmlEncoder.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      }
      ours
    }

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[scala.xml.Elem]]] =
      Log.info(s"Attempting to use implicit XmlEncoder for ${Type[A].prettyPrint}") >> {
        if (ectx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          Types.XmlEncoder[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) => cacheAndUse[A](instanceExpr)
            case Left(reason)        => yieldUnsupported[A](reason)
          }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def cacheAndUse[A: EncoderCtx](
        instanceExpr: Expr[XmlEncoder[A]]
    ): MIO[Rule.Applicability[Expr[scala.xml.Elem]]] = {
      implicit val EncoderAT: Type[hearth.kindlings.xmlderivation.XmlEncoder[A]] = Types.XmlEncoder[A]
      val publicInstance: Expr[hearth.kindlings.xmlderivation.XmlEncoder[A]] =
        instanceExpr.upcast[hearth.kindlings.xmlderivation.XmlEncoder[A]]
      Log.info(s"Found implicit XML encoder ${instanceExpr.prettyPrint}, using directly") >>
        MIO.pure(Rule.matched(Expr.quote {
          Expr.splice(publicInstance).encode(Expr.splice(ectx.value), Expr.splice(ectx.elementName))
        }))
    }

    private def yieldUnsupported[A: EncoderCtx](reason: String): MIO[Rule.Applicability[Expr[scala.xml.Elem]]] =
      MIO.pure(
        Rule.yielded(
          s"The type ${Type[A].prettyPrint} does not have an implicit XmlEncoder instance: $reason"
        )
      )
  }

  @scala.annotation.nowarn("msg=is never used")
  object EncHandleAsBuiltInRule extends EncoderDerivationRule("handle as built-in primitive type") {

    implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
    implicit val BooleanT: Type[Boolean] = Types.Boolean
    implicit val ByteT: Type[Byte] = Types.Byte
    implicit val ShortT: Type[Short] = Types.Short
    implicit val IntT: Type[Int] = Types.Int
    implicit val LongT: Type[Long] = Types.Long
    implicit val FloatT: Type[Float] = Types.Float
    implicit val DoubleT: Type[Double] = Types.Double
    implicit val CharT: Type[Char] = Types.Char
    implicit val StringT: Type[String] = Types.String
    implicit val BigDecimalT: Type[BigDecimal] = Types.BigDecimal
    implicit val BigIntT: Type[BigInt] = Types.BigInt

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[scala.xml.Elem]]] =
      Log.info(s"Attempting to use built-in support for ${Type[A].prettyPrint}") >> MIO {
        if (Type[A] <:< Type[String]) Rule.matched(Expr.quote {
          XmlDerivationUtils.makeTextElem(Expr.splice(ectx.elementName), Expr.splice(ectx.value.upcast[String]))
        })
        else if (Type[A] <:< Type[Boolean]) Rule.matched(Expr.quote {
          XmlDerivationUtils.makeTextElem(Expr.splice(ectx.elementName), Expr.splice(ectx.value.upcast[Boolean]).toString)
        })
        else if (Type[A] <:< Type[Int]) Rule.matched(Expr.quote {
          XmlDerivationUtils.makeTextElem(Expr.splice(ectx.elementName), Expr.splice(ectx.value.upcast[Int]).toString)
        })
        else if (Type[A] <:< Type[Long]) Rule.matched(Expr.quote {
          XmlDerivationUtils.makeTextElem(Expr.splice(ectx.elementName), Expr.splice(ectx.value.upcast[Long]).toString)
        })
        else if (Type[A] <:< Type[Double]) Rule.matched(Expr.quote {
          XmlDerivationUtils.makeTextElem(Expr.splice(ectx.elementName), Expr.splice(ectx.value.upcast[Double]).toString)
        })
        else if (Type[A] <:< Type[Float]) Rule.matched(Expr.quote {
          XmlDerivationUtils.makeTextElem(Expr.splice(ectx.elementName), Expr.splice(ectx.value.upcast[Float]).toString)
        })
        else if (Type[A] <:< Type[Short]) Rule.matched(Expr.quote {
          XmlDerivationUtils.makeTextElem(Expr.splice(ectx.elementName), Expr.splice(ectx.value.upcast[Short]).toString)
        })
        else if (Type[A] <:< Type[Byte]) Rule.matched(Expr.quote {
          XmlDerivationUtils.makeTextElem(Expr.splice(ectx.elementName), Expr.splice(ectx.value.upcast[Byte]).toString)
        })
        else if (Type[A] <:< Type[Char]) Rule.matched(Expr.quote {
          XmlDerivationUtils.makeTextElem(Expr.splice(ectx.elementName), Expr.splice(ectx.value.upcast[Char]).toString)
        })
        else if (Type[A] <:< Type[BigDecimal]) Rule.matched(Expr.quote {
          XmlDerivationUtils.makeTextElem(Expr.splice(ectx.elementName), Expr.splice(ectx.value.upcast[BigDecimal]).toString)
        })
        else if (Type[A] <:< Type[BigInt]) Rule.matched(Expr.quote {
          XmlDerivationUtils.makeTextElem(Expr.splice(ectx.elementName), Expr.splice(ectx.value.upcast[BigInt]).toString)
        })
        else Rule.yielded(s"The type ${Type[A].prettyPrint} is not a built-in primitive type")
      }
  }

  object EncHandleAsValueTypeRule extends EncoderDerivationRule("handle as value type when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[scala.xml.Elem]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner
            val unwrappedExpr = isValueType.value.unwrap(ectx.value)
            for {
              innerResult <- deriveEncoderRecursively[Inner](using ectx.nest(unwrappedExpr))
            } yield Rule.matched(innerResult)

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
        }
      }
  }

  @scala.annotation.nowarn("msg=is never used")
  object EncHandleAsOptionRule extends EncoderDerivationRule("handle as Option when possible") {
    implicit val ElemT: Type[scala.xml.Elem] = Types.Elem

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[scala.xml.Elem]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Option") >> {
        Type[A] match {
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            LambdaBuilder
              .of1[Inner]("inner")
              .traverse { innerExpr =>
                deriveEncoderRecursively[Inner](using ectx.nest(innerExpr))
              }
              .map { builder =>
                val lambda = builder.build[scala.xml.Elem]
                Rule.matched(
                  isOption.value.fold[scala.xml.Elem](ectx.value)(
                    onEmpty = Expr.quote(XmlDerivationUtils.makeEmptyElem(Expr.splice(ectx.elementName))),
                    onSome = innerExpr =>
                      Expr.quote {
                        Expr.splice(lambda).apply(Expr.splice(innerExpr))
                      }
                  )
                )
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
        }
      }
  }

  @scala.annotation.nowarn("msg=Infinite loop")
  object EncHandleAsMapRule extends EncoderDerivationRule("handle as map when possible") {
    implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
    implicit val StringT: Type[String] = Types.String

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[scala.xml.Elem]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a map") >> {
        Type[A] match {
          case IsMap(isMap) =>
            import isMap.Underlying as Pair
            deriveMapEntries[A, Pair](isMap.value)

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a map"))
        }
      }

    private def deriveMapEntries[A: EncoderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[scala.xml.Elem]]] = {
      import isMap.{Key, Value}
      if (!(Key <:< Type[String]))
        MIO.pure(Rule.yielded(s"Map key type ${Key.prettyPrint} is not String"))
      else {
        LambdaBuilder
          .of2[Value, String]("mapValue", "itemElementName")
          .traverse { case (valueExpr, itemNameExpr) =>
            deriveEncoderRecursively[Value](using ectx.nest(valueExpr).copy(elementName = itemNameExpr))
          }
          .map { builder =>
            val lambda = builder.build[scala.xml.Elem]
            val iterableExpr = isMap.asIterable(ectx.value)
            Rule.matched(Expr.quote {
              val entries = Expr.splice(iterableExpr).asInstanceOf[Iterable[(String, Value)]]
              val children = XmlDerivationUtils.encodeMappedPairs[Value](
                entries,
                "entry",
                "key",
                (v: Value, n: String) => Expr.splice(lambda).apply(v, n)
              )
              XmlDerivationUtils.makeElem(Expr.splice(ectx.elementName), Nil, children)
            })
          }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  object EncHandleAsCollectionRule extends EncoderDerivationRule("handle as collection when possible") {
    implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
    implicit val StringT: Type[String] = Types.String

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[scala.xml.Elem]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            LambdaBuilder
              .of2[Item, String]("collItem", "itemElementName")
              .traverse { case (itemExpr, itemNameExpr) =>
                deriveEncoderRecursively[Item](using ectx.nest(itemExpr).copy(elementName = itemNameExpr))
              }
              .map { builder =>
                val lambda = builder.build[scala.xml.Elem]
                val iterableExpr = isCollection.value.asIterable(ectx.value)
                Rule.matched(Expr.quote {
                  val items = Expr.splice(iterableExpr)
                  val children = XmlDerivationUtils.encodeIterable[Item](
                    items,
                    "item",
                    (i: Item, n: String) => Expr.splice(lambda).apply(i, n)
                  )
                  XmlDerivationUtils.makeElem(Expr.splice(ectx.elementName), Nil, children)
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }

  @scala.annotation.nowarn("msg=is never used")
  object EncHandleAsSingletonRule extends EncoderDerivationRule("handle as singleton when possible") {
    implicit val ElemT: Type[scala.xml.Elem] = Types.Elem

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[scala.xml.Elem]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a singleton") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) if caseClass.primaryConstructor.parameters.flatten.isEmpty =>
            MIO.pure(Rule.matched(Expr.quote {
              XmlDerivationUtils.makeEmptyElem(Expr.splice(ectx.elementName))
            }))
          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a singleton/empty case class"))
        }
      }
  }

  object EncHandleAsCaseClassRule extends EncoderDerivationRule("handle as case class when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[scala.xml.Elem]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            val allFields = caseClass.caseFieldValuesAt(ectx.value).toList
            if (allFields.isEmpty)
              MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is an empty case class, handled by singleton rule"))
            else
              for {
                _ <- ectx.setHelper[A] { (value, name, config) =>
                  encodeCaseClassFields[A](caseClass)(
                    using ectx.nestInCache(value, name, config)
                  )
                }
                result <- ectx.getHelper[A].flatMap {
                  case Some(helperCall) =>
                    MIO.pure(Rule.matched(helperCall(ectx.value, ectx.elementName, ectx.config)))
                  case None => MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
                }
              } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used|dead code")
    private def encodeCaseClassFields[A: EncoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[scala.xml.Elem]] = {
      implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
      implicit val StringT: Type[String] = Types.String
      implicit val NodeT: Type[scala.xml.Node] = Types.Node
      implicit val XmlConfigT: Type[XmlConfig] = Types.XmlConfig
      implicit val XmlFieldModeT: Type[XmlFieldMode] = Types.XmlFieldMode
      implicit val ProductType: Type[Product] = Types.Product
      implicit val IntType: Type[Int] = Types.Int
      implicit val AnyType: Type[Any] = Types.Any
      implicit val transientFieldT: Type[transientField] = Types.TransientField
      implicit val xmlNameT: Type[xmlName] = Types.XmlNameAnnotation
      implicit val xmlAttributeT: Type[xmlAttribute] = Types.XmlAttributeAnnotation
      implicit val xmlElementT: Type[xmlElement] = Types.XmlElementAnnotation
      implicit val xmlContentT: Type[xmlContent] = Types.XmlContentAnnotation
      implicit val xmlWrapperT: Type[xmlWrapper] = Types.XmlWrapperAnnotation

      val allFields = caseClass.caseFieldValuesAt(ectx.value).toList
      val paramsByName: Map[String, Parameter] =
        if (allFields.isEmpty) Map.empty
        else caseClass.primaryConstructor.parameters.flatten.toMap

      NonEmptyList.fromList(allFields) match {
        case Some(fieldValues) =>
          fieldValues
            .traverse { case (fName, fieldValue) =>
              import fieldValue.{Underlying as Field, value as fieldExpr}
              val paramOpt = paramsByName.get(fName)
              val isTransient = paramOpt.exists(p => hasAnnotationType[transientField](p))
              val customName = paramOpt.flatMap(p => getAnnotationStringArg[xmlName](p))
              val isAttrAnnotated = paramOpt.exists(p => hasAnnotationType[xmlAttribute](p))
              val isElemAnnotated = paramOpt.exists(p => hasAnnotationType[xmlElement](p))
              val isContentAnnotated = paramOpt.exists(p => hasAnnotationType[xmlContent](p))

              if (isTransient) {
                MIO.pure(FieldEncoding.Skip)
              } else if (isContentAnnotated) {
                deriveEncoderRecursively[Field](using ectx.nest(fieldExpr)).map { encodedExpr =>
                  FieldEncoding.Content(encodedExpr)
                }
              } else if (isAttrAnnotated) {
                val xmlFieldName = customName.getOrElse(fName)
                MIO.pure(FieldEncoding.Attr(xmlFieldName, fieldExpr.upcast[Any]))
              } else if (isElemAnnotated) {
                val xmlFieldName = customName.getOrElse(fName)
                val wrapperName = paramOpt.flatMap(p => getAnnotationStringArg[xmlWrapper](p))
                deriveEncoderRecursively[Field](using ectx.nest(fieldExpr).copy(
                  elementName = Expr(xmlFieldName)
                )).map { encodedExpr =>
                  wrapperName match {
                    case Some(wrapper) => FieldEncoding.WrappedChild(wrapper, encodedExpr)
                    case None          => FieldEncoding.Child(encodedExpr)
                  }
                }
              } else {
                // Default mode from config - use Element by default
                val xmlFieldName = customName.getOrElse(fName)
                deriveEncoderRecursively[Field](using ectx.nest(fieldExpr).copy(
                  elementName = Expr(xmlFieldName)
                )).map { encodedExpr =>
                  FieldEncoding.Child(encodedExpr)
                }
              }
            }
            .map { encodings =>
              val attrList: List[(String, Expr[Any])] = encodings.toList.collect {
                case FieldEncoding.Attr(name, expr) => (name, expr)
              }
              val childList: List[Expr[scala.xml.Elem]] = encodings.toList.collect {
                case FieldEncoding.Child(expr)             => expr
                case FieldEncoding.WrappedChild(_, expr)   => expr
              }
              val contentOpt: Option[Expr[scala.xml.Elem]] = encodings.toList.collectFirst {
                case FieldEncoding.Content(expr) => expr
              }

              // Build attributes using foldRight
              val attrListExpr: Expr[List[(String, String)]] = attrList.foldRight(
                Expr.quote(List.empty[(String, String)])
              ) { case ((name, valueExpr), acc) =>
                Expr.quote {
                  (Expr.splice(Expr(name)), Expr.splice(valueExpr).toString) :: Expr.splice(acc)
                }
              }

              // Build children list using foldRight
              val childListExpr: Expr[List[scala.xml.Node]] = childList.foldRight(
                Expr.quote(List.empty[scala.xml.Node])
              ) { (elem, acc) =>
                Expr.quote {
                  (Expr.splice(elem): scala.xml.Node) :: Expr.splice(acc)
                }
              }

              contentOpt match {
                case Some(contentExpr) =>
                  // Content mode: attributes + content from annotated field
                  Expr.quote {
                    val attrs = Expr.splice(attrListExpr)
                    val contentElem = Expr.splice(contentExpr)
                    XmlDerivationUtils.combineAttributesAndChildren(
                      Expr.splice(ectx.elementName),
                      attrs,
                      contentElem.child.toList
                    )
                  }
                case None =>
                  // Normal mode: attributes + child elements
                  Expr.quote {
                    val attrs = Expr.splice(attrListExpr)
                    val children: List[scala.xml.Node] = Expr.splice(childListExpr)
                    XmlDerivationUtils.combineAttributesAndChildren(
                      Expr.splice(ectx.elementName),
                      attrs,
                      children
                    )
                  }
              }
            }
        case None =>
          MIO.pure(Expr.quote {
            XmlDerivationUtils.makeEmptyElem(Expr.splice(ectx.elementName))
          })
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  object EncHandleAsEnumRule extends EncoderDerivationRule("handle as sealed trait/enum when possible") {
    implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
    implicit val StringT: Type[String] = Types.String

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[scala.xml.Elem]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a sealed trait/enum") >> {
        Enum.parse[A].toEither match {
          case Right(parsedEnum) =>
            deriveEnumEncoder[A](parsedEnum)
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    private def deriveEnumEncoder[A: EncoderCtx](
        enumType: Enum[A]
    ): MIO[Rule.Applicability[Expr[scala.xml.Elem]]] = {
      val childrenList = enumType.directChildren.toList

      enumType
        .parMatchOn[MIO, scala.xml.Elem](ectx.value) { matched =>
          import matched.{value as enumCaseValue, Underlying as EnumCase}
          Log.namedScope(s"Encoding enum case ${enumCaseValue.prettyPrint}: ${EnumCase.prettyPrint}") {
            deriveEncoderRecursively[EnumCase](using ectx.nest(enumCaseValue)).map { caseElem =>
              val caseName: String = childrenList
                .find { case (_, child) =>
                  import child.Underlying as ChildType
                  Type[EnumCase] <:< Type[ChildType]
                }
                .map(_._1)
                .getOrElse(Type[EnumCase].shortName)
              Expr.quote {
                val inner = Expr.splice(caseElem)
                XmlDerivationUtils.addDiscriminator(inner, "type", Expr.splice(Expr(caseName)))
              }
            }
          }
        }
        .flatMap {
          case Some(result) => MIO.pure(Rule.matched(result))
          case None         =>
            val err = EncoderDerivationError.UnsupportedType(Type[A].prettyPrint, List("Enum has no children"))
            Log.error(err.message) >> MIO.fail(err)
        }
    }
  }

  // Field encoding result type
  sealed trait FieldEncoding
  object FieldEncoding {
    case object Skip extends FieldEncoding
    case class Attr(name: String, value: Expr[Any]) extends FieldEncoding
    case class Child(elem: Expr[scala.xml.Elem]) extends FieldEncoding
    case class WrappedChild(wrapperName: String, elem: Expr[scala.xml.Elem]) extends FieldEncoding
    case class Content(elem: Expr[scala.xml.Elem]) extends FieldEncoding
  }

  // Error types

  sealed abstract class EncoderDerivationError(val message: String) extends Exception(message)
  object EncoderDerivationError {
    final case class UnsupportedType(typeName: String, reasons: List[String])
        extends EncoderDerivationError(
          s"Cannot derive XmlEncoder for type $typeName:\n${reasons.mkString("\n")}"
        )
  }

  // Types

  private[compiletime] object Types {
    def Elem: Type[scala.xml.Elem] = Type.of[scala.xml.Elem]
    def Node: Type[scala.xml.Node] = Type.of[scala.xml.Node]
    def XmlConfig: Type[hearth.kindlings.xmlderivation.XmlConfig] = Type.of[hearth.kindlings.xmlderivation.XmlConfig]
    def XmlFieldMode: Type[hearth.kindlings.xmlderivation.XmlFieldMode] =
      Type.of[hearth.kindlings.xmlderivation.XmlFieldMode]
    def String: Type[String] = Type.of[String]
    def Boolean: Type[Boolean] = Type.of[Boolean]
    def Byte: Type[Byte] = Type.of[Byte]
    def Short: Type[Short] = Type.of[Short]
    def Int: Type[Int] = Type.of[Int]
    def Long: Type[Long] = Type.of[Long]
    def Float: Type[Float] = Type.of[Float]
    def Double: Type[Double] = Type.of[Double]
    def Char: Type[Char] = Type.of[Char]
    def BigDecimal: Type[BigDecimal] = Type.of[BigDecimal]
    def BigInt: Type[BigInt] = Type.of[BigInt]
    def Any: Type[Any] = Type.of[Any]
    def Product: Type[Product] = Type.of[Product]
    def TransientField: Type[hearth.kindlings.xmlderivation.annotations.transientField] =
      Type.of[hearth.kindlings.xmlderivation.annotations.transientField]
    def XmlNameAnnotation: Type[hearth.kindlings.xmlderivation.annotations.xmlName] =
      Type.of[hearth.kindlings.xmlderivation.annotations.xmlName]
    def XmlAttributeAnnotation: Type[hearth.kindlings.xmlderivation.annotations.xmlAttribute] =
      Type.of[hearth.kindlings.xmlderivation.annotations.xmlAttribute]
    def XmlElementAnnotation: Type[hearth.kindlings.xmlderivation.annotations.xmlElement] =
      Type.of[hearth.kindlings.xmlderivation.annotations.xmlElement]
    def XmlContentAnnotation: Type[hearth.kindlings.xmlderivation.annotations.xmlContent] =
      Type.of[hearth.kindlings.xmlderivation.annotations.xmlContent]
    def XmlWrapperAnnotation: Type[hearth.kindlings.xmlderivation.annotations.xmlWrapper] =
      Type.of[hearth.kindlings.xmlderivation.annotations.xmlWrapper]
    def XmlEncoder[A: Type]: Type[hearth.kindlings.xmlderivation.XmlEncoder[A]] =
      Type.of[hearth.kindlings.xmlderivation.XmlEncoder[A]]
    def KindlingsXmlEncoder[A: Type]: Type[hearth.kindlings.xmlderivation.KindlingsXmlEncoder[A]] =
      Type.of[hearth.kindlings.xmlderivation.KindlingsXmlEncoder[A]]
    val EncoderLogDerivation: Type[hearth.kindlings.xmlderivation.KindlingsXmlEncoder.LogDerivation] =
      Type.of[hearth.kindlings.xmlderivation.KindlingsXmlEncoder.LogDerivation]
  }

  private type XmlEncoder[A] = hearth.kindlings.xmlderivation.XmlEncoder[A]
}
