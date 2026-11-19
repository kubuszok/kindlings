package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.xmlderivation.XmlConfig
import hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils

trait EncoderHandleAsEnumRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsEnumRule extends EncoderDerivationRule("handle as sealed trait/enum when possible") {
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

    private def hasUserEncoder[A: EncoderCtx]: ?? => Boolean =
      EncoderUseImplicitWhenAvailableRule.hasUserEncoder(ectx.derivedType)

    private def isValueEnum[T: Type]: Boolean = Type[T].isJavaEnum || Type[T].isEnumeration

    /** The name of a subtype: a constant with `@xmlName` or a configuration known at compile time. */
    @scala.annotation.nowarn("msg=is never used")
    private def nameExprOf[A: EncoderCtx](subtype: XmlSubtype, config: Expr[XmlConfig]): Expr[String] = {
      implicit val XmlConfigT: Type[XmlConfig] = Types.XmlConfig
      (subtype.customName, ectx.evaluatedConfig) match {
        case (Some(custom), _) => Expr(custom)
        case (None, Some(cfg)) => Expr(cfg.constructorNameMapper(subtype.rawName))
        case (None, None)      =>
          Expr.quote(Expr.splice(config).constructorNameMapper(Expr.splice(Expr(subtype.rawName))))
      }
    }

    private def subtypeOf[Case: Type](subtypes: List[XmlSubtype]): XmlSubtype =
      subtypes
        .find { subtype =>
          import subtype.tpe.Underlying as Leaf
          Type[Case] <:< Type[Leaf]
        }
        .getOrElse(XmlSubtype(Type[Case].shortName, None, Type[Case].as_??))

    @scala.annotation.nowarn("msg=is never used")
    private def deriveEnumEncoder[A: EncoderCtx](
        enumType: Enum[A]
    ): MIO[Rule.Applicability[Expr[scala.xml.Elem]]] = {
      implicit val XmlConfigT: Type[XmlConfig] = Types.XmlConfig
      val subtypes = xmlSubtypesOf[A](enumType)(hasUserEncoder[A])
      // an enum whose every case is a singleton can be written as the case name (`enumAsStrings`)
      val isStringEnum = subtypes.nonEmpty && (isValueEnum[A] || subtypes.forall { subtype =>
        SingletonValue.unapply(subtype.tpe.Underlying).isDefined
      })

      def tagged: MIO[Expr[scala.xml.Elem]] =
        encodeCasesOf[A, A](enumType, ectx.value, subtypes).flatMap {
          case Some(result) => MIO.pure(result)
          case None         =>
            val err = EncoderDerivationError.UnsupportedType(Type[A].prettyPrint, List("Enum has no children"))
            Log.error(err.message) >> MIO.fail(err)
        }
      // an enum of singletons: an empty element tagged with the case name (one `match`, in the cached name def)
      def taggedSingletons: MIO[Expr[scala.xml.Elem]] = enumNameOf[A](subtypes).map { nameOf =>
        def withDiscriminator(attribute: Expr[String]): Expr[scala.xml.Elem] = Expr.quote {
          XmlDerivationUtils.addDiscriminator(
            XmlDerivationUtils.makeEmptyElem(Expr.splice(ectx.elementName)),
            Expr.splice(attribute),
            Expr.splice(nameOf(ectx.value, ectx.config))
          )
        }
        def wrapped: Expr[scala.xml.Elem] = Expr.quote {
          XmlDerivationUtils.wrapWithTypeName(
            Expr.splice(ectx.elementName),
            XmlDerivationUtils.makeEmptyElem(Expr.splice(nameOf(ectx.value, ectx.config)))
          )
        }
        ectx.evaluatedConfig.map(_.discriminatorAttribute) match {
          case Some(Some(attribute)) => withDiscriminator(Expr(attribute))
          case Some(None)            => wrapped
          case None                  =>
            Expr.quote {
              if (Expr.splice(ectx.config).discriminatorAttribute.isDefined)
                Expr.splice(withDiscriminator(Expr.quote(Expr.splice(ectx.config).discriminatorAttribute.get)))
              else Expr.splice(wrapped)
            }
        }
      }
      def asString: MIO[Expr[scala.xml.Elem]] = enumNameOf[A](subtypes).map { nameOf =>
        Expr.quote {
          XmlDerivationUtils
            .makeTextElem(Expr.splice(ectx.elementName), Expr.splice(nameOf(ectx.value, ectx.config)))
        }
      }

      // With a config known at compile time only the applicable encoding is generated.
      (if (!isStringEnum) tagged
       else
         ectx.evaluatedConfig.map(_.enumAsStrings) match {
           case Some(true)  => asString
           case Some(false) => taggedSingletons
           case None        =>
             for {
               string <- asString
               element <- taggedSingletons
             } yield Expr.quote {
               if (Expr.splice(ectx.config).enumAsStrings) Expr.splice(string) else Expr.splice(element)
             }
         }).map(Rule.matched)
    }

    /** The `match` over `B`'s cases writing each leaf with its discriminator attribute (or inside the element named
      * after it, without a discriminator). A case that is a nested sealed type gets a nested `match`, so its leaves are
      * written under their own names.
      */
    @scala.annotation.nowarn("msg=is never used")
    private def encodeCasesOf[A: EncoderCtx, B: Type](
        enumB: Enum[B],
        value: Expr[B],
        subtypes: List[XmlSubtype]
    ): MIO[Option[Expr[scala.xml.Elem]]] = {
      implicit val XmlConfigT: Type[XmlConfig] = Types.XmlConfig
      enumB.parMatchOn[MIO, scala.xml.Elem](value) { matched =>
        import matched.{value as enumCaseValue, Underlying as EnumCase}
        Log.namedScope(s"Encoding enum case ${enumCaseValue.prettyPrint}: ${EnumCase.prettyPrint}") {
          val nested: Option[Enum[EnumCase]] =
            if (!isValueEnum[B] && isNestedXmlSum[EnumCase](hasUserEncoder[A])) Enum.parse[EnumCase].toOption
            else None
          nested match {
            case Some(nestedEnum) =>
              encodeCasesOf[A, EnumCase](nestedEnum, enumCaseValue, subtypes).flatMap {
                case Some(result) => MIO.pure(result)
                case None         =>
                  val err =
                    EncoderDerivationError.UnsupportedType(Type[EnumCase].prettyPrint, List("Enum has no children"))
                  Log.error(err.message) >> MIO.fail(err)
              }
            case None =>
              val nameExpr = nameExprOf[A](subtypeOf[EnumCase](subtypes), ectx.config)
              // the case's element under a given name (a value of a Java enum / an `Enumeration` has no type to
              // derive an encoder for: it is an empty element)
              def caseElem(elementName: Expr[String]): MIO[Expr[scala.xml.Elem]] =
                if (isValueEnum[B]) MIO.pure(Expr.quote(XmlDerivationUtils.makeEmptyElem(Expr.splice(elementName))))
                else deriveEncoderRecursively[EnumCase](using ectx.nest(enumCaseValue).copy(elementName = elementName))
              def withDiscriminator(attribute: Expr[String]): MIO[Expr[scala.xml.Elem]] =
                caseElem(ectx.elementName).map { elem =>
                  Expr.quote {
                    XmlDerivationUtils
                      .addDiscriminator(Expr.splice(elem), Expr.splice(attribute), Expr.splice(nameExpr))
                  }
                }
              // without a discriminator: <elementName><CaseName>...</CaseName></elementName>
              def wrapped: MIO[Expr[scala.xml.Elem]] = caseElem(nameExpr).map { elem =>
                Expr.quote(XmlDerivationUtils.wrapWithTypeName(Expr.splice(ectx.elementName), Expr.splice(elem)))
              }
              ectx.evaluatedConfig.map(_.discriminatorAttribute) match {
                case Some(Some(attribute)) => withDiscriminator(Expr(attribute))
                case Some(None)            => wrapped
                case None                  =>
                  // evaluatedConfig not available — use runtime check
                  for {
                    withAttribute <- withDiscriminator(Expr.quote(Expr.splice(ectx.config).discriminatorAttribute.get))
                    inWrapper <- wrapped
                  } yield Expr.quote {
                    if (Expr.splice(ectx.config).discriminatorAttribute.isDefined) Expr.splice(withAttribute)
                    else Expr.splice(inWrapper)
                  }
              }
          }
        }
      }
    }

    /** The cached `(value, config) => case name` def of an enum whose cases are all singletons: what is written as the
      * text of its element with `enumAsStrings`, and as its attribute value.
      */
    def enumNameOf[A: EncoderCtx](subtypes: List[XmlSubtype]): MIO[(Expr[A], Expr[XmlConfig]) => Expr[String]] =
      ectx.getEnumNameHelper[A].flatMap {
        case Some(helper) => MIO.pure(helper)
        case None         =>
          Enum.parse[A].toEither match {
            case Left(reason) =>
              MIO.fail(EncoderDerivationError.UnsupportedType(Type[A].prettyPrint, List(reason)))
            case Right(enumType) =>
              ectx.setEnumNameHelper[A] { (value, config) =>
                caseNameOf[A, A](enumType, value, config, subtypes)
              } >> ectx.getEnumNameHelper[A].flatMap {
                case Some(helper) => MIO.pure(helper)
                case None         =>
                  MIO.fail(new RuntimeException(s"No enum name helper for ${Type[A].prettyPrint}"))
              }
          }
      }

    private def caseNameOf[A: EncoderCtx, B: Type](
        enumB: Enum[B],
        value: Expr[B],
        config: Expr[XmlConfig],
        subtypes: List[XmlSubtype]
    ): MIO[Expr[String]] =
      enumB
        .parMatchOn[MIO, String](value) { matched =>
          import matched.{value as enumCaseValue, Underlying as EnumCase}
          val nested: Option[Enum[EnumCase]] =
            if (!isValueEnum[B] && isNestedXmlSum[EnumCase](hasUserEncoder[A])) Enum.parse[EnumCase].toOption
            else None
          nested match {
            case Some(nestedEnum) => caseNameOf[A, EnumCase](nestedEnum, enumCaseValue, config, subtypes)
            case None             => MIO.pure(nameExprOf[A](subtypeOf[EnumCase](subtypes), config))
          }
        }
        .flatMap {
          case Some(result) => MIO.pure(result)
          case None         =>
            MIO.fail(EncoderDerivationError.UnsupportedType(Type[B].prettyPrint, List("Enum has no children")))
        }
  }

}
