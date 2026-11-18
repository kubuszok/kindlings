package hearth.kindlings.sconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.sconfigderivation.CoproductHint
import hearth.kindlings.sconfigderivation.internal.runtime.SConfigDerivationUtils
import org.ekrich.config.ConfigValue
import hearth.kindlings.derivation.compiletime.StableChildrenOrder

trait WriterHandleAsEnumRuleImpl {
  this: WriterMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object WriterHandleAsEnumRule extends WriterDerivationRule("handle as enum when possible") {

    def apply[A: WriterCtx]: MIO[Rule.Applicability[Expr[ConfigValue]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            encodeEnumCases[A](enumm).map(Rule.matched)
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def encodeEnumCases[A: WriterCtx](
        enumm: Enum[A]
    ): MIO[Expr[ConfigValue]] = {
      implicit val ConfigValueT: Type[ConfigValue] = WTypes.ConfigValue
      @scala.annotation.nowarn("msg=is never used")
      implicit val StringT: Type[String] = WTypes.String
      implicit val FuncT: Type[String => String] = WTypes.StringToString
      implicit val OptionStringT: Type[Option[String]] = WTypes.OptionString
      implicit val FieldHintT: Type[CoproductHint.Field[A]] = WTypes.fieldCoproductHintType[A]
      implicit val WrappedHintT: Type[CoproductHint.Wrapped[A]] = WTypes.wrappedCoproductHintType[A]

      val maybeFieldHint: Option[Expr[CoproductHint.Field[A]]] =
        Expr.summonImplicit[CoproductHint.Field[A]].toOption
      val maybeWrappedHint: Option[Expr[CoproductHint.Wrapped[A]]] =
        Expr.summonImplicit[CoproductHint.Wrapped[A]].toOption
      // Upstream `FirstSuccessCoproductHint.to` writes the subtype's value as is, with no disambiguation information.
      val isFirstSuccess: Boolean = {
        implicit val FirstSuccessHintT: Type[CoproductHint.FirstSuccess[A]] = WTypes.firstSuccessCoproductHintType[A]
        Expr.summonImplicit[CoproductHint.FirstSuccess[A]].toOption.isDefined
      }

      // How the subtypes' names are transformed: by a per-type hint, or by the global config (`None` when it is known
      // at compile time: the names are then constants).
      val runtimeTransform: Option[Expr[String => String]] = maybeFieldHint match {
        case Some(h) => Some(Expr.quote(Expr.splice(h).transformConstructorNames))
        case None    =>
          maybeWrappedHint match {
            case Some(h) => Some(Expr.quote(Expr.splice(h).transformConstructorNames))
            case None    =>
              if (wctx.evaluatedConfig.isDefined) None
              else Some(Expr.quote(Expr.splice(wctx.instanceConfig).transformConstructorNames))
          }
      }

      // A discriminator field (`Right(Some(field))`), a wrapper key / string (`Right(None)`), decided at compile time
      // unless the global config is only known at runtime (`Left`).
      val discriminatorMode: Either[Expr[Option[String]], Option[Expr[String]]] = maybeFieldHint match {
        case Some(h) => Right(Some(Expr.quote(Expr.splice(h).fieldName)))
        case None    =>
          maybeWrappedHint match {
            case Some(_) => Right(None)
            case None    =>
              wctx.evaluatedConfig match {
                case Some(c) => Right(c.discriminator.map(f => Expr(f)))
                case None    => Left(Expr.quote(Expr.splice(wctx.config).discriminator))
              }
          }
      }

      val childrenList = StableChildrenOrder.directChildren(WriterHandleAsEnumRuleImpl.this)(enumm)
      val isEnumerationOrJavaEnum = Type[A].isEnumeration || Type[A].isJavaEnum
      val allCaseObjects = isEnumerationOrJavaEnum || childrenList.forall { case (_, child) =>
        SingletonValue.unapply(child.Underlying).isDefined
      }

      val namesMIO: MIO[String => Expr[String]] = runtimeTransform match {
        case None =>
          val config = wctx.evaluatedConfig.get
          MIO.pure((name: String) => Expr(config.transformConstructorNames(name)))
        case Some(transform) =>
          // computed once per instance (a lazy val), not on every write
          implicit val ArrayStringT: Type[Array[String]] = WTypes.ArrayString
          implicit val IntT: Type[Int] = WTypes.IntType
          implicit val ListStringT: Type[List[String]] = WTypes.ListStringType
          val childNames = childrenList.map(_._1)
          val cacheKey = s"cached-subtype-names:${Type[A].plainPrint}"
          val names: Expr[Array[String]] = Expr.quote {
            val transformName = Expr.splice(transform)
            Expr.splice(Expr(childNames)).map(transformName).toArray
          }
          val builder = ValDefBuilder.ofLazy[Array[String]](s"subtypeNames_${Type[A].shortName}")
          for {
            state <- wctx.cache.get
            _ <-
              if (builder.isBuilt(state, cacheKey)) MIO.pure(())
              else wctx.cache.buildCachedWith(cacheKey, builder)(_ => names)
            cached <- wctx.cache.get0Ary[Array[String]](cacheKey)
          } yield (name: String) =>
            Expr.quote(Expr.splice(cached.get)(Expr.splice(Expr(math.max(childNames.indexOf(name), 0)))))
      }

      // `name`: the subtype's name; `named(name)`: its value as the mode says
      def written(nameExpr: Expr[String], caseValue: Expr[ConfigValue]): Expr[ConfigValue] = {
        def withField(field: Expr[String]): Expr[ConfigValue] = Expr.quote {
          SConfigDerivationUtils.addDiscriminatorField(
            Expr.splice(field),
            Expr.splice(nameExpr),
            Expr.splice(caseValue)
          )
        }
        val withoutField: Expr[ConfigValue] =
          if (allCaseObjects) Expr.quote(SConfigDerivationUtils.writeEnumAsString(Expr.splice(nameExpr)))
          else Expr.quote(SConfigDerivationUtils.wrapWithTypeName(Expr.splice(nameExpr), Expr.splice(caseValue)))
        discriminatorMode match {
          case Right(Some(field)) => withField(field)
          case Right(None)        => withoutField
          case Left(runtime)      =>
            Expr.quote {
              Expr.splice(runtime) match {
                case Some(discriminatorField) => Expr.splice(withField(Expr.quote(discriminatorField)))
                case None                     => Expr.splice(withoutField)
              }
            }
        }
      }

      namesMIO.flatMap { nameOf =>
        enumm
          .parMatchOn[MIO, ConfigValue](wctx.value) { matched =>
            import matched.{value as enumCaseValue, Underlying as EnumCase}
            Log.namedScope(s"Writing enum case ${enumCaseValue.prettyPrint}: ${EnumCase.prettyPrint}") {
              val caseValueMIO: MIO[Expr[ConfigValue]] =
                if (isEnumerationOrJavaEnum)
                  MIO.pure(Expr.quote(SConfigDerivationUtils.emptyConfigObject))
                else deriveWriterRecursively[EnumCase](using wctx.nest(enumCaseValue))
              caseValueMIO.map { caseValue =>
                val caseName: String = childrenList
                  .find { case (_, child) =>
                    import child.Underlying as ChildType
                    Type[EnumCase] <:< Type[ChildType]
                  }
                  .map(_._1)
                  .getOrElse(Type[EnumCase].shortName)
                if (isFirstSuccess) caseValue
                else written(nameOf(caseName), caseValue)
              }
            }
          }
          .flatMap {
            case Some(result) => MIO.pure(result)
            case None         =>
              val err = WriterDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint)
              Log.error(err.message) >> MIO.fail(err)
          }
      }
    }
  }
}
