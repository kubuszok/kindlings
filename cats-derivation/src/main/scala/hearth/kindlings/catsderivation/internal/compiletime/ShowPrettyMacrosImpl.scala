package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait ShowPrettyMacrosImpl
    extends ShowMacrosImpl
    with StrictDerivationSupport
    with rules.ShowPrettyUseCachedRuleImpl
    with rules.ShowPrettyUseImplicitRuleImpl
    with rules.ShowPrettyCaseClassRuleImpl
    with rules.ShowPrettyEnumRuleImpl { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  def deriveShowPretty[A: Type]: Expr[hearth.kindlings.catsderivation.ShowPretty[A]] = {
    val macroName = "ShowPretty.derived"
    implicit val ShowPrettyA: Type[hearth.kindlings.catsderivation.ShowPretty[A]] = ShowPrettyTypes.ShowPretty[A]
    implicit val StringType: Type[String] = ShowTypes.String
    val selfType: Option[??] = Some(Type[A].as_??)

    deriveShowFromCtxAndAdaptForEntrypoint[A, hearth.kindlings.catsderivation.ShowPretty[A]](macroName, selfType)(
      deriveShowPrettyRecursively[A](using _)
    ) { callShow =>
      Expr.quote {
        hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories
          .showPrettyInstance[A] { (value: A) =>
            val _ = value
            Expr.splice(callShow(Expr.quote(value)))
          }
      }
    }
  }

  def deriveShowPrettyRecursively[A: ShowCtx]: MIO[Expr[String]] =
    Log.namedScope(s"Deriving ShowPretty for ${Type[A].prettyPrint}") {
      Rules(
        ShowPrettyUseCachedRule,
        ShowPrettyUseImplicitRule,
        ShowDerivationPolicyRule,
        ShowBuiltInRule,
        ShowValueTypeRule,
        ShowOptionRule,
        ShowMapRule,
        ShowCollectionRule,
        ShowSingletonRule,
        ShowPrettyCaseClassRule,
        ShowPrettyEnumRule
      )(_[A]).flatMap {
        case Right(result) =>
          Log.info(s"Derived ShowPretty for ${Type[A].prettyPrint}") >> MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap
            .removed(ShowPrettyUseCachedRule)
            .view
            .map { case (rule, reasons) =>
              if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
              else s" - ${rule.name}: ${reasons.mkString(", ")}"
            }
            .toList
          val err = ShowDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
          Log.error(err.message) >> MIO.fail(err)
      }
    }

  protected object ShowPrettyTypes {
    def ShowPretty: Type.Ctor1[hearth.kindlings.catsderivation.ShowPretty] =
      Type.Ctor1.of[hearth.kindlings.catsderivation.ShowPretty]
  }
}
