package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait GroupMacrosImpl
    extends SemigroupMacrosImpl
    with StrictDerivationSupport
    with rules.GroupUseCachedRuleImpl
    with rules.GroupUseImplicitRuleImpl
    with rules.GroupBuiltInRuleImpl
    with rules.GroupCaseClassRuleImpl
    with CatsDerivationTimeout { this: MacroCommons & StdExtensions =>

  final case class GroupDerivationResult[A](
      empty: Expr[A],
      combine: (Expr[A], Expr[A]) => MIO[Expr[A]],
      inverse: Expr[A] => MIO[Expr[A]]
  )

  final case class GroupCtx[A](
      tpe: Type[A],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  )
  object GroupCtx {
    def from[A: Type](derivedType: Option[??]): GroupCtx[A] =
      GroupCtx(Type[A], ValDefsCache.mlocal, derivedType)
  }

  def gctx[A](implicit A: GroupCtx[A]): GroupCtx[A] = A
  implicit def groupCtxType[A: GroupCtx]: Type[A] = gctx.tpe

  abstract class GroupDerivationRule(val name: String) extends Rule {
    def apply[A: GroupCtx]: MIO[Rule.Applicability[GroupDerivationResult[A]]]
  }

  def deriveGroupRecursively[A: GroupCtx]: MIO[GroupDerivationResult[A]] =
    Log.namedScope(s"Deriving Group for ${Type[A].prettyPrint}") {
      Rules(
        GroupUseCachedRule,
        GroupUseImplicitRule,
        GroupBuiltInRule,
        GroupCaseClassRule
      )(_[A]).flatMap {
        case Right(result) =>
          Log.info(s"Derived Group for ${Type[A].prettyPrint}") >> MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap
            .removed(GroupUseCachedRule)
            .view
            .map { case (rule, reasons) =>
              if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
              else s" - ${rule.name}: ${reasons.mkString(", ")}"
            }
            .toList
          val err = GroupDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
          Log.error(err.message) >> MIO.fail(err)
      }
    }

  @scala.annotation.nowarn("msg=is never used")
  def deriveGroup[A: Type]: Expr[cats.kernel.Group[A]] = {
    val macroName = "Group.derived"
    implicit val GroupA: Type[cats.kernel.Group[A]] = GroupTypes.Group[A]

    deriveGroupEntrypoint[A, cats.kernel.Group[A]](macroName) { (doEmpty, doCombine, doInverse) =>
      Expr.quote {
        hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories.groupInstance[A](
          Expr.splice(doEmpty),
          (x: A, y: A) => {
            val _ = x
            val _ = y
            Expr.splice(doCombine(Expr.quote(x), Expr.quote(y)))
          },
          (a: A) => {
            val _ = a
            Expr.splice(doInverse(Expr.quote(a)))
          }
        )
      }
    }
  }

  protected def deriveGroupEntrypoint[A: Type, Out: Type](macroName: String)(
      adapt: (Expr[A], (Expr[A], Expr[A]) => Expr[A], Expr[A] => Expr[A]) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended."
      )

    Log
      .namedScope(s"Deriving $macroName[${Type[A].prettyPrint}] at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          runSafe {
            val selfType: Option[??] = Some(Type[A].as_??)
            val ctx = GroupCtx.from[A](selfType)
            for {
              _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
              result <- deriveGroupRecursively[A](using ctx)
              cache <- ctx.cache.get
            } yield {
              val doEmpty: Expr[A] = result.empty
              val doCombine: (Expr[A], Expr[A]) => Expr[A] = (xExpr, yExpr) => runSafe(result.combine(xExpr, yExpr))
              val doInverse: Expr[A] => Expr[A] = (aExpr) => runSafe(result.inverse(aExpr))
              cache.toValDefs.use { _ =>
                adapt(doEmpty, doCombine, doInverse)
              }
            }
          }
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogSemigroupDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogSemigroupDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  protected object GroupTypes {
    def Group: Type.Ctor1[cats.kernel.Group] = Type.Ctor1.of[cats.kernel.Group]
  }
}

sealed private[compiletime] trait GroupDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object GroupDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends GroupDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any Group derivation rule:\n${reasons.mkString("\n")}"
  }
}
