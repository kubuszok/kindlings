package hearth.kindlings.diffderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*
import hearth.kindlings.diffderivation.*
import hearth.kindlings.diffderivation.internal.runtime.*

trait DiffMacrosImpl
    extends hearth.kindlings.derivation.compiletime.DerivationTimeout
    with hearth.kindlings.derivation.compiletime.LoadStandardExtensionsOnce
    with rules.DiffUseCachedRuleImpl
    with rules.DiffUseImplicitRuleImpl
    with rules.DiffBuiltInRuleImpl
    with rules.DiffValueTypeRuleImpl
    with rules.DiffOptionRuleImpl
    with rules.DiffMapRuleImpl
    with rules.DiffCollectionRuleImpl
    with rules.DiffSingletonRuleImpl
    with rules.DiffCaseClassRuleImpl
    with rules.DiffEnumRuleImpl { this: MacroCommons & StdExtensions =>

  override protected def derivationSettingsNamespace: String = "diffDerivation"

  // ── Type references ───────────────────────────────────────

  protected object DiffTypes {
    def DiffCtor: Type.Ctor1[Diff] = Type.Ctor1.of[Diff]
    def Diff[A: Type]: Type[Diff[A]] = DiffCtor.apply[A]
    val DiffResultType: Type[DiffResult] = Type.of[DiffResult]
    val StringType: Type[String] = Type.of[String]
    val BooleanType: Type[Boolean] = Type.of[Boolean]
    val IntType: Type[Int] = Type.of[Int]
    val ByteType: Type[Byte] = Type.of[Byte]
    val ShortType: Type[Short] = Type.of[Short]
    val LongType: Type[Long] = Type.of[Long]
    val FloatType: Type[Float] = Type.of[Float]
    val DoubleType: Type[Double] = Type.of[Double]
    val CharType: Type[Char] = Type.of[Char]
    val BigDecimalType: Type[BigDecimal] = Type.of[BigDecimal]
    val BigIntType: Type[BigInt] = Type.of[BigInt]
  }

  // ── Diff derivation (two values → DiffResult) ────────────

  final case class DiffCtx[A](
      tpe: Type[A],
      left: Expr[A],
      right: Expr[A],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {
    def nest[B: Type](newLeft: Expr[B], newRight: Expr[B]): DiffCtx[B] =
      DiffCtx(Type[B], newLeft, newRight, cache, derivedType)
  }

  def dctx[A](implicit A: DiffCtx[A]): DiffCtx[A] = A
  implicit def diffCtxType[A: DiffCtx]: Type[A] = dctx.tpe

  abstract class DiffDerivationRule(val name: String) extends Rule {
    def apply[A: DiffCtx]: MIO[Rule.Applicability[Expr[DiffResult]]]
  }

  @scala.annotation.nowarn("msg=is never used|unused")
  def deriveDiffRecursively[A: DiffCtx]: MIO[Expr[DiffResult]] =
    Log.namedScope(s"Deriving Diff for ${Type[A].prettyPrint}") {
      implicit val DRT: Type[DiffResult] = DiffTypes.DiffResultType
      Rules(
        DiffUseCachedRule,
        DiffUseImplicitRule,
        DiffBuiltInRule,
        DiffValueTypeRule,
        DiffOptionRule,
        DiffMapRule,
        DiffCollectionRule,
        DiffSingletonRule,
        DiffCaseClassRule,
        DiffEnumRule
      )(_[A]).flatMap {
        case Right(result) =>
          Log.info(s"Derived Diff for ${Type[A].prettyPrint}") >> MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap
            .removed(DiffUseCachedRule)
            .view
            .map { case (rule, reasons) =>
              if (reasons.isEmpty) s" - ${rule.name}: not applicable"
              else s" - ${rule.name}: ${reasons.mkString(", ")}"
            }
            .toList
          MIO.fail(DiffDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings))
      }
    }

  // ── Snapshot derivation (one value → DiffResult) ─────────

  final case class SnapshotCtx[A](
      tpe: Type[A],
      value: Expr[A],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {
    def nest[B: Type](newValue: Expr[B]): SnapshotCtx[B] =
      SnapshotCtx(Type[B], newValue, cache, derivedType)
  }

  def sctx[A](implicit A: SnapshotCtx[A]): SnapshotCtx[A] = A
  implicit def snapshotCtxType[A: SnapshotCtx]: Type[A] = sctx.tpe

  abstract class SnapshotDerivationRule(val name: String) extends Rule {
    def apply[A: SnapshotCtx]: MIO[Rule.Applicability[Expr[DiffResult]]]
  }

  @scala.annotation.nowarn("msg=is never used|unused")
  def deriveSnapshotRecursively[A: SnapshotCtx]: MIO[Expr[DiffResult]] =
    Log.namedScope(s"Deriving Snapshot for ${Type[A].prettyPrint}") {
      implicit val DRT: Type[DiffResult] = DiffTypes.DiffResultType
      Rules(
        SnapshotUseCachedRule,
        SnapshotUseImplicitRule,
        SnapshotBuiltInRule,
        SnapshotSingletonRule,
        SnapshotCaseClassRule,
        SnapshotEnumRule,
        SnapshotFallbackRule
      )(_[A]).flatMap {
        case Right(result) =>
          Log.info(s"Derived Snapshot for ${Type[A].prettyPrint}") >> MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap
            .removed(SnapshotUseCachedRule)
            .view
            .map { case (rule, reasons) =>
              if (reasons.isEmpty) s" - ${rule.name}: not applicable"
              else s" - ${rule.name}: ${reasons.mkString(", ")}"
            }
            .toList
          MIO.fail(DiffDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings))
      }
    }

  // ── Snapshot rules (inlined — simpler than diff rules) ───

  object SnapshotUseCachedRule extends SnapshotDerivationRule("use cached Snapshot") {
    def apply[A: SnapshotCtx]: MIO[Rule.Applicability[Expr[DiffResult]]] = {
      implicit val DRT: Type[DiffResult] = DiffTypes.DiffResultType
      implicit val DiffA: Type[Diff[A]] = DiffTypes.Diff[A]
      sctx.cache.get1Ary[A, DiffResult]("cached-snapshot-method").flatMap {
        case Some(helper) => MIO.pure(Rule.matched(helper(sctx.value)))
        case None         =>
          sctx.cache.get0Ary[Diff[A]]("cached-diff-instance").map {
            case Some(instance) =>
              Rule.matched(Expr.quote(Expr.splice(instance).snapshot(Expr.splice(sctx.value))))
            case None =>
              Rule.yielded(s"No cached Snapshot for ${Type[A].prettyPrint}")
          }
      }
    }
  }

  object SnapshotUseImplicitRule extends SnapshotDerivationRule("use implicit for Snapshot") {
    @scala.annotation.nowarn("msg=is never used|unused")
    def apply[A: SnapshotCtx]: MIO[Rule.Applicability[Expr[DiffResult]]] = {
      implicit val DiffA: Type[Diff[A]] = DiffTypes.Diff[A]
      implicit val DRT: Type[DiffResult] = DiffTypes.DiffResultType
      if (sctx.derivedType.exists(_.Underlying =:= Type[A]))
        MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is the self-type"))
      else
        DiffA.summonExprIgnoring(ignoredImplicits*).toEither match {
          case Right(instanceExpr) =>
            val expr = instanceExpr.asInstanceOf[Expr[Diff[A]]]
            MIO.pure(
              Rule.matched(
                Expr.quote(Expr.splice(expr).snapshot(Expr.splice(sctx.value)))
              )
            )
          case Left(_) =>
            MIO.pure(Rule.yielded(s"No implicit Diff[${Type[A].prettyPrint}]"))
        }
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused")
  object SnapshotBuiltInRule extends SnapshotDerivationRule("built-in Snapshot for primitives") {
    def apply[A: SnapshotCtx]: MIO[Rule.Applicability[Expr[DiffResult]]] = MIO {
      implicit val DRT: Type[DiffResult] = DiffTypes.DiffResultType
      implicit val ST: Type[String] = DiffTypes.StringType
      val pn = Expr(Type.prettyPrint[A])
      val fn = Expr(Type.plainPrint[A])
      val sn = Expr(Type.shortName[A])
      if (Type[A] <:< DiffTypes.StringType) {
        Rule.matched(Expr.quote {
          DiffRuntime.snapshotString(
            Expr.splice(pn),
            Expr.splice(fn),
            Expr.splice(sn),
            Expr.splice(sn),
            Expr.splice(sctx.value.upcast[String])
          )
        })
      } else if (
        Type[A] <:< DiffTypes.BooleanType || Type[A] <:< DiffTypes.ByteType ||
        Type[A] <:< DiffTypes.ShortType || Type[A] <:< DiffTypes.IntType ||
        Type[A] <:< DiffTypes.LongType || Type[A] <:< DiffTypes.FloatType ||
        Type[A] <:< DiffTypes.DoubleType || Type[A] <:< DiffTypes.CharType ||
        Type[A] <:< DiffTypes.BigDecimalType || Type[A] <:< DiffTypes.BigIntType
      ) {
        Rule.matched(Expr.quote {
          DiffResult.Identical(
            Expr.splice(pn),
            Expr.splice(fn),
            Expr.splice(sn),
            Expr.splice(sn),
            Expr.splice(sctx.value).toString
          )
        })
      } else {
        Rule.yielded(s"${Type[A].prettyPrint} is not a built-in type")
      }
    }
  }

  object SnapshotSingletonRule extends SnapshotDerivationRule("Snapshot as singleton") {
    def apply[A: SnapshotCtx]: MIO[Rule.Applicability[Expr[DiffResult]]] =
      SingletonValue.parse[A].toEither match {
        case Right(_) =>
          implicit val DRT: Type[DiffResult] = DiffTypes.DiffResultType
          val sn = Expr(Type.shortName[A])
          val pn = Expr(Type.prettyPrint[A])
          val fn = Expr(Type.plainPrint[A])
          MIO.pure(Rule.matched(Expr.quote {
            DiffResult.Identical(Expr.splice(pn), Expr.splice(fn), Expr.splice(sn), Expr.splice(sn), Expr.splice(sn))
          }))
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }
  }

  object SnapshotCaseClassRule extends SnapshotDerivationRule("Snapshot as case class") {
    @scala.annotation.nowarn("msg=is never used|unused")
    def apply[A: SnapshotCtx]: MIO[Rule.Applicability[Expr[DiffResult]]] =
      CaseClass.parse[A].toEither match {
        case Right(caseClass) =>
          implicit val DRT: Type[DiffResult] = DiffTypes.DiffResultType
          implicit val ST: Type[String] = DiffTypes.StringType
          val defBuilder = ValDefBuilder.ofDef1[A, DiffResult](s"snapshot_${Type[A].shortName}")
          for {
            _ <- sctx.cache.forwardDeclare("cached-snapshot-method", defBuilder)
            _ <- MIO.scoped { runSafe =>
              runSafe(sctx.cache.buildCachedWith("cached-snapshot-method", defBuilder) { case (_, value) =>
                runSafe(deriveSnapshotCaseClass[A](caseClass, value))
              })
            }
            result <- SnapshotUseCachedRule[A]
          } yield result
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }

    private def deriveSnapshotCaseClass[A: SnapshotCtx](
        caseClass: CaseClass[A],
        value: Expr[A]
    ): MIO[Expr[DiffResult]] = {
      implicit val DRT: Type[DiffResult] = DiffTypes.DiffResultType
      val fields = caseClass.caseFieldValuesAt(value, AtCallSite).toList
      val pn = Expr(Type.prettyPrint[A])
      val fn = Expr(Type.plainPrint[A])
      val sn = Expr(Type.shortName[A])

      hearth.fp.data.NonEmptyList.fromList(fields) match {
        case Some(fieldValues) =>
          import hearth.fp.syntax.*
          fieldValues
            .traverse { case (fieldName, fieldValue) =>
              import fieldValue.{Underlying as Field, value as fieldExpr}
              Log.namedScope(s"Snapshot field $fieldName: ${Field.prettyPrint}") {
                deriveSnapshotRecursively[Field](using sctx.nest(fieldExpr)).map(r => (fieldName, r))
              }
            }
            .map { results =>
              val fieldExprs = results.toList.map { case (name, expr) =>
                Expr.quote((Expr.splice(Expr(name)), Expr.splice(expr)))
              }
              val fieldsVec = fieldExprs.foldRight(Expr.quote(Vector.empty[(String, DiffResult)])) { (item, acc) =>
                Expr.quote(Expr.splice(item) +: Expr.splice(acc))
              }
              Expr.quote {
                DiffResult.Record(
                  Expr.splice(pn),
                  Expr.splice(fn),
                  Expr.splice(sn),
                  Expr.splice(sn),
                  Expr.splice(fieldsVec)
                )
              }
            }
        case None =>
          MIO.pure(Expr.quote {
            DiffResult.Record(
              Expr.splice(pn),
              Expr.splice(fn),
              Expr.splice(sn),
              Expr.splice(sn),
              Vector.empty[(String, DiffResult)]
            )
          })
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used|is unchecked")
  object SnapshotEnumRule extends SnapshotDerivationRule("Snapshot as enum") {
    def apply[A: SnapshotCtx]: MIO[Rule.Applicability[Expr[DiffResult]]] =
      Enum.parse[A].toEither match {
        case Right(enumm) =>
          implicit val DRT: Type[DiffResult] = DiffTypes.DiffResultType
          val defBuilder = ValDefBuilder.ofDef1[A, DiffResult](s"snapshot_${Type[A].shortName}")
          for {
            _ <- sctx.cache.forwardDeclare("cached-snapshot-method", defBuilder)
            _ <- MIO.scoped { runSafe =>
              runSafe(sctx.cache.buildCachedWith("cached-snapshot-method", defBuilder) { case (_, value) =>
                runSafe(deriveSnapshotEnum[A](enumm, value))
              })
            }
            result <- SnapshotUseCachedRule[A]
          } yield result
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }

    private def deriveSnapshotEnum[A: SnapshotCtx](
        enumm: Enum[A],
        value: Expr[A]
    ): MIO[Expr[DiffResult]] = {
      implicit val DRT: Type[DiffResult] = DiffTypes.DiffResultType
      implicit val ST: Type[String] = DiffTypes.StringType
      enumm
        .matchOn[MIO, DiffResult](value) { matchedValue =>
          import matchedValue.{value as caseValue, Underlying as EnumCase}
          deriveSnapshotRecursively[EnumCase](using sctx.nest(caseValue))
        }
        .flatMap {
          case Some(result) => MIO.pure(result)
          case None         =>
            val sn = Expr(Type.shortName[A])
            val pn = Expr(Type.prettyPrint[A])
            val fn = Expr(Type.plainPrint[A])
            MIO.pure(Expr.quote {
              DiffResult.Identical(
                Expr.splice(pn),
                Expr.splice(fn),
                Expr.splice(sn),
                Expr.splice(sn),
                Expr.splice(value).toString
              )
            })
        }
    }
  }

  object SnapshotFallbackRule extends SnapshotDerivationRule("Snapshot fallback (toString)") {
    @scala.annotation.nowarn("msg=is never used|unused")
    def apply[A: SnapshotCtx]: MIO[Rule.Applicability[Expr[DiffResult]]] = {
      implicit val DRT: Type[DiffResult] = DiffTypes.DiffResultType
      implicit val ST: Type[String] = DiffTypes.StringType
      val sn = Expr(Type.shortName[A])
      val pn = Expr(Type.prettyPrint[A])
      val fn = Expr(Type.plainPrint[A])
      MIO.pure(Rule.matched(Expr.quote {
        DiffResult.Identical(
          Expr.splice(pn),
          Expr.splice(fn),
          Expr.splice(sn),
          Expr.splice(sn),
          Expr.splice(sctx.value).toString
        )
      }))
    }
  }

  // ── Helper: summon or recursively derive Diff[Inner] ────

  @scala.annotation.nowarn("msg=is never used|unused")
  protected def summonOrDeriveDiffInstance[Inner: Type](
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ): MIO[Option[Expr[Diff[Inner]]]] = {
    implicit val DiffInnerT: Type[Diff[Inner]] = DiffTypes.Diff[Inner]
    implicit val DRT: Type[DiffResult] = DiffTypes.DiffResultType
    implicit val ST: Type[String] = DiffTypes.StringType
    DiffInnerT.summonExprIgnoring(ignoredImplicits*).toEither match {
      case Right(expr) => MIO.pure(Some(expr.asInstanceOf[Expr[Diff[Inner]]]))
      case Left(_)     =>
        cache.get0Ary[Diff[Inner]]("cached-diff-instance").flatMap {
          case Some(cached) => MIO.pure(Some(cached))
          case None         =>
            val diffKey = s"inner-diff:${Type[Inner].prettyPrint}"
            val snapKey = s"inner-snap:${Type[Inner].prettyPrint}"
            val diffDefBuilder = ValDefBuilder.ofDef2[Inner, Inner, DiffResult](s"innerDiff_${Type[Inner].shortName}")
            val snapDefBuilder = ValDefBuilder.ofDef1[Inner, DiffResult](s"innerSnap_${Type[Inner].shortName}")
            val pn = Expr(Type.prettyPrint[Inner])
            val fn = Expr(Type.plainPrint[Inner])
            val sn = Expr(Type.shortName[Inner])
            val derivation: MIO[Option[Expr[Diff[Inner]]]] = for {
              _ <- cache.forwardDeclare(diffKey, diffDefBuilder)
              _ <- cache.forwardDeclare(snapKey, snapDefBuilder)
              _ <- MIO.scoped { runSafe =>
                runSafe(cache.buildCachedWith(diffKey, diffDefBuilder) { case (_, (l, r)) =>
                  runSafe(deriveDiffRecursively[Inner](using DiffCtx[Inner](Type[Inner], l, r, cache, derivedType)))
                })
              }
              _ <- MIO.scoped { runSafe =>
                runSafe(cache.buildCachedWith(snapKey, snapDefBuilder) { case (_, v) =>
                  runSafe(
                    deriveSnapshotRecursively[Inner](using SnapshotCtx[Inner](Type[Inner], v, cache, derivedType))
                  )
                })
              }
              diffCall <- cache.get2Ary[Inner, Inner, DiffResult](diffKey)
              snapCall <- cache.get1Ary[Inner, DiffResult](snapKey)
              instanceExpr = {
                val dc = diffCall.get
                val sc = snapCall.get
                Expr
                  .quote {
                    DiffFactories.instance[Inner](
                      Expr.splice(pn),
                      Expr.splice(fn),
                      Expr.splice(sn),
                      Expr.splice(sn),
                      (left: Inner, right: Inner) => {
                        val _ = left
                        val _ = right
                        Expr.splice(dc(Expr.quote(left), Expr.quote(right)))
                      },
                      (value: Inner) => {
                        val _ = value
                        Expr.splice(sc(Expr.quote(value)))
                      }
                    )
                  }
                  .asInstanceOf[Expr[Diff[Inner]]]
              }
              _ <- cache.buildCachedWith(
                "cached-diff-instance",
                ValDefBuilder.ofLazy[Diff[Inner]](s"diffInst_${Type[Inner].shortName}")
              )(_ => instanceExpr)
              cached <- cache.get0Ary[Diff[Inner]]("cached-diff-instance")
            } yield Some(cached.get)
            derivation.recoverWith(_ => MIO.pure(None))
        }
    }
  }

  // ── Entry point ──────────────────────────────────────────

  private var nameCache: ValDefsCache = ValDefsCache.empty

  private def summonCachedDiffForName[X: Type]: Option[Expr[Diff[X]]] = {
    implicit val DiffX: Type[Diff[X]] = DiffTypes.Diff[X]
    nameCache.get0Ary[Diff[X]]("diff-for-name").orElse {
      DiffX.summonExprIgnoring(ignoredImplicits*).toOption.map { expr =>
        val typedExpr = expr.asInstanceOf[Expr[Diff[X]]]
        val builder = ValDefBuilder.ofLazy[Diff[X]](s"diffName_${Type[X].shortName}")
        nameCache = builder.buildCachedWith(nameCache, "diff-for-name")(_ => typedExpr)
        nameCache.get0Ary[Diff[X]]("diff-for-name").get
      }
    }
  }

  protected lazy val ignoredImplicits: Seq[UntypedMethod] =
    Type
      .of[Diff.type]
      .asUntyped
      .methods
      .collect { case method if method.name == "derived" => method }
      .toSeq

  protected def shouldWeLogDiffDerivation: Boolean =
    Type.of[Diff.LogDerivation].summonExprIgnoring().toOption.isDefined

  @scala.annotation.nowarn("msg=is never used|unused")
  def deriveTypeClass[A: Type]: Expr[Diff[A]] = {
    val macroName = "Diff.derived"
    implicit val DiffA: Type[Diff[A]] = DiffTypes.Diff[A]
    implicit val DRT: Type[DiffResult] = DiffTypes.DiffResultType
    implicit val ST: Type[String] = DiffTypes.StringType
    val selfType: Option[??] = Some(Type[A].as_??)

    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended."
      )

    Log
      .namedScope(s"Deriving $macroName[${Type[A].prettyPrint}] at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          val sharedCache = ValDefsCache.mlocal

          val placeholderLeft: Expr[A] = Expr.quote(null.asInstanceOf[A])
          val placeholderRight: Expr[A] = Expr.quote(null.asInstanceOf[A])
          val placeholderVal: Expr[A] = Expr.quote(null.asInstanceOf[A])

          runSafe {
            for {
              _ <- ensureStandardExtensionsLoaded()
              _ <- deriveDiffRecursively[A](using
                DiffCtx[A](Type[A], placeholderLeft, placeholderRight, sharedCache, selfType)
              ).parTuple(
                deriveSnapshotRecursively[A](using SnapshotCtx[A](Type[A], placeholderVal, sharedCache, selfType))
              )
            } yield ()
          }

          val cacheStateAfterRules = runSafe(sharedCache.get)
          val diffBuilder = ValDefBuilder.ofDef2[A, A, DiffResult](s"diff_${Type[A].shortName}")
          val snapBuilder = ValDefBuilder.ofDef1[A, DiffResult](s"snapshot_${Type[A].shortName}")
          val diffBuilt = diffBuilder.isBuilt(cacheStateAfterRules, "cached-diff-method")
          val snapBuilt = snapBuilder.isBuilt(cacheStateAfterRules, "cached-snapshot-method")

          val diffCallFor: (Expr[A], Expr[A]) => Expr[DiffResult] =
            if (diffBuilt) {
              runSafe(sharedCache.get2Ary[A, A, DiffResult]("cached-diff-method")).get
            } else {
              val key = s"entry-diff:${Type[A].prettyPrint}"
              val builder = ValDefBuilder.ofDef2[A, A, DiffResult](s"diff_${Type[A].shortName}")
              runSafe {
                MIO.scoped { rs =>
                  rs(sharedCache.buildCachedWith(key, builder) { case (_, (l, r)) =>
                    rs(deriveDiffRecursively[A](using DiffCtx[A](Type[A], l, r, sharedCache, selfType)))
                  })
                }
              }
              runSafe(sharedCache.get2Ary[A, A, DiffResult](key)).get
            }

          val snapCallFor: Expr[A] => Expr[DiffResult] =
            if (snapBuilt) {
              runSafe(sharedCache.get1Ary[A, DiffResult]("cached-snapshot-method")).get
            } else {
              val key = s"entry-snap:${Type[A].prettyPrint}"
              val builder = ValDefBuilder.ofDef1[A, DiffResult](s"snapshot_${Type[A].shortName}")
              runSafe {
                MIO.scoped { rs =>
                  rs(sharedCache.buildCachedWith(key, builder) { case (_, v) =>
                    rs(deriveSnapshotRecursively[A](using SnapshotCtx[A](Type[A], v, sharedCache, selfType)))
                  })
                }
              }
              runSafe(sharedCache.get1Ary[A, DiffResult](key)).get
            }

          nameCache = ValDefsCache.empty

          val prettyNameExpr: Expr[String] = Type.runtimePrettyPrint[A] { tpe =>
            import tpe.Underlying
            if (tpe.Underlying =:= Type[A]) None
            else
              summonCachedDiffForName[tpe.Underlying].map { d =>
                Expr.quote(Expr.splice(d).prettyName)
              }
          }

          val plainNameExpr: Expr[String] = Type.runtimePlainPrint[A] { tpe =>
            import tpe.Underlying
            if (tpe.Underlying =:= Type[A]) None
            else
              summonCachedDiffForName[tpe.Underlying].map { d =>
                Expr.quote(Expr.splice(d).plainName)
              }
          }

          val simpleNameExpr: Expr[String] = Type.runtimeShortPrint[A] { tpe =>
            import tpe.Underlying
            if (tpe.Underlying =:= Type[A]) None
            else
              summonCachedDiffForName[tpe.Underlying] match {
                case Some(d) => Some(Expr.quote(Expr.splice(d).simpleName))
                case None    =>
                  val plain = Type.plainPrint[tpe.Underlying]
                  if (plain.contains("[")) None
                  else Some(Expr(Type.shortName[tpe.Underlying]))
              }
          }

          val shortNameExpr: Expr[String] = Expr(Type.shortName[A])

          val cacheState = runSafe(sharedCache.get)
          nameCache.toValDefs.use { _ =>
            cacheState.toValDefs.use { _ =>
              Expr
                .quote {
                  DiffFactories.instance[A](
                    Expr.splice(prettyNameExpr),
                    Expr.splice(plainNameExpr),
                    Expr.splice(simpleNameExpr),
                    Expr.splice(shortNameExpr),
                    (left: A, right: A) => {
                      val _ = left
                      val _ = right
                      Expr.splice(diffCallFor(Expr.quote(left), Expr.quote(right)))
                    },
                    (value: A) => {
                      val _ = value
                      Expr.splice(snapCallFor(Expr.quote(value)))
                    }
                  )
                }
                .asInstanceOf[Expr[Diff[A]]]
            }
          }
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogDiffDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogDiffDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.diffderivation.debug.logDerivationForDiffDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }
}
