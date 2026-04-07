package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

/** Hash derivation: combines Eq (field-by-field equality) with Hash (MurmurHash3-based hashing).
  *
  * Implementation note (Phase 1' validation rewrite):
  *
  * The previous version used `LambdaBuilder` to pre-build the `hash` and `eqv` method bodies as `Expr[A => Int]` /
  * `Expr[(A, A) => Boolean]` and splice them into a wrapping `Expr.quote { new Hash[A] { … } }`. The reason was that on
  * Scala 3 each `Expr.splice` inside the wrapping quote receives its own `Quotes` instance, so deriving inside one
  * splice produces `Expr` values tagged with that splice's `Quotes` which cannot be reused inside the sibling splice.
  *
  * The proper fix is not LambdaBuilder, but to make sure the [[ValDefsCache]] emits its `def`s **outside** any splice —
  * i.e. at `Q0`, the `MIO.scoped` setup scope — and then have each splice merely *call* those `def`s by name. We do
  * this by:
  *
  *   1. Sharing one [[ValDefsCache]] between the [[HashCtx]] and the [[EqCtx]] used for derivation.
  *   2. Running both `deriveHashRecursively` and `deriveEqRecursively` in the outer `MIO.scoped` block. The recursive
  *      rule chains (`HashCaseClassRule` etc.) populate the shared cache with `def hash_A(a: A): Int` and
  *      `def eqv_A(x: A, y: A): Boolean` entries via the same `forwardDeclare`/`buildCachedWith` pattern they already
  *      use today.
  *   3. Retrieving the cached helper-call functions via `cache.get1Ary` / `cache.get2Ary` — these construct the call
  *      tree using the [[Quotes]] active at call time, so calling them inside each splice produces an Expr correctly
  *      tagged with that splice's `Quotes`.
  *   4. Wrapping the entire `new Hash[A] { … }` with `cacheState.toValDefs.use` so the cached `def`s are emitted at
  *      `Q0`'s outermost level, lexically visible to both splices.
  *
  * No `LambdaBuilder` is involved. This is the same pattern that `circe-derivation/.../DecoderMacrosImpl.scala` already
  * uses for its two-method `KindlingsDecoder` instance (`apply` + `decodeAccumulating`).
  */
trait HashMacrosImpl
    extends EqMacrosImpl
    with rules.HashUseCachedRuleImpl
    with rules.HashUseImplicitRuleImpl
    with rules.HashBuiltInRuleImpl
    with rules.HashSingletonRuleImpl
    with rules.HashCaseClassRuleImpl
    with rules.HashEnumRuleImpl { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  def deriveHash[A: Type]: Expr[cats.kernel.Hash[A]] = {
    val macroName = "Hash.derived"
    implicit val HashA: Type[cats.kernel.Hash[A]] = HashTypes.Hash[A]
    implicit val BooleanType: Type[Boolean] = EqTypes.Boolean
    implicit val IntType: Type[Int] = HashTypes.Int
    val selfType: Option[??] = Some(Type[A].as_??)

    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended."
      )

    Log
      .namedScope(s"Deriving $macroName[${Type[A].prettyPrint}] at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          // Shared val-def cache used by BOTH Hash and Eq derivations.
          // All derivation runs in this outer Q0 scope, BEFORE the wrapping `Expr.quote`,
          // so any helper `def`s the cache emits live at Q0 — visible to both method splices.
          val sharedCache = ValDefsCache.mlocal

          // Placeholder Exprs for the HashCtx/EqCtx `value` slots. The recursive case-class /
          // enum rules forward-declare a cached def via `forwardDeclare`/`buildCachedWith`;
          // inside the build callback they receive the actual def-parameter Exprs and ignore
          // these placeholders.
          val placeholderA: Expr[A] = Expr.quote(null.asInstanceOf[A])
          val placeholderX: Expr[A] = Expr.quote(null.asInstanceOf[A])
          val placeholderY: Expr[A] = Expr.quote(null.asInstanceOf[A])

          // Step 1: Run BOTH derivations in PARALLEL via parTuple. The rule chains populate
          // `sharedCache` for case-class / enum shapes via their own `forwardDeclare` +
          // `buildCachedWith` calls under the standard keys "cached-hash-method" /
          // "cached-eq-method". For shapes that don't emit cache entries (e.g. primitives
          // via HashBuiltInRule), the cache is left untouched and we fall back to the
          // entry-point-own-def path below.
          //
          // Why parTuple instead of a sequential for-comprehension: a sequential chain
          // short-circuits on the first error, so if `deriveHashRecursively` fails the user
          // never sees the (potentially also-failing) `deriveEqRecursively` errors. Parallel
          // execution aggregates errors from BOTH derivations into a single combined report,
          // which is much more useful for users debugging an unsupported type. This is the
          // same pattern jsoniter's `deriveCombinedCodecTypeClass` uses for its 5 method
          // bodies.
          runSafe {
            for {
              _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
              _ <- deriveHashRecursively[A](using HashCtx[A](Type[A], placeholderA, sharedCache, selfType))
                .parTuple(
                  deriveEqRecursively[A](using EqCtx[A](Type[A], placeholderX, placeholderY, sharedCache, selfType))
                )
            } yield ()
          }

          // Step 2: Use `isBuilt` (NOT `getNAry returns Some`) to check whether each rule
          // actually populated a body. `getNAry` returns Some for forward-declared entries
          // even before they are built — that distinction matters during recursive self-
          // derivation, where a rule may forward-declare and then call itself inside the
          // body builder. Here we want the strict check: was the body actually populated?
          val cacheStateAfterRules = runSafe(sharedCache.get)
          val hashRuleBuilder = ValDefBuilder.ofDef1[A, Int](s"hash_${Type[A].shortName}")
          val eqRuleBuilder = ValDefBuilder.ofDef2[A, A, Boolean](s"eqv_${Type[A].shortName}")
          val hashRuleBuilt = hashRuleBuilder.isBuilt(cacheStateAfterRules, "cached-hash-method")
          val eqRuleBuilt = eqRuleBuilder.isBuilt(cacheStateAfterRules, "cached-eq-method")

          // Step 3: Resolve the helper-call function for each method. If the rule chain
          // already built it, reuse the rule's def directly. Otherwise the entry point
          // owns its own forward-declare + buildCachedWith under a unique key so the
          // wrapping `new Hash[A] { … }` always has a callable helper to splice into.
          val hashCallFor: Expr[A] => Expr[Int] =
            if (hashRuleBuilt) {
              runSafe(sharedCache.get1Ary[A, Int]("cached-hash-method")).get
            } else {
              val key = s"entry-hash:${Type[A].prettyPrint}"
              val builder = ValDefBuilder.ofDef1[A, Int](s"hash_${Type[A].shortName}")
              runSafe {
                MIO.scoped { rs =>
                  rs(sharedCache.buildCachedWith(key, builder) { case (_, valueExpr) =>
                    rs(deriveHashRecursively[A](using HashCtx[A](Type[A], valueExpr, sharedCache, selfType)))
                  })
                }
              }
              runSafe(sharedCache.get1Ary[A, Int](key)).get
            }

          val eqCallFor: (Expr[A], Expr[A]) => Expr[Boolean] =
            if (eqRuleBuilt) {
              runSafe(sharedCache.get2Ary[A, A, Boolean]("cached-eq-method")).get
            } else {
              val key = s"entry-eq:${Type[A].prettyPrint}"
              val builder = ValDefBuilder.ofDef2[A, A, Boolean](s"eqv_${Type[A].shortName}")
              runSafe {
                MIO.scoped { rs =>
                  rs(sharedCache.buildCachedWith(key, builder) { case (_, (xExpr, yExpr)) =>
                    rs(deriveEqRecursively[A](using EqCtx[A](Type[A], xExpr, yExpr, sharedCache, selfType)))
                  })
                }
              }
              runSafe(sharedCache.get2Ary[A, A, Boolean](key)).get
            }

          // Step 4: Snapshot the populated cache and wrap the outer `Expr.quote` with
          // `toValDefs.use`. This emits the cached `def`s as siblings of `new Hash[A]` at
          // Q0's outermost level — visible to both method splices.
          val cacheState = runSafe(sharedCache.get)
          cacheState.toValDefs.use { _ =>
            Expr
              .quote {
                new cats.kernel.Hash[A] {
                  def hash(value: A): Int = {
                    val _ = value
                    Expr.splice(hashCallFor(Expr.quote(value)))
                  }
                  def eqv(x: A, y: A): Boolean = {
                    val _ = x
                    val _ = y
                    Expr.splice(eqCallFor(Expr.quote(x), Expr.quote(y)))
                  }
                }
              }
              .asInstanceOf[Expr[cats.kernel.Hash[A]]]
          }
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogEqDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogEqDerivation) RenderFrom(Log.Level.Info) else DontRender
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  // Hash context — carries a single value to hash

  final case class HashCtx[A](
      tpe: Type[A],
      value: Expr[A],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??]
  ) {
    def nest[B: Type](newValue: Expr[B]): HashCtx[B] = HashCtx(Type[B], newValue, cache, derivedType)
  }
  object HashCtx {
    def from[A: Type](value: Expr[A], derivedType: Option[??]): HashCtx[A] =
      HashCtx(Type[A], value, ValDefsCache.mlocal, derivedType)
  }

  def hctx[A](implicit A: HashCtx[A]): HashCtx[A] = A
  implicit def hashCtxType[A: HashCtx]: Type[A] = hctx.tpe

  abstract class HashDerivationRule(val name: String) extends Rule {
    def apply[A: HashCtx]: MIO[Rule.Applicability[Expr[Int]]]
  }

  // Recursive derivation

  def deriveHashRecursively[A: HashCtx]: MIO[Expr[Int]] =
    Log.namedScope(s"Deriving Hash for ${Type[A].prettyPrint}") {
      Rules(
        HashUseCachedRule,
        HashUseImplicitRule,
        HashBuiltInRule,
        HashSingletonRule,
        HashCaseClassRule,
        HashEnumRule
      )(_[A]).flatMap {
        case Right(result) =>
          Log.info(s"Derived Hash for ${Type[A].prettyPrint}") >> MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap
            .removed(HashUseCachedRule)
            .view
            .map { case (rule, reasons) =>
              if (reasons.isEmpty) s"${rule.name} not applicable"
              else s" - ${rule.name}: ${reasons.mkString(", ")}"
            }
            .toList
          MIO.fail(HashDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings))
      }
    }

  protected object HashTypes {
    def Hash: Type.Ctor1[cats.kernel.Hash] = Type.Ctor1.of[cats.kernel.Hash]
    val Int: Type[Int] = Type.of[Int]
  }
}

sealed private[compiletime] trait HashDerivationError extends util.control.NoStackTrace with Product with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object HashDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends HashDerivationError {
    override def message: String = s"$tpeName not handled by any Hash rule:\n${reasons.mkString("\n")}"
  }
}
