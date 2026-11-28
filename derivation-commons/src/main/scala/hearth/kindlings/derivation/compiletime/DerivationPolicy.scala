package hearth.kindlings.derivation.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*

/** Per-library, settings-driven gate controlling '''where''' automatic (structural) derivation of case classes and
  * sealed hierarchies is allowed to happen.
  *
  * Motivation (issue kubuszok/kindlings#85): rather than splitting derivation into "automatic" vs "semi-automatic"
  * mechanisms (and rather than making recursion optional), Kindlings keeps a single derivation mechanism and lets the
  * build '''globally opt out''' of automatic derivation. When the policy is `opt-in`, structural derivation is only
  * permitted in designated scopes (or behind an explicit import marker); everywhere else the macro fails with an
  * actionable message telling the user to define the instance explicitly (e.g. `given X = TypeClass.derived`) in an
  * allowed place.
  *
  * Enforced once per macro expansion by a root rule placed AFTER the "use implicit" and "use cached def" rules and
  * BEFORE every derivation rule: a pre-existing in-scope implicit is used without gating (it is not derivation), and a
  * cached recursive type already passed the check at the first level. When permitted at the first (outermost) level all
  * nested derivations are permitted too; when denied, the macro fails fast before any nesting.
  *
  * Reuses the same per-module settings namespace as [[DerivationTimeout]] (`derivationSettingsNamespace`).
  * Configuration keys (all under `<namespace>.policy.`):
  *
  * {{{
  * -Xmacro-settings:<ns>.policy.enabled=opt-in            // or always-allowed (default => current behavior)
  * -Xmacro-settings:<ns>.policy.allowedScopes=com.acme.json;com.acme.Codecs
  * -Xmacro-settings:<ns>.policy.optInByImport=true        // default true
  * }}}
  *
  * '''Why `;`/`|` and not `,` for `allowedScopes`''': the Scala 3 compiler splits a single `-Xmacro-settings:a,b`
  * option on commas into `List("a","b")`, while Scala 2 keeps it as one `"a,b"` string - so a comma is not portable.
  * `;` and `|` are never split by either compiler, so the value arrives intact and we split it ourselves. (Indexed keys
  * would also work but are noisier.)
  */
trait DerivationPolicy { this: MacroCommons =>
  // $COVERAGE-OFF$

  /** Shared with [[DerivationTimeout]] - the per-module settings namespace (e.g. `fastShowPrettyDerivation`). */
  protected def derivationSettingsNamespace: String

  /** Human-readable type class name used in the denial message (e.g. `"FastShowPretty"`). */
  protected def derivationPolicyTypeClassName: String

  /** Whether the per-module opt-in marker implicit is in scope. Each module implements this as
    * `Expr.summonImplicit[<TypeClass>.AllowDerivation].isDefined`.
    */
  protected def isDerivationOptInMarkerInScope: Boolean

  /** A copy-pasteable `import ...` line shown in the denial message when opt-in-by-import is enabled (e.g.
    * `"import hearth.kindlings.fastshowpretty.policy.allowDerivationForFastShowPretty"`).
    */
  protected def derivationOptInImportHint: String

  /** Computed once per macro expansion. On the default `always-allowed` path this short-circuits before touching
    * [[Environment.enclosingScope]] or summoning the marker implicit, keeping the common path cheap.
    */
  protected lazy val derivationPolicyDecision: DerivationPolicy.Decision = {
    val ns = derivationSettingsNamespace

    val policyData = for {
      data <- Environment.typedSettings.toOption
      nsData <- data.get(ns)
      policy <- nsData.get("policy")
    } yield policy

    val mode = policyData.flatMap(_.get("enabled")).flatMap(_.asString) match {
      case None      => DerivationPolicy.Mode.AlwaysAllowed
      case Some(raw) =>
        DerivationPolicy.parseMode(raw).getOrElse {
          Environment.reportWarn(
            s"$ns.policy.enabled: unrecognized value '$raw'. " +
              s"Expected 'always-allowed' or 'opt-in'. Using 'always-allowed'."
          )
          DerivationPolicy.Mode.AlwaysAllowed
        }
    }

    mode match {
      case DerivationPolicy.Mode.AlwaysAllowed => DerivationPolicy.Decision.Allowed
      case DerivationPolicy.Mode.OptIn         =>
        val allowedScopes =
          policyData
            .flatMap(_.get("allowedScopes"))
            .flatMap(_.asString)
            .map(DerivationPolicy.splitScopes)
            .getOrElse(Nil)
        val optInByImport =
          policyData.flatMap(_.get("optInByImport")).flatMap(_.asBoolean).getOrElse(true)
        // Read without forcing any type (`enclosingScope` lists members and local values, which fails with "illegal
        // cyclic reference" while the type of the `val` holding this expansion is being inferred).
        val enclosureFullNames = EnclosingOwnerNames(this).getOrElse(enclosingScope.toList.flatMap(_.fullName))

        DerivationPolicy.decide(
          allowedScopes = allowedScopes,
          enclosureFullNames = enclosureFullNames,
          optInByImport = optInByImport,
          markerInScope = isDerivationOptInMarkerInScope
        )
    }
  }

  private var derivationPolicyChecked = false

  /** The single policy check, run at most once per macro expansion. Yields `()` when derivation is permitted here
    * (recording, the first time, that the check passed so all nested derivations skip it); fails the [[MIO]] with a
    * [[DerivationPolicy.PolicyViolation]] when denied.
    *
    * Intended to be the body of a per-module root rule placed AFTER the "use implicit" and "use cached def" rules and
    * BEFORE every derivation rule: using an existing implicit is not derivation (so it is never gated), and anything
    * found in the cache already passed this check at the first level. The rule yields, so the actual derivation rules
    * run next; a denial fails fast before any nesting.
    */
  protected def checkDerivationPolicyOncePerExpansion(typeName: => String): MIO[Unit] = MIO.pure(()).flatMap { _ =>
    if (derivationPolicyChecked) MIO.pure(())
    else
      derivationPolicyDecision match {
        case DerivationPolicy.Decision.Allowed =>
          derivationPolicyChecked = true
          MIO.pure(())
        case denied: DerivationPolicy.Decision.Denied =>
          MIO.fail(new DerivationPolicy.PolicyViolation(derivationPolicyDeniedMessage(typeName, denied)))
      }
  }

  /** [[checkDerivationPolicyOncePerExpansion]] that gates only '''structural''' derivation: when `A` is a built-in, a
    * collection or map, an `Option`, an `Either`, a value type, a tuple or a named tuple, the check is skipped (and NOT
    * recorded as passed), so the derivation proceeds and the policy is checked when (if) an element of `A` needs
    * structural derivation - e.g. `derived[List[User]]` at a denied site compiles when an instance of `User` is in
    * scope, and is denied (for `User`) when it is not.
    */
  protected def checkDerivationPolicyOncePerExpansionFor[A: Type]: MIO[Unit] =
    MIO.pure(()).flatMap { _ =>
      if (derivationPolicyChecked) MIO.pure(())
      // the default (always-allowed) path never pays for the type analysis
      else if (derivationPolicyDecision == DerivationPolicy.Decision.Allowed || isStructuralDerivationTarget[A])
        checkDerivationPolicyOncePerExpansion(Type[A].prettyPrint)
      else MIO.pure(())
    }

  /** Whether deriving `A` is structural derivation (a case class, a case object, a sealed hierarchy or an enum) that is
    * not also handled as one of "the parts that always work" (collection, `Option`, `Either`, value type, tuple).
    */
  protected def isStructuralDerivationTarget[A: Type]: Boolean = {
    val structural = Type[A].isCase || Type[A].isCaseObject || Enum.parse[A].toOption.isDefined
    structural && !Type[A].isTuple && !Type[A].isNamedTuple && (this match {
      case std: hearth.std.StdExtensions =>
        // `this` is the platform MacroCommons, which always mixes in StdExtensions: same `Type` at runtime.
        val tpe = Type[A].asInstanceOf[std.Type[A]]
        std.IsOption.unapply(tpe).isEmpty && std.IsEither.unapply(tpe).isEmpty &&
        std.IsCollection.unapply(tpe).isEmpty && std.IsValueType.unapply(tpe).isEmpty
      case _ => true
    })
  }

  /** Eager variant for derivations that are NOT structured as a `MIO` rule pipeline (the cats polymorphic entry
    * points): when derivation is denied, immediately abort the macro with the actionable message. A no-op under the
    * default `always-allowed` policy. `typeName` is by-name so it is only computed when the policy actually denies.
    */
  protected def enforceDerivationPolicyOrAbort(typeName: => String): Unit = derivationPolicyDecision match {
    case DerivationPolicy.Decision.Allowed        => ()
    case denied: DerivationPolicy.Decision.Denied =>
      Environment.reportErrorAndAbort(derivationPolicyDeniedMessage(typeName, denied))
  }

  private def derivationPolicyDeniedMessage(typeName: String, denied: DerivationPolicy.Decision.Denied): String = {
    val target = s"$derivationPolicyTypeClassName[$typeName]"

    val base =
      if (denied.allowedScopes.nonEmpty)
        s"""Derivation of $target is enabled only in the following scopes:
           |${denied.allowedScopes.map(" - " + _).mkString("\n")}
           |
           |Currently you are in the following scope: ${denied.currentScope.getOrElse("<unknown>")}.""".stripMargin
      else
        s"Derivation of $target is globally disabled."

    if (denied.optInByImport)
      base +
        s"""|
            |
            |You are allowed to enable this derivation locally by adding the import:
            |$derivationOptInImportHint""".stripMargin
    else base
  }
  // $COVERAGE-ON$
}

object DerivationPolicy {

  /** Parsed value of `<ns>.policy.enabled`. */
  sealed trait Mode extends Product with Serializable
  object Mode {
    case object AlwaysAllowed extends Mode
    case object OptIn extends Mode
  }

  /** Outcome of evaluating the policy for the current macro-expansion point. */
  sealed trait Decision extends Product with Serializable
  object Decision {
    case object Allowed extends Decision
    final case class Denied(currentScope: Option[String], allowedScopes: List[String], optInByImport: Boolean)
        extends Decision
  }

  /** Thrown (via `MIO.fail`) when structural derivation is attempted outside an allowed scope. Carries the actionable
    * message; rendered through each module's existing macro error path.
    */
  final class PolicyViolation(message: String) extends RuntimeException(message) with scala.util.control.NoStackTrace

  def parseMode(raw: String): Option[Mode] = raw.trim.toLowerCase match {
    case "always-allowed" | "always-allow" | "always" | "allowed" => Some(Mode.AlwaysAllowed)
    case "opt-in" | "optin"                                       => Some(Mode.OptIn)
    case _                                                        => None
  }

  /** Splits an `allowedScopes` setting value on `;` or `|` (NOT `,` - see [[DerivationPolicy]] for why), trimming and
    * dropping empty entries.
    */
  def splitScopes(raw: String): List[String] =
    raw.split("[;|]").iterator.map(_.trim).filter(_.nonEmpty).toList

  /** Package-prefix-aware scope match: an `allowed` entry matches `fullName` when it is equal to it, or is a
    * dotted-segment / member prefix of it (so a package entry covers everything nested under it, and an object/class
    * entry covers its inner derivations). Boundaries recognized: `.`, `#`, `$`.
    */
  def scopeMatches(allowed: String, fullName: String): Boolean = {
    val a = allowed.trim
    a.nonEmpty && (fullName == a ||
      fullName.startsWith(a + ".") ||
      fullName.startsWith(a + "#") ||
      fullName.startsWith(a + "$"))
  }

  /** Whether an owner's full name is worth matching/showing: drops the root and empty packages and compiler-generated
    * owners (anonymous classes and functions, local dummies, macro-expansion wrappers).
    */
  def isUserVisibleOwnerName(fullName: String): Boolean =
    fullName.nonEmpty && fullName != "<root>" && fullName != "<empty>" && !fullName.contains("$anon") &&
      !fullName.contains("<local ") && !fullName.endsWith("$macro") && !fullName.endsWith(".macro")

  def scopeAllows(allowedScopes: List[String], enclosureFullNames: List[String]): Boolean =
    allowedScopes.exists(a => enclosureFullNames.exists(fn => scopeMatches(a, fn)))

  /** Pure core of the decision (unit-tested directly). `markerInScope` is by-name so the (non-free) marker-implicit
    * summon is only forced when the scope check has already failed and opt-in-by-import is enabled.
    */
  def decide(
      allowedScopes: List[String],
      enclosureFullNames: List[String],
      optInByImport: Boolean,
      markerInScope: => Boolean
  ): Decision =
    if (scopeAllows(allowedScopes, enclosureFullNames)) Decision.Allowed
    else if (optInByImport && markerInScope) Decision.Allowed
    else Decision.Denied(enclosureFullNames.headOption, allowedScopes, optInByImport)
}
