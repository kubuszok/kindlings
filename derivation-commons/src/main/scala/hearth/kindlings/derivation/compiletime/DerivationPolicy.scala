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
  * This gates ONLY the structural rules (`HandleAsCaseClass` / `HandleAsEnum`). Built-in primitives, collections,
  * `Option`, value types, named tuples, and any pre-existing in-scope implicit are reached BEFORE the structural rules
  * and are therefore always allowed - consistent with the project stance that those "always work".
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
        val enclosureFullNames = enclosingScope.toList.flatMap(_.fullName)

        DerivationPolicy.decide(
          allowedScopes = allowedScopes,
          enclosureFullNames = enclosureFullNames,
          optInByImport = optInByImport,
          markerInScope = isDerivationOptInMarkerInScope
        )
    }
  }

  /** No-op when derivation is allowed at this expansion point; otherwise fails the [[MIO]] with a
    * [[DerivationPolicy.PolicyViolation]] carrying a detailed, actionable message.
    *
    * Call this from inside the structural (case-class / enum) rules, AFTER they have matched, so that non-structural
    * types and found implicits are never gated.
    */
  protected def enforceDerivationPolicy[A: Type]: MIO[Unit] = enforceDerivationPolicyForType(Type[A].prettyPrint)

  /** Same as [[enforceDerivationPolicy]] but takes the derived type's pretty name directly, for derivations whose type
    * parameter is higher-kinded (e.g. cats-tagless `Alg[_[_]]`) and so cannot be passed to the `[A: Type]` form.
    */
  protected def enforceDerivationPolicyForType(typeName: String): MIO[Unit] = derivationPolicyDecision match {
    case DerivationPolicy.Decision.Allowed        => MIO.pure(())
    case denied: DerivationPolicy.Decision.Denied =>
      MIO.fail(new DerivationPolicy.PolicyViolation(derivationPolicyDeniedMessage(typeName, denied)))
  }

  /** Eager variant of [[enforceDerivationPolicy]] for derivations that are NOT structured as a `MIO` rule pipeline
    * (e.g. the cats polymorphic entry points): when derivation is denied, immediately abort the macro with the
    * actionable message. A no-op under the default `always-allowed` policy, so it never affects normal builds.
    *
    * `typeName` is by-name so it is only computed when the policy actually denies.
    */
  protected def enforceDerivationPolicyOrAbort(typeName: => String): Unit = derivationPolicyDecision match {
    case DerivationPolicy.Decision.Allowed        => ()
    case denied: DerivationPolicy.Decision.Denied =>
      Environment.reportErrorAndAbort(derivationPolicyDeniedMessage(typeName, denied))
  }

  private def derivationPolicyDeniedMessage(typeName: String, denied: DerivationPolicy.Decision.Denied): String = {
    val ns = derivationSettingsNamespace
    val tc = derivationPolicyTypeClassName
    val location = denied.currentScope.getOrElse("<unknown scope>")
    val position = Environment.currentPosition.prettyPrint
    val scopesLine = if (denied.allowedScopes.isEmpty) "none configured" else denied.allowedScopes.mkString(", ")
    val importOption =
      if (denied.optInByImport)
        s"""|  3. Import the opt-in marker into this scope (opt-in-by-import is enabled):
            |       $derivationOptInImportHint""".stripMargin
      else
        s"""|  3. Opt-in-by-import is disabled for this build; enable it with
            |       -Xmacro-settings:$ns.policy.optInByImport=true
            |     and then import the opt-in marker, or use one of the other options.""".stripMargin

    s"""|$tc derivation of $typeName is not allowed at this location.
        |
        |This build enables an opt-in derivation policy (-Xmacro-settings:$ns.policy.enabled=opt-in),
        |which only permits automatic (structural) derivation of case classes and sealed hierarchies
        |in designated scopes.
        |
        |  Location:         $location (at $position)
        |  Allowed scopes:   $scopesLine
        |  Opt-in by import: ${if (denied.optInByImport) "enabled" else "disabled"}
        |
        |To fix this, do ONE of the following:
        |  1. Define the instance in an allowed scope and import/inherit it here, e.g.:
        |       given $tc[$typeName] = $tc.derived
        |  2. Add this location to the allowed scopes (separate entries with ';' or '|', never ','):
        |       -Xmacro-settings:$ns.policy.allowedScopes=<package-or-object>[;<more>]
        |$importOption
        |  4. Disable the policy for this build:
        |       -Xmacro-settings:$ns.policy.enabled=always-allowed""".stripMargin
  }
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
