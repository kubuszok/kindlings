package hearth.kindlings.policytest

import hearth.kindlings.fastshowpretty.RenderConfig
import hearth.kindlings.policytest.model.{Allowed1, Imported1}

/** End-to-end tests for the derivation policy (issue kubuszok/kindlings#85).
  *
  * This module's `Test / scalacOptions` set:
  * {{{
  * -Xmacro-settings:fastShowPrettyDerivation.policy.enabled=opt-in
  * -Xmacro-settings:fastShowPrettyDerivation.policy.allowedScopes=hearth.kindlings.policytest.allowed
  * -Xmacro-settings:fastShowPrettyDerivation.policy.optInByImport=true
  * }}}
  *
  * This spec lives in `hearth.kindlings.policytest` (NOT an allowed scope) and does NOT import the opt-in marker, so
  * any structural derivation triggered directly from here is denied. It therefore uses the instances pre-derived in the
  * allowed / opt-in-by-import scopes, and asserts the denial only via `compileErrors`.
  */
final class DerivationPolicyIntegrationSpec extends hearth.MacroSuite {

  private def render[A](instance: hearth.kindlings.fastshowpretty.FastShowPretty[A], value: A): String =
    instance.render(new StringBuilder, RenderConfig.Default, 0)(value).toString

  group("derivation policy: opt-in") {

    test("allows structural derivation inside a configured allowed scope") {
      // `allowed.AllowedInstances` compiled only because the policy permitted derivation in its package.
      val out = render(allowed.AllowedInstances.instance, Allowed1(1, "x"))
      assert(out.contains("Allowed1"), s"unexpected rendering: $out")
    }

    test("allows structural derivation behind the opt-in import marker") {
      val out = render(viaimport.ImportedInstances.instance, Imported1(7))
      assert(out.contains("Imported1"), s"unexpected rendering: $out")
    }

    test("denies structural derivation outside allowed scopes and without the import marker") {
      compileErrors(
        """hearth.kindlings.fastshowpretty.FastShowPretty.derived[hearth.kindlings.policytest.model.Denied1]"""
      ).check("is not allowed at this location")
    }
  }

  // Each library is gated independently (its own namespace). These prove the gate fires in real codec modules
  // (rule-based encoder/decoder/codec shapes), not just FastShowPretty. The module configures circe/jsoniter as
  // `opt-in` with no allowed scope, and this spec imports no opt-in marker, so derivation here is denied.
  group("derivation policy: per-library gating") {

    test("circe: structural derivation is denied in a non-allowed scope") {
      compileErrors(
        """hearth.kindlings.circederivation.KindlingsEncoder.derived[hearth.kindlings.policytest.model.Denied1]"""
      ).check("is not allowed at this location")
    }

    test("jsoniter: structural derivation is denied in a non-allowed scope") {
      compileErrors(
        """hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[hearth.kindlings.policytest.model.Denied1]"""
      ).check("is not allowed at this location")
    }
  }
}
