package hearth.kindlings.derivation.compiletime

import hearth.kindlings.derivation.compiletime.DerivationPolicy.{Decision, Mode}

final class DerivationPolicySpec extends hearth.MacroSuite {

  group("DerivationPolicy.parseMode") {

    test("parses always-allowed (and aliases), case-insensitively") {
      DerivationPolicy.parseMode("always-allowed") ==> Some(Mode.AlwaysAllowed)
      DerivationPolicy.parseMode("ALWAYS-ALLOWED") ==> Some(Mode.AlwaysAllowed)
      DerivationPolicy.parseMode("  always  ") ==> Some(Mode.AlwaysAllowed)
      DerivationPolicy.parseMode("allowed") ==> Some(Mode.AlwaysAllowed)
    }

    test("parses opt-in (and aliases), case-insensitively") {
      DerivationPolicy.parseMode("opt-in") ==> Some(Mode.OptIn)
      DerivationPolicy.parseMode("OPT-IN") ==> Some(Mode.OptIn)
      DerivationPolicy.parseMode("optin") ==> Some(Mode.OptIn)
    }

    test("returns None for unrecognized values") {
      DerivationPolicy.parseMode("disabled") ==> None
      DerivationPolicy.parseMode("") ==> None
      DerivationPolicy.parseMode("nonsense") ==> None
    }
  }

  group("DerivationPolicy.splitScopes") {

    test("splits on ';'") {
      DerivationPolicy.splitScopes("com.acme.json;com.acme.Codecs") ==> List("com.acme.json", "com.acme.Codecs")
    }

    test("splits on '|'") {
      DerivationPolicy.splitScopes("com.acme.json|com.acme.Codecs") ==> List("com.acme.json", "com.acme.Codecs")
    }

    test("splits on a mix of ';' and '|', trimming whitespace and dropping empties") {
      DerivationPolicy.splitScopes(" com.acme.json ; | com.acme.Codecs ;; ") ==>
        List("com.acme.json", "com.acme.Codecs")
    }

    test("a single scope yields a one-element list") {
      DerivationPolicy.splitScopes("com.acme.json") ==> List("com.acme.json")
    }

    test("does NOT split on ',' (a comma stays part of the value)") {
      DerivationPolicy.splitScopes("com.acme.a,com.acme.b") ==> List("com.acme.a,com.acme.b")
    }
  }

  group("DerivationPolicy.scopeMatches") {

    test("matches an exact fully-qualified name") {
      DerivationPolicy.scopeMatches("com.acme.Codecs", "com.acme.Codecs") ==> true
    }

    test("matches a dotted-segment (package) prefix") {
      DerivationPolicy.scopeMatches("com.acme.json", "com.acme.json.UserCodecs") ==> true
    }

    test("matches a member ('#') and module ('$') prefix") {
      DerivationPolicy.scopeMatches("com.acme.Codecs", "com.acme.Codecs#Inner") ==> true
      DerivationPolicy.scopeMatches("com.acme.Codecs", "com.acme.Codecs$") ==> true
    }

    test("does NOT match a mere string prefix that is not a segment boundary") {
      DerivationPolicy.scopeMatches("com.acme.js", "com.acme.json") ==> false
    }

    test("does NOT match an unrelated scope, nor an empty allowed entry") {
      DerivationPolicy.scopeMatches("com.acme.json", "com.other") ==> false
      DerivationPolicy.scopeMatches("", "com.acme.json") ==> false
    }
  }

  group("DerivationPolicy.scopeAllows") {

    test("true when any allowed entry matches any enclosure full name") {
      DerivationPolicy.scopeAllows(
        List("com.other", "com.acme.json"),
        List("com.acme.json.UserCodecs", "com.acme.json", "com.acme")
      ) ==> true
    }

    test("false when no allowed entry matches and when there are no allowed entries") {
      DerivationPolicy.scopeAllows(List("com.acme.json"), List("com.other.Foo")) ==> false
      DerivationPolicy.scopeAllows(Nil, List("com.acme.json.Foo")) ==> false
    }
  }

  group("DerivationPolicy.decide") {

    test("allows when the scope matches, WITHOUT evaluating the marker") {
      var markerEvaluated = false
      val decision = DerivationPolicy.decide(
        allowedScopes = List("com.acme.json"),
        enclosureFullNames = List("com.acme.json.UserCodecs"),
        optInByImport = true,
        markerInScope = { markerEvaluated = true; true }
      )
      decision ==> Decision.Allowed
      markerEvaluated ==> false
    }

    test("allows out-of-scope when opt-in-by-import is enabled and the marker is present") {
      DerivationPolicy.decide(
        allowedScopes = List("com.acme.json"),
        enclosureFullNames = List("com.other.Foo"),
        optInByImport = true,
        markerInScope = true
      ) ==> Decision.Allowed
    }

    test("denies out-of-scope when the marker is absent") {
      DerivationPolicy.decide(
        allowedScopes = List("com.acme.json"),
        enclosureFullNames = List("com.other.Foo"),
        optInByImport = true,
        markerInScope = false
      ) ==> Decision.Denied(Some("com.other.Foo"), List("com.acme.json"), optInByImport = true)
    }

    test("denies out-of-scope when opt-in-by-import is disabled, even if the marker is present") {
      DerivationPolicy.decide(
        allowedScopes = List("com.acme.json"),
        enclosureFullNames = List("com.other.Foo"),
        optInByImport = false,
        markerInScope = true
      ) ==> Decision.Denied(Some("com.other.Foo"), List("com.acme.json"), optInByImport = false)
    }

    test("Denied carries the innermost enclosure as the current scope (or None when unknown)") {
      DerivationPolicy.decide(Nil, Nil, optInByImport = false, markerInScope = false) ==>
        Decision.Denied(None, Nil, optInByImport = false)
    }
  }
}
