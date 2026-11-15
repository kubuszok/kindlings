package hearth.kindlings.derivation.compiletime

import ValueTypeIntegrationHints.*

final class ValueTypeIntegrationHintsSpec extends hearth.MacroSuite {

  group("ValueTypeIntegrationHints.integrationFor") {

    test("recognises Iron's IronType, by either of its names, with type arguments") {
      integrationFor(List("io.github.iltotore.iron.IronType[java.lang.String, Pure]")) ==> Some(Iron)
      integrationFor(List("io.github.iltotore.iron.:|")) ==> Some(Iron)
    }

    test("recognises Iron's RefinedType/RefinedSubtype companions (by their singleton types too)") {
      integrationFor(List("app.Key.T", "app.Key.type", "io.github.iltotore.iron.RefinedType")) ==> Some(Iron)
      integrationFor(List("app.Key.T", "io.github.iltotore.iron.RefinedSubtype[java.lang.String, C]")) ==> Some(Iron)
    }

    test("recognises refined, neotype and scala-newtype") {
      integrationFor(List("eu.timepit.refined.api.Refined[java.lang.String, NonEmpty]")) ==> Some(Refined)
      integrationFor(List("app.Name.Type", "neotype.Newtype")) ==> Some(Neotype)
      integrationFor(List("app.Name.Type", "neotype.Subtype")) ==> Some(Neotype)
      integrationFor(List("app.Age.Type", ScalaNewtypeCompanionMarker)) ==> Some(Newtype)
    }

    test("ignores other types, including look-alikes in other packages") {
      integrationFor(List("app.Key.T", "scala.Any", "java.lang.Object")) ==> None
      integrationFor(List("app.neotype.Newtype", "my.RefinedType")) ==> None
      integrationFor(Nil) ==> None
    }
  }

  group("ValueTypeIntegrationHints.hintForNames") {

    test("names the type, the library and the module") {
      hintForNames("app.Key.T", List("io.github.iltotore.iron.RefinedType")) ==>
        " (app.Key.T looks like a type of Iron: add kindlings-iron-integration to the dependencies to make it a value " +
        "type wrapping its underlying type)"
    }

    test("is empty for other types") {
      hintForNames("app.Key", List("app.Key")) ==> ""
    }
  }
}
