package hearth.kindlings.integrationtests

import hearth.MacroSuite
import io.circe.{Encoder, Json}

final case class EpItem(n: Int)
final case class EpPage[A](items: List[A])
object EpPage {
  // a user's instance; its JSON is deliberately not the structural one
  implicit def enc[A](implicit a: Encoder[A]): Encoder[EpPage[A]] =
    Encoder.instance(p => Json.obj("DATA" -> Json.fromValues(p.items.map(a.apply))))
}
final case class EpHoldsItems(page: EpPage[EpItem])
final case class EpHoldsCrashing(value: nested.CrashingInstance)

/** A derivation started from a library's own auto-derivation entry point (an implicit macro calling Kindlings), nested
  * in another one: the user's generic instance of a field gets its dependency from the entry point (the expansions of
  * the entry point are independent Hearth runs, which failed with "MIO timeout is already set" before Hearth 985e5e6).
  */
final class NestedDerivationEntryPointSpec extends MacroSuite {

  private val page = EpPage(List(EpItem(1)))
  private val pageJson = Json.obj("DATA" -> Json.arr(Json.obj("n" -> Json.fromInt(1))))

  group("circe: a library's entry point (Exported) built on KindlingsEncoder.derived") {

    test("Page[Item] at the root uses the user's instance") {
      import nested.NestedCirceAuto.*
      implicitly[Encoder[EpPage[EpItem]]].apply(page) ==> pageJson
    }

    test("a field Page[Item] of a type derived through the entry point uses the user's instance") {
      import nested.NestedCirceAuto.*
      implicitly[Encoder[EpHoldsItems]].apply(EpHoldsItems(page)) ==> Json.obj("page" -> pageJson)
    }
  }

  group("an implicit whose macro crashes while it is searched") {

    test("fails the derivation with the reason (instead of deriving the type structurally)") {
      val errors = compileErrors(
        """
        import hearth.kindlings.circederivation.KindlingsEncoder
        KindlingsEncoder.derived[hearth.kindlings.integrationtests.EpHoldsCrashing]
        """
      )
      errors.check("the instance macro crashed")
      // Scala 2 reports the exception of a candidate's macro itself, Scala 3 only tells why the search failed
      if (nested.ScalaMajor.isScala3)
        errors.check(
          "The implicit search for io.circe.Encoder[hearth.kindlings.integrationtests.nested.CrashingInstance] failed"
        )
    }
  }
}
