package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.{Decoder, Encoder, HCursor, Json}

// A user's generic instance for a generic case class, whose JSON is deliberately not the structural one: it is only
// used when Kindlings passes it the instance of the type argument (an instance Kindlings has to derive itself).
final case class NuItem(n: Int)
final case class NuPage[A](items: List[A])
object NuPage {
  implicit def enc[A](implicit a: Encoder[A]): Encoder[NuPage[A]] =
    Encoder.instance(p => Json.obj("DATA" -> Json.fromValues(p.items.map(a.apply))))
  implicit def dec[A](implicit a: Decoder[A]): Decoder[NuPage[A]] =
    Decoder.instance((c: HCursor) => c.downField("DATA").as[List[A]].map(NuPage(_)))
}
// the user's instance needs two instances
final case class NuPair[A, B](a: A, b: B)
object NuPair {
  implicit def enc[A, B](implicit a: Encoder[A], b: Encoder[B]): Encoder[NuPair[A, B]] =
    Encoder.instance(p => Json.arr(Json.fromString("PAIR"), a(p.a), b(p.b)))
}
final case class NuWrapper[A](value: A)

final case class NuHoldsInts(page: NuPage[Int])
final case class NuHoldsItems(page: NuPage[NuItem])
final case class NuHoldsWrapped(page: NuPage[NuWrapper[NuItem]])
final case class NuHoldsPair(pair: NuPair[NuItem, Int], other: NuPair[NuItem, NuWrapper[Int]])
final case class NuTree(label: String, children: NuPage[NuTree])
sealed trait NuShape
object NuShape {
  final case class Group(shapes: NuPage[NuShape]) extends NuShape
  final case class Dot(n: Int) extends NuShape
}

// A user's generic instance that needs an instance Kindlings cannot provide (no `Encoder[NuNoEncoder]` exists and it
// has no Show either): the structural encoding is used, as before.
final case class NuNeedsShow[A](a: A)
object NuNeedsShow {
  trait NuShow[A] { def show(a: A): String }
  implicit def enc[A](implicit s: NuShow[A]): Encoder[NuNeedsShow[A]] =
    Encoder.instance(v => Json.fromString(s.show(v.a)))
}
final case class NuHoldsNeedsShow(value: NuNeedsShow[NuItem])

/** A user's generic instance (`implicit def enc[A](implicit a: Encoder[A]): Encoder[Page[A]]`) is used for a field even
  * when the instance it needs has to be derived by Kindlings.
  */
final class NestedUserInstanceSpec extends MacroSuite {

  private val itemJson = Json.obj("n" -> Json.fromInt(1))
  private val pageOfItemsJson = Json.obj("DATA" -> Json.arr(itemJson))

  group("a user's generic instance for a field") {

    test("is used when its dependency already exists (Page[Int])") {
      KindlingsEncoder.derived[NuHoldsInts].apply(NuHoldsInts(NuPage(List(1)))) ==>
        Json.obj("page" -> Json.obj("DATA" -> Json.arr(Json.fromInt(1))))
    }

    test("is used when its dependency has to be derived (Page[Item])") {
      KindlingsEncoder.derived[NuHoldsItems].apply(NuHoldsItems(NuPage(List(NuItem(1))))) ==>
        Json.obj("page" -> pageOfItemsJson)
    }

    test("is used by the decoder when its dependency has to be derived (Page[Item])") {
      KindlingsDecoder.derived[NuHoldsItems].decodeJson(Json.obj("page" -> pageOfItemsJson)) ==>
        Right(NuHoldsItems(NuPage(List(NuItem(1)))))
    }

    test("is used when its dependency is itself a derived generic type (Page[Wrapper[Item]])") {
      KindlingsEncoder.derived[NuHoldsWrapped].apply(NuHoldsWrapped(NuPage(List(NuWrapper(NuItem(1)))))) ==>
        Json.obj("page" -> Json.obj("DATA" -> Json.arr(Json.obj("value" -> itemJson))))
    }

    test("is used when it needs several instances, some derived and some existing") {
      KindlingsEncoder
        .derived[NuHoldsPair]
        .apply(NuHoldsPair(NuPair(NuItem(1), 2), NuPair(NuItem(3), NuWrapper(4)))) ==>
        Json.obj(
          "pair" -> Json.arr(Json.fromString("PAIR"), itemJson, Json.fromInt(2)),
          "other" -> Json.arr(
            Json.fromString("PAIR"),
            Json.obj("n" -> Json.fromInt(3)),
            Json.obj("value" -> Json.fromInt(4))
          )
        )
    }

    test("is used with the instance being derived (a recursive type)") {
      val tree = NuTree("root", NuPage(List(NuTree("leaf", NuPage(Nil)))))
      val json = Json.obj(
        "label" -> Json.fromString("root"),
        "children" -> Json.obj(
          "DATA" -> Json.arr(
            Json.obj("label" -> Json.fromString("leaf"), "children" -> Json.obj("DATA" -> Json.arr()))
          )
        )
      )
      KindlingsEncoder.derived[NuTree].apply(tree) ==> json
      KindlingsDecoder.derived[NuTree].decodeJson(json) ==> Right(tree)
    }

    test("is used inside a sealed hierarchy with the sum being derived") {
      val shape: NuShape = NuShape.Group(NuPage(List(NuShape.Dot(1))))
      KindlingsEncoder.derived[NuShape].apply(shape) ==>
        Json.obj(
          "Group" -> Json.obj(
            "shapes" -> Json.obj("DATA" -> Json.arr(Json.obj("Dot" -> Json.obj("n" -> Json.fromInt(1)))))
          )
        )
    }

    test("is used by the inline encode") {
      KindlingsEncoder.encode(NuHoldsItems(NuPage(List(NuItem(1))))) ==> Json.obj("page" -> pageOfItemsJson)
    }

    test("is not used when it needs an instance of another type class: the type is derived structurally") {
      KindlingsEncoder.derived[NuHoldsNeedsShow].apply(NuHoldsNeedsShow(NuNeedsShow(NuItem(1)))) ==>
        Json.obj("value" -> Json.obj("a" -> itemJson))
    }

    test("is used when the nested instance is already a value in scope (the workaround)") {
      implicit val item: Encoder[NuItem] = KindlingsEncoder.derived[NuItem]
      KindlingsEncoder.derived[NuHoldsItems].apply(NuHoldsItems(NuPage(List(NuItem(1))))) ==>
        Json.obj("page" -> pageOfItemsJson)
    }
  }
}
