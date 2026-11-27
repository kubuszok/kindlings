package hearth.kindlings.catstaglessderivation

import cats.arrow.FunctionK
import cats.data.{Cokleisli, EitherK, EitherT, Kleisli, OptionT, Tuple2K}
import cats.tagless.{ApplyK, ContravariantK, FunctorK, InvariantK, SemigroupalK}
import hearth.MacroSuite

import scala.util.Try

/** Sealed traits / enums of algebras, like cats-tagless-data (`tests/src/test/scala-3/cats/tagless/data/tests`,
  * cats-tagless 0.16.6, where they are Scala 3 enums deriving FunctorK / ContravariantK / InvariantK).
  */
object DataAlgebras {
  // FunctorKSuite.Tree
  sealed trait TreeK[F[_]]
  final case class LeafK[F[_]](values: F[String]) extends TreeK[F]
  final case class BranchK[F[_]](size: Int, left: TreeK[F], right: TreeK[F]) extends TreeK[F]

  // ContravariantKSuite.Fun (Choice / Switch)
  sealed trait FunK[F[_]]
  final case class ChoiceK[F[_]](name: String, choice: FunctionK[F, Option]) extends FunK[F]
  final case class SwitchK[F[_]](name: String, switch: Cokleisli[F, Boolean, String]) extends FunK[F]

  // InvariantKSuite.Codec
  sealed trait CodecK[F[_]]
  final case class DecoderK[F[_]](id: Int, f: Kleisli[F, Int, String]) extends CodecK[F]
  final case class EncoderK[F[_]](id: Int, f: Cokleisli[F, Int, String]) extends CodecK[F]

  // ApplyKSuite.Spec
  final case class SpecK[F[_]](
      replicas: Int,
      labels: F[String],
      containers: Tuple2K[F, Option, Long],
      service: EitherK[List, F, String]
  )

  // SemigroupalKSuite.Parsed
  final case class ParsedK[F[_]](
      position: Int,
      labels: F[String],
      metadata: OptionT[F, String],
      errors: EitherT[F, String, Int]
  )
}

final class DataAlgebrasSpec extends MacroSuite {
  import DataAlgebras.*

  private val tryToOption: FunctionK[Try, Option] = new FunctionK[Try, Option] {
    def apply[A](fa: Try[A]): Option[A] = fa.toOption
  }
  private val optionToTry: FunctionK[Option, Try] = new FunctionK[Option, Try] {
    def apply[A](fa: Option[A]): Try[A] = Try(fa.get)
  }

  test("FunctorK for a recursive sealed algebra (cats-tagless-data FunctorKSuite.Tree)") {
    val functorK: FunctorK[TreeK] = KindlingsFunctorK.derived[TreeK]
    val tree: TreeK[Try] = BranchK[Try](
      2,
      LeafK[Try](Try("a")),
      BranchK[Try](1, LeafK[Try](Try[String](sys.error("boom"))), LeafK[Try](Try("c")))
    )
    val expected: TreeK[Option] = BranchK[Option](
      2,
      LeafK[Option](Some("a")),
      BranchK[Option](1, LeafK[Option](None), LeafK[Option](Some("c")))
    )
    assertEquals(functorK.mapK(tree)(tryToOption), expected)
  }

  test("ContravariantK for a sealed algebra (cats-tagless-data ContravariantKSuite.Fun)") {
    val contravariantK: ContravariantK[FunK] = KindlingsContravariantK.derived[FunK]
    val choice: FunK[Option] = ChoiceK[Option]("c", FunctionK.id[Option])
    contravariantK.contramapK(choice)(tryToOption) match {
      case ChoiceK(name, f) =>
        assertEquals(name, "c")
        assertEquals(f(Try(1)), Some(1))
      case other => fail(s"unexpected $other")
    }
    val switch: FunK[Option] = SwitchK[Option]("s", Cokleisli[Option, Boolean, String]((o: Option[Boolean]) => o.fold("none")(_.toString)))
    contravariantK.contramapK(switch)(tryToOption) match {
      case SwitchK(name, f) =>
        assertEquals(name, "s")
        assertEquals(f.run(Try(true)), "true")
      case other => fail(s"unexpected $other")
    }
  }

  test("InvariantK for a sealed algebra (cats-tagless-data InvariantKSuite.Codec)") {
    val invariantK: InvariantK[CodecK] = KindlingsInvariantK.derived[CodecK]
    val decoder: CodecK[Try] = DecoderK[Try](1, Kleisli[Try, Int, String]((i: Int) => Try(i.toString)))
    invariantK.imapK(decoder)(tryToOption)(optionToTry) match {
      case DecoderK(id, f) =>
        assertEquals(id, 1)
        assertEquals(f.run(5), Some("5"))
      case other => fail(s"unexpected $other")
    }
    val encoder: CodecK[Try] = EncoderK[Try](2, Cokleisli[Try, Int, String]((t: Try[Int]) => t.fold(_ => "error", _.toString)))
    invariantK.imapK(encoder)(tryToOption)(optionToTry) match {
      case EncoderK(id, f) =>
        assertEquals(id, 2)
        assertEquals(f.run(Some(7)), "7")
      case other => fail(s"unexpected $other")
    }
  }

  test("ApplyK for a case-class algebra with F nested in Tuple2K / EitherK (cats-tagless-data ApplyKSuite.Spec)") {
    val applyK: ApplyK[SpecK] = KindlingsApplyK.derived[SpecK]
    val spec = SpecK[Try](3, Try("l"), Tuple2K[Try, Option, Long](Try(1L), Some(2L)), EitherK.rightc[List, Try, String](Try("s")))
    val mapped = applyK.mapK(spec)(tryToOption)
    assertEquals(mapped.replicas, 3)
    assertEquals(mapped.labels, Some("l"))
    assertEquals(mapped.containers.first, Some(1L))
    assertEquals(mapped.containers.second, Some(2L))
    assertEquals(mapped.service.run, Right(Some("s")))
    val product = applyK.productK(spec, mapped)
    assertEquals(product.labels.first, Try("l"))
    assertEquals(product.labels.second, Some("l"))
  }

  test("SemigroupalK and InvariantK for a case-class algebra with OptionT / EitherT fields (SemigroupalKSuite.Parsed)") {
    val semigroupalK: SemigroupalK[ParsedK] = KindlingsSemigroupalK.derived[ParsedK]
    val invariantK: InvariantK[ParsedK] = KindlingsInvariantK.derived[ParsedK]
    val parsed = ParsedK[Option](1, Some("a"), OptionT.some[Option]("m"), EitherT.rightT[Option, String](2))
    val product = semigroupalK.productK(parsed, parsed)
    assertEquals(product.position, 1)
    assertEquals(product.labels.second, Some("a"))
    assertEquals(product.metadata.value.first, Some(Some("m")))
    assertEquals(product.errors.value.second, Some(Right(2)))
    val imapped = invariantK.imapK(parsed)(optionToTry)(tryToOption)
    assertEquals(imapped.metadata.value, Try(Some("m")))
    assertEquals(imapped.errors.value, Try(Right(2)))
  }
}
