package hearth.kindlings.optics.upstream

import scala.reflect.ClassTag

/** Base of the suites in this package: quicklens' own test suite (1.9.15), run against Kindlings' optics.
  *
  * The suites are ported verbatim: the import (`hearth.kindlings.optics._` instead of `com.softwaremill.quicklens._`)
  * and the test framework are the only differences. This trait provides the few ScalaTest spellings they use (`it
  * should "..." in { ... }`, `a should be(b)`, `a shouldBe b`, `an[E] should be thrownBy { ... }`, ...) on top of
  * munit, so that the bodies of the tests stay as upstream wrote them and can be diffed against a newer quicklens.
  *
  * Not ported (see `docs/user-guide/feature-parity.md`, optics): the suites for user-defined `QuicklensFunctor`s
  * (Kindlings' extension point is Hearth's `IsCollection`/`IsMap`/`IsOption` providers), Scala 3 extension-method
  * `copy`/accessors (`ExtensionCopyTest`), literal-typed focuses (`LiteralTypeTest`) and the compilation-time check.
  */
trait QuicklensSuite extends hearth.MacroSuite {

  // `it should "name" in { body }`
  protected object it {
    def should(name: String): InWord = new InWord(name)
  }
  final protected class InWord(name: String) {
    def in(body: => Any)(implicit loc: munit.Location): Unit = test(name) { val _ = body }
  }

  final protected class BeMatcher(val expected: Any)
  final protected class ContainMatcher(val element: Any)
  protected object be {
    def apply(expected: Any): BeMatcher = new BeMatcher(expected)
  }
  protected def contain(element: Any): ContainMatcher = new ContainMatcher(element)

  // ScalaTest's default equality compares arrays structurally, also with other sequences.
  private def normalized(value: Any): Any = value match {
    case array: Array[?]              => array.toList.map(normalized)
    case seq: scala.collection.Seq[?] => seq.toList.map(normalized)
    case other                        => other
  }
  private def assertSame(actual: Any, expected: Any)(implicit loc: munit.Location): Unit =
    assert(normalized(actual) == normalized(expected), s"$actual was not equal to $expected")

  implicit final protected class ShouldOps(private val actual: Any) {
    def should(matcher: BeMatcher)(implicit loc: munit.Location): Unit = assertSame(actual, matcher.expected)
    def should(matcher: ContainMatcher)(implicit loc: munit.Location): Unit = actual match {
      case map: scala.collection.Map[?, ?] =>
        assert(map.toList.contains(matcher.element), s"$map did not contain ${matcher.element}")
      case it: scala.collection.Iterable[?] =>
        assert(it.toList.contains(matcher.element), s"$it did not contain ${matcher.element}")
      case other => fail(s"$other is not a collection")
    }
    def shouldBe(expected: Any)(implicit loc: munit.Location): Unit = assertSame(actual, expected)
    def shouldEqual(expected: Any)(implicit loc: munit.Location): Unit = assertSame(actual, expected)
  }

  // `an[E] should be thrownBy { body }`
  final protected class AnWord[E <: Throwable](tag: ClassTag[E]) {
    def should(word: be.type): ThrownBy[E] = new ThrownBy[E](tag)
  }
  final protected class ThrownBy[E <: Throwable](tag: ClassTag[E]) {
    def thrownBy(body: => Any)(implicit loc: munit.Location): Unit = {
      val _ = intercept[E](body)(tag, loc)
    }
  }
  protected def an[E <: Throwable](implicit tag: ClassTag[E]): AnWord[E] = new AnWord[E](tag)
}
