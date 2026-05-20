package hearth.kindlings.diffderivation

import hearth.kindlings.diffderivation.internal.runtime.Myers

final class MyersSpec extends hearth.MacroSuite {

  private val eq: (String, String) => Boolean = _ == _
  private val eqI: (Int, Int) => Boolean = _ == _

  group("Myers.diff") {

    test("empty sequences") {
      assertEquals(Myers.diff(IndexedSeq.empty[String], IndexedSeq.empty[String], eq), Vector.empty)
    }

    test("identical sequences") {
      val s = IndexedSeq("a", "b", "c")
      val result = Myers.diff(s, s, eq)
      assertEquals(result, Vector(Edit.Equal("a"), Edit.Equal("b"), Edit.Equal("c")))
    }

    test("all inserts") {
      val result = Myers.diff(IndexedSeq.empty[String], IndexedSeq("a", "b"), eq)
      assertEquals(result, Vector(Edit.Insert("a"), Edit.Insert("b")))
    }

    test("all deletes") {
      val result = Myers.diff(IndexedSeq("a", "b"), IndexedSeq.empty[String], eq)
      assertEquals(result, Vector(Edit.Delete("a"), Edit.Delete("b")))
    }

    test("single insert in middle") {
      val result = Myers.diff(IndexedSeq("a", "c"), IndexedSeq("a", "b", "c"), eq)
      assertEquals(result, Vector(Edit.Equal("a"), Edit.Insert("b"), Edit.Equal("c")))
    }

    test("single delete in middle") {
      val result = Myers.diff(IndexedSeq("a", "b", "c"), IndexedSeq("a", "c"), eq)
      assertEquals(result, Vector(Edit.Equal("a"), Edit.Delete("b"), Edit.Equal("c")))
    }

    test("replace element") {
      val result = Myers.diff(IndexedSeq("a", "b", "c"), IndexedSeq("a", "x", "c"), eq)
      val hasDelete = result.exists { case Edit.Delete("b") => true; case _ => false }
      val hasInsert = result.exists { case Edit.Insert("x") => true; case _ => false }
      assert(hasDelete, "expected Delete(b)")
      assert(hasInsert, "expected Insert(x)")
    }

    test("common prefix/suffix optimization") {
      val left = IndexedSeq(1, 2, 3, 4, 5)
      val right = IndexedSeq(1, 2, 99, 4, 5)
      val result = Myers.diff(left, right, eqI)
      assertEquals(result.head, Edit.Equal(1))
      assertEquals(result(1), Edit.Equal(2))
      assertEquals(result.last, Edit.Equal(5))
    }

    test("complex interleaved edits") {
      val left = IndexedSeq("a", "b", "c", "d")
      val right = IndexedSeq("a", "x", "c", "y")
      val result = Myers.diff(left, right, eq)
      val equalCount = result.count { case Edit.Equal(_) => true; case _ => false }
      assert(equalCount == 2, s"expected 2 equals, got $equalCount in $result")
    }
  }
}
