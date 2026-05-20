package hearth.kindlings.diffderivation.internal.runtime

import hearth.kindlings.diffderivation.Edit

object Myers {

  def diff[A](left: IndexedSeq[A], right: IndexedSeq[A], eq: (A, A) => Boolean): Vector[Edit[A]] = {
    val n = left.length
    val m = right.length

    if (n == 0 && m == 0) return Vector.empty
    if (n == 0) return right.iterator.map(Edit.Insert(_)).toVector
    if (m == 0) return left.iterator.map(Edit.Delete(_)).toVector

    val prefixLen = commonPrefixLength(left, right, eq)
    if (prefixLen == n && prefixLen == m)
      return left.iterator.map(Edit.Equal(_)).toVector

    val suffixLen = commonSuffixLength(left, right, eq, prefixLen)

    val trimmedLeft = left.slice(prefixLen, n - suffixLen)
    val trimmedRight = right.slice(prefixLen, m - suffixLen)

    val prefix = left.iterator.take(prefixLen).map(Edit.Equal(_))
    val suffix = left.iterator.drop(n - suffixLen).map(Edit.Equal(_))

    if (trimmedLeft.isEmpty) {
      val middle = trimmedRight.iterator.map(Edit.Insert(_))
      return (prefix ++ middle ++ suffix).toVector
    }
    if (trimmedRight.isEmpty) {
      val middle = trimmedLeft.iterator.map(Edit.Delete(_))
      return (prefix ++ middle ++ suffix).toVector
    }

    val middle = diffCore(trimmedLeft, trimmedRight, eq)
    (prefix ++ middle ++ suffix).toVector
  }

  private def commonPrefixLength[A](left: IndexedSeq[A], right: IndexedSeq[A], eq: (A, A) => Boolean): Int = {
    val limit = math.min(left.length, right.length)
    var i = 0
    while (i < limit && eq(left(i), right(i))) i += 1
    i
  }

  private def commonSuffixLength[A](
      left: IndexedSeq[A],
      right: IndexedSeq[A],
      eq: (A, A) => Boolean,
      prefixLen: Int
  ): Int = {
    val limit = math.min(left.length, right.length) - prefixLen
    var i = 0
    while (i < limit && eq(left(left.length - 1 - i), right(right.length - 1 - i))) i += 1
    i
  }

  private def diffCore[A](left: IndexedSeq[A], right: IndexedSeq[A], eq: (A, A) => Boolean): Vector[Edit[A]] = {
    val n = left.length
    val m = right.length
    val max = n + m + 1
    val size = 2 * max + 1
    val mid = max

    val v = new Array[Int](size)
    val trace = new scala.collection.mutable.ArrayBuffer[Array[Int]]()

    var found = false
    var d = 0
    while (d < max && !found) {
      trace += v.clone()
      var k = -d
      while (k <= d && !found) {
        val idx = mid + k
        var x: Int =
          if (k == -d || (k != d && v(idx - 1) < v(idx + 1))) v(idx + 1)
          else v(idx - 1) + 1
        var y = x - k

        while (x < n && y < m && eq(left(x), right(y))) {
          x += 1
          y += 1
        }

        v(idx) = x

        if (x >= n && y >= m) found = true
        else k += 2
      }
      if (!found) d += 1
    }

    backtrack(left, right, trace, d)
  }

  private def backtrack[A](
      left: IndexedSeq[A],
      right: IndexedSeq[A],
      trace: scala.collection.mutable.ArrayBuffer[Array[Int]],
      d: Int
  ): Vector[Edit[A]] = {
    val max = left.length + right.length + 1
    val mid = max

    var x = left.length
    var y = right.length
    val edits = new scala.collection.mutable.ArrayBuffer[Edit[A]]()

    var step = d
    while (step > 0) {
      val vPrev = trace(step)
      val k = x - y

      val prevK: Int =
        if (k == -step || (k != step && vPrev(mid + k - 1) < vPrev(mid + k + 1))) k + 1
        else k - 1

      val prevX = vPrev(mid + prevK)
      val prevY = prevX - prevK

      val xBeforeSnake = if (prevK == k + 1) prevX else prevX + 1
      val yBeforeSnake = xBeforeSnake - k

      while (x > xBeforeSnake && y > yBeforeSnake) {
        x -= 1
        y -= 1
        edits += Edit.Equal(left(x))
      }

      if (prevK == k + 1) {
        y -= 1
        edits += Edit.Insert(right(y))
      } else {
        x -= 1
        edits += Edit.Delete(left(x))
      }

      x = prevX
      y = prevY
      step -= 1
    }

    while (x > 0 && y > 0) {
      x -= 1
      y -= 1
      edits += Edit.Equal(left(x))
    }

    edits.reverseIterator.toVector
  }
}
