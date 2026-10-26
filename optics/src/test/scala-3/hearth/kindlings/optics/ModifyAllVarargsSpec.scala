package hearth.kindlings.optics

import hearth.MacroSuite

/** Scala 3 receives `modifyAll`'s extra paths as an `Expr[Seq[...]]` (Scala 2 as a `Seq[c.Expr[...]]`), so only on
  * Scala 3 can a caller splat a non-literal sequence (`paths*`). The bridge must reject that rather than silently drop
  * the splatted paths — which is what Hearth's `VarArgs.toIterable` would do (it returns `Nil` for a non-literal
  * varargs; see docs/research/hearth-gap-varargs-non-literal.md). Lives in `src/test/scala-3` because the varargs
  * encoding is a platform difference.
  */
final class ModifyAllVarargsSpec extends MacroSuite {

  group("modifyAll with splatted (non-literal) varargs") {

    test("a splatted path sequence is rejected instead of silently ignored") {
      compileErrors(
        """
        import hearth.kindlings.optics.*
        import hearth.kindlings.optics.ModifyAllVarargsSpec.*
        val more: Seq[OpticsContext ?=> (Names => String)] = Seq(_.second)
        Names("a", "b").modifyAll(_.first, more*).setTo("x")
        """
      ).check(
        "`modifyAll` expects an explicit list of paths"
      )
    }
  }
}

object ModifyAllVarargsSpec {

  final case class Names(first: String, second: String)
}
