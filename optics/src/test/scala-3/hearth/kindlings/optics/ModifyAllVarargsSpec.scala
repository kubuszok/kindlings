package hearth.kindlings.optics

import hearth.MacroSuite

/** Scala 3 receives `modifyAll`'s extra paths as an `Expr[Seq[...]]` (Scala 2 as a `Seq[c.Expr[...]]`). The bridge must
  * reject a splatted non-literal sequence (`paths*`) rather than silently drop the splatted paths, which is what
  * Hearth's `VarArgs.toIterable` would do (it returns `Nil` for a non-literal varargs on Scala 3); the shared
  * implementation uses `VarArgs.toIterableOption` instead (see docs/research/hearth-gap-varargs-non-literal.md). This
  * spec covers the Scala 3-only `paths*` syntax; `ModifyAllSplatSpec` covers the cross-platform `paths: _*` one.
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
