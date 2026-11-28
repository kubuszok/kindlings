package hearth.kindlings.integrationtests.nested

import io.circe.Encoder
import io.circe.`export`.Exported

import scala.language.experimental.macros
import scala.reflect.macros.{blackbox, whitebox}

// Fixtures that have to be compiled before the tests that use them (Scala 2 macros).

/** A library's own auto-derivation entry point built on Kindlings, on circe's low-priority `Exported` hook. */
object NestedCirceAuto {
  implicit def auto[A]: Exported[Encoder[A]] = macro NestedEntryPointMacros.circeAuto[A]
}

class NestedEntryPointMacros(val c: whitebox.Context) {
  import c.universe.*

  private def caseClassOrAbort[A: c.WeakTypeTag]: Type = {
    val A = weakTypeOf[A]
    if (!(A.typeSymbol.isClass && A.typeSymbol.asClass.isCaseClass)) c.abort(c.enclosingPosition, "not a case class")
    A
  }

  def circeAuto[A: c.WeakTypeTag]: Tree = {
    val A = caseClassOrAbort[A]
    q"""_root_.io.circe.`export`.Exported[_root_.io.circe.Encoder[$A]](
          _root_.hearth.kindlings.circederivation.KindlingsEncoder.derived[$A](
            _root_.hearth.kindlings.circederivation.Configuration()))"""
  }
}

/** A type whose implicit encoder is a macro that crashes (throws an exception). */
final case class CrashingInstance(n: Int)
object CrashingInstance {
  implicit def encoder: Encoder[CrashingInstance] = macro CrashingInstanceMacros.crash
}

class CrashingInstanceMacros(val c: blackbox.Context) {
  def crash: c.Tree = throw new IllegalStateException("the instance macro crashed")
}

/** Whether the fixtures are compiled with Scala 3 (some expectations differ). */
object ScalaMajor {
  val isScala3: Boolean = false
}
