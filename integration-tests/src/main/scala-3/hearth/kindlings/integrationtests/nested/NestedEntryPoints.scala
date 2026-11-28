package hearth.kindlings.integrationtests.nested

import hearth.kindlings.circederivation.{Configuration, KindlingsEncoder}
import io.circe.Encoder
import io.circe.`export`.Exported

import scala.deriving.Mirror
import scala.quoted.*

// Fixtures that have to be compiled before the tests that use them (macros).

/** A library's own auto-derivation entry point built on Kindlings, on circe's low-priority `Exported` hook. */
object NestedCirceAuto {
  implicit inline def auto[A](using inline m: Mirror.Of[A]): Exported[Encoder[A]] =
    Exported(KindlingsEncoder.derived[A](using Configuration()))
}

/** A type whose implicit encoder is a macro that crashes (throws an exception). */
final case class CrashingInstance(n: Int)
object CrashingInstance {
  implicit inline def encoder: Encoder[CrashingInstance] = ${ CrashingInstanceMacros.crash }
}

object CrashingInstanceMacros {
  def crash(using Quotes): Expr[Encoder[CrashingInstance]] =
    throw new IllegalStateException("the instance macro crashed")
}

/** Whether the fixtures are compiled with Scala 3 (some expectations differ). */
object ScalaMajor {
  val isScala3: Boolean = true
}
