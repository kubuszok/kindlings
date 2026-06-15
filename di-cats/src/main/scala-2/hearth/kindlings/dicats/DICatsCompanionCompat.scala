package hearth.kindlings.dicats

import scala.language.experimental.macros
import cats.effect.kernel.Resource

private[dicats] trait DICatsCompanionCompat {

  /** Build a `Resource[F, T]` that constructs a `T` from the supplied dependencies. F-agnostic re-imagining of
    * macwire's `autocats.autowire`. See [[hearth.kindlings.dicats.DICats]] for the full description.
    */
  def wireResource[F[_], T](dependencies: Any*): Resource[F, T] =
    macro internal.compiletime.ResourceWiringMacros.wireResourceImpl[F, T]
}
