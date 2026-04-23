package hearth.kindlings.scalacheckderivation

import org.scalacheck.{Arbitrary, Shrink}
import scala.language.experimental.macros

trait ScalaCheckDerivationScala2Extensions {

  implicit class ArbitraryDerived(private val companion: Arbitrary.type) {
    def derived[A]: Arbitrary[A] = macro internal.compiletime.ArbitraryMacros.deriveArbitraryImpl[A]
  }

  implicit class ShrinkDerived(private val companion: Shrink.type) {
    def derived[A]: Shrink[A] = macro internal.compiletime.ShrinkMacros.deriveShrinkImpl[A]
  }
}

object extensions extends ScalaCheckDerivationScala2Extensions
