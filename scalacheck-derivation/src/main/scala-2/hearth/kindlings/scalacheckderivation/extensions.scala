package hearth.kindlings.scalacheckderivation

import org.scalacheck.{Arbitrary, Cogen, Shrink}
import scala.language.experimental.macros

trait ScalaCheckDerivationScala2Extensions {

  implicit class ArbitraryDerived(private val companion: Arbitrary.type) {
    def derived[A]: Arbitrary[A] = macro internal.compiletime.ArbitraryMacros.deriveArbitraryImpl[A]
  }

  implicit class ShrinkDerived(private val companion: Shrink.type) {
    def derived[A]: Shrink[A] = macro internal.compiletime.ShrinkMacros.deriveShrinkImpl[A]
  }

  implicit class CogenDerived(private val companion: Cogen.type) {
    def derived[A]: Cogen[A] = macro internal.compiletime.CogenMacros.deriveCogenImpl[A]
  }
}

object extensions extends ScalaCheckDerivationScala2Extensions
