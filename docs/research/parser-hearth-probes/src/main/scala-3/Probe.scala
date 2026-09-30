//> using scala 3.8.4
//> using dep com.kubuszok::hearth:0.4.2
//> using dep io.github.iltotore::iron:3.3.2

package parserhearthprobe

import scala.quoted.*
import io.github.iltotore.iron.IronType

trait SelfReference {
  def identity: this.type
  def value: Int
  def viaSelf: Int
}

object Probe {
  inline def aliasMatches[A]: (Boolean, Boolean) = ${ aliasMatchesImpl[A] }
  inline def selfReference: SelfReference = ${ selfReferenceImpl }

  private def aliasMatchesImpl[A: Type](using q: Quotes): Expr[(Boolean, Boolean)] = {
    import q.reflect.*
    val ctx = new hearth.MacroCommonsScala3
    val ctor = ctx.Type.Ctor2.fromUntyped[IronType](TypeRepr.of[IronType].asInstanceOf[ctx.UntypedType])
    val original = TypeRepr.of[A].asInstanceOf[ctx.UntypedType].as_??
    val normalized = TypeRepr.of[A].dealias.asInstanceOf[ctx.UntypedType].as_??
    Expr((ctor.unapply(original.Underlying).isDefined, ctor.unapply(normalized.Underlying).isDefined))
  }

  private def selfReferenceImpl(using q: Quotes): Expr[SelfReference] = {
    val ctx = new hearth.MacroCommonsScala3
    import ctx.*
    val instance = AnonymousInstance.parse[SelfReference].toOption.get
    val overrides = instance.mustOverride.map { member =>
      member.method.asUntyped -> new OverrideBody {
        def apply(body: OverrideContext): Expr_?? = member.method.name match {
          case "identity" => body.self
          case "value"    => ctx.Expr(21).as_??
          case "viaSelf"  =>
            val self = body.self.value.asInstanceOf[scala.quoted.Expr[SelfReference]]
            '{ $self.value * 2 }.asInstanceOf[ctx.Expr[Int]].as_??
        }
      }
    }.toMap
    instance.construct(None, Map.empty, overrides).toOption.get.asInstanceOf[scala.quoted.Expr[SelfReference]]
  }
}
