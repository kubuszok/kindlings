package hearth.kindlings.neotypeintegration.internal.compiletime

import hearth.fp.data.NonEmptyList
import hearth.{MacroCommons, MacroCommonsScala3}
import hearth.std.{ProviderResult, StandardMacroExtension, StdExtensions}

/** Teaches every Kindlings derivation to treat a [[https://github.com/kitlangton/neotype neotype]] `Newtype`/`Subtype`
  * as a value type: unwrap to the underlying type on encoding, validate and re-wrap with the companion's `make` on
  * decoding.
  *
  * neotype exposes its `Newtype.WithType` witness as a `transparent inline given`, which is invisible to the macro-time
  * implicit search performed by the "use implicit when available" rules - so a generic `given [A, B](using
  * Newtype.WithType[B, A], ...)` fallback would resolve at a normal call site but never inside a derivation macro. A
  * value-type provider sidesteps implicit search entirely by matching the type structurally at macro-expansion time.
  *
  * Registered with priority 1000, so it runs before Hearth's built-in opaque-type provider (priority -1000), which
  * would otherwise match the neotype's opaque `Type` and wrap it without running `validate`.
  */
final class IsValueTypeProviderForNeotype extends StandardMacroExtension { loader =>

  override def priority: Int = 1000

  override def extend(ctx: MacroCommons & StdExtensions): Unit = ctx match {
    case ctx3: MacroCommonsScala3 =>
      val reflection = new NeotypeReflection(ctx3)
      import ctx.*

      IsValueType.registerProvider(new IsValueType.Provider {

        override def name: String = loader.getClass.getName

        // Cheap sound pre-filter: every neotype `Type` is an opaque type, so a non-opaque type can never match.
        override def mightMatch[A](tpe: Type[A]): Boolean = tpe.isOpaqueType

        @scala.annotation.nowarn("msg=is never used")
        override def parse[A](tpe: Type[A]): ProviderResult[IsValueType[A]] =
          reflection.parseNeotype(UntypedType.fromTyped(using tpe).asInstanceOf[reflection.TypeRepr]) match {
            case Some(neotype) =>
              val inner = neotype.underlying.asInstanceOf[UntypedType].as_??
              import inner.Underlying as Inner
              implicit val AT: Type[A] = tpe

              // Unwrap: an opaque type has the same runtime representation as its underlying type.
              val unwrapExpr: Expr[A] => Expr[Inner] =
                outerExpr => Expr.quote(Expr.splice(outerExpr).asInstanceOf[Inner])

              // NOT implicit to avoid Type.of bootstrap cycle
              val eitherType: Type[Either[String, A]] = Type.of[Either[String, A]]

              // Wrap: `Companion.make(value)` runs the (possibly overridden) `validate` and returns
              // `Either[String, Companion.Type]`, i.e. `Either[String, A]`.
              val eitherCtor = CtorLikeOf.EitherStringOrValue[Inner, A](
                ctor = innerExpr => {
                  val innerTerm = UntypedExpr.fromTyped(innerExpr).asInstanceOf[reflection.Term]
                  val makeCall = neotype.make(innerTerm).asInstanceOf[UntypedExpr]
                  UntypedExpr.toTyped[Either[String, A]](makeCall)(using eitherType)
                },
                method = None
              )

              ProviderResult.Matched(
                Existential[IsValueTypeOf[A, *], Inner](
                  new IsValueTypeOf[A, Inner] {
                    override val unwrap: Expr[A] => Expr[Inner] = unwrapExpr
                    override val wrap: CtorLikeOf[Inner, A] = eitherCtor
                    override lazy val ctors: CtorLikes[A] = NonEmptyList.one(
                      Existential[CtorLikeOf[*, A], Inner](eitherCtor)
                    )
                  }
                )
              )

            case None => skippedLazily(s"${tpe.prettyPrint} is not a neotype Newtype/Subtype")
          }
      })
    case _ => ()
  }
}

/** Scala 3 reflection needed to recognize a neotype and to call its companion's `make`. */
final private class NeotypeReflection(val ctx: MacroCommonsScala3) {
  import ctx.quotes.reflect.*

  type TypeRepr = ctx.quotes.reflect.TypeRepr
  type Term = ctx.quotes.reflect.Term

  /** A recognized `object Foo extends Newtype[A]`/`Subtype[A]`: the underlying `A` and `Foo.make(_)`. */
  final class Neotype(val underlying: TypeRepr, companion: Term) {
    def make(value: Term): Term = Select.unique(companion, "make").appliedTo(value)
  }

  // Base classes declaring neotype's opaque `Type` member.
  private val neotypeBaseNames = Set("neotype.Newtype", "neotype.Subtype")

  /** A neotype `object Foo extends Newtype[A]` defines `Foo.Type`: an opaque type *declared in* the neotype base class,
    * whose prefix is the companion `Foo`.
    *
    * The underlying type is the base class' type argument seen from the companion - exactly one level, so that a
    * neotype over another neotype keeps the inner validation.
    */
  def parseNeotype(repr: TypeRepr): Option[Neotype] = repr.dealias match {
    case tpe: TypeRef =>
      val sym = tpe.typeSymbol
      val owner = sym.owner
      if (!sym.flags.is(Flags.Opaque) || !neotypeBaseNames(owner.fullName)) None
      else
        for {
          companion <- tpe.qualifier match {
            case termRef: TermRef => Some(Ref.term(termRef))
            // Referenced from within the companion's own body (`this.Type`).
            case thisType if thisType.typeSymbol.flags.is(Flags.Module) =>
              Some(Ref(thisType.typeSymbol.companionModule))
            case _ => None
          }
          underlying <- tpe.qualifier.widen.baseType(owner).typeArgs.headOption
        } yield new Neotype(underlying, companion)
    case _ => None
  }
}
