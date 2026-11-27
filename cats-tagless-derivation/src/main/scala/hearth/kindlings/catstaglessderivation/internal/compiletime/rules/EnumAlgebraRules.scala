package hearth.kindlings.catstaglessderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories

/** Sealed traits / Scala 3 enums of algebras (`enum Tree[F[_]] { case Leaf(values: F[String]); case Branch(size: Int,
  * left: Tree[F], right: Tree[F]) }`), like cats-tagless-data's `derives FunctorK` / `ContravariantK` / `InvariantK`:
  * the value's subtype is transformed like a case-class algebra. The instance is cached (forward-declared) before the
  * subtypes are derived, so subtypes referring to the sealed algebra itself use it.
  */
trait EnumAlgebraSupport { this: InvariantKMacrosImpl & MacroCommons & StdExtensions =>

  /** `af match { case c: Child[F] => childBody(Child, c) ... }` for a sealed algebra `Alg[F]`. */
  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  protected def enumAlgebraBody[Alg[_[_]]](
      typeClass: String,
      algCtorK1: Type.CtorK1[Alg],
      afExpr: Expr[Alg[CatsTaglessFactories.WCtor1]]
  )(
      childBody: (Type.CtorK1[CatsTaglessFactories.DummyHKT], Expr[Any]) => MIO[Expr[Any]]
  ): MIO[Expr[Alg[CatsTaglessFactories.WCtor2]]] = {
    // `WCtor1` and `WCtor2` are both `[A] =>> Any`: one implicit `Type` serves both applications.
    val AlgW1: Type[Alg[CatsTaglessFactories.WCtor1]] = algCtorK1.apply[CatsTaglessFactories.WCtor1](using mkWCtor1)
    implicit val AlgW2: Type[Alg[CatsTaglessFactories.WCtor2]] =
      algCtorK1.apply[CatsTaglessFactories.WCtor2](using mkWCtor2)
    parsedOrFail(Enum.parse(using AlgW1).toEither, "Alg[WCtor1]").flatMap { enumm =>
      enumm
        .matchOn[MIO, Alg[CatsTaglessFactories.WCtor2]](afExpr.asInstanceOf[Expr[Alg[CatsTaglessFactories.WCtor1]]]) { matched =>
          import matched.{value as childValue, Underlying as Child}
          val childType = UntypedType.dealias(Type[Child].asUntyped)
          UntypedType.typeArguments(childType) match {
            case _ :: Nil =>
              val childCtor =
                Type.CtorK1.fromUntyped[CatsTaglessFactories.DummyHKT](UntypedType.typeConstructor(childType))
              // The Expr keeps the subtype's static type (only the Scala-level wrapper is cast).
              // The subtype's transformed value (e.g. `Leaf[G]`) conforms to `Alg[G]`.
              childBody(childCtor, childValue.asInstanceOf[Expr[Any]])
                .map(_.asInstanceOf[Expr[Alg[CatsTaglessFactories.WCtor2]]])
            case _ =>
              failCannotConstruct[Expr[Alg[CatsTaglessFactories.WCtor2]]](
                typeClass,
                s"subtype ${Type[Child].prettyPrint} of a sealed algebra has to take the algebra's effect as its " +
                  "only type parameter"
              )
          }
        }
        .flatMap {
          case Some(result) => MIO.pure(result)
          case None => failCannotConstruct(typeClass, s"sealed algebra ${AlgW1.prettyPrint} has no subtypes")
        }
    }
  }

  protected def isEnumAlgebra[Alg[_[_]]](algCtorK1: Type.CtorK1[Alg]): Either[String, Unit] = {
    val AlgW1 = algCtorK1.apply[CatsTaglessFactories.WCtor1](using mkWCtor1)
    if (CaseClass.parse(using AlgW1).toEither.isRight) Left(s"${AlgW1.prettyPrint} is a case class")
    else Enum.parse(using AlgW1).toEither.map(_ => ()).left.map(_.toString)
  }
}

trait FunctorKEnumRuleImpl { this: FunctorKMacrosImpl & MacroCommons & StdExtensions =>

  object FunctorKEnumRule extends FunctorKDerivationRule("FunctorK as sealed trait / enum") {
    def apply[Alg[_[_]]: FunctorKCtx]: MIO[Rule.Applicability[Expr[cats.tagless.FunctorK[Alg]]]] =
      isEnumAlgebra(fkctx.algCtorK1) match {
        case Right(_) =>
          implicit val FKAlgType: Type[cats.tagless.FunctorK[Alg]] = fkctx.functorKAlgType
          for {
            _ <- MIO.scoped { runSafe =>
              runSafe {
                fkctx.cache.buildCachedWith(
                  "cached-functork-instance",
                  ValDefBuilder.ofLazy[cats.tagless.FunctorK[Alg]](s"functorK_${fkctx.functorKAlgType.shortName}")
                )(_ => buildFactory[Alg](runSafe))
              }
            }
            result <- FunctorKUseCachedRule[Alg]
          } yield result
        case Left(reason) => MIO.pure(Rule.yielded(reason))
      }

    /** Derives a subtype's body (a method, not a lambda inside the quote: Scala 2 cross-quotes type its parameters). */
    private def childBodyFor[Alg[_[_]]](fk: Expr[Any])(implicit
        ctx: FunctorKCtx[Alg]
    ): (Type.CtorK1[CatsTaglessFactories.DummyHKT], Expr[Any]) => MIO[Expr[Any]] = {
      val anchor = Type.of[cats.tagless.FunctorK[CatsTaglessFactories.DummyHKT]].asInstanceOf[Type[Any]]
      (childCtor, child) => {
        val childCtx = FunctorKCtx[CatsTaglessFactories.DummyHKT](
          childCtor,
          applyTypeClassToAlgebra[Any, cats.tagless.FunctorK[CatsTaglessFactories.DummyHKT]](anchor, childCtor.asUntyped),
          ctx.cache,
          ctx.derivedType
        )
        deriveFunctorKCaseClassBody[CatsTaglessFactories.DummyHKT](
          child.asInstanceOf[Expr[CatsTaglessFactories.DummyHKT[CatsTaglessFactories.WCtor1]]],
          fk
        )(childCtx).map(_.asInstanceOf[Expr[Any]])
      }
    }

    @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
    private def buildFactory[Alg[_[_]]](
        runSafe: hearth.fp.DirectStyle.RunSafe[hearth.fp.effect.MIO]
    )(implicit ctx: FunctorKCtx[Alg]): Expr[cats.tagless.FunctorK[Alg]] = {
      implicit val AlgCtorK1: Type.CtorK1[Alg] = ctx.algCtorK1
      implicit val FKAlgType: Type[cats.tagless.FunctorK[Alg]] = ctx.functorKAlgType
      Expr.quote {
        CatsTaglessFactories.functorKInstance[Alg] {
          (
              af: Alg[CatsTaglessFactories.WCtor1],
              fk: cats.arrow.FunctionK[CatsTaglessFactories.WCtor1, CatsTaglessFactories.WCtor2]
          ) =>
            val _ = af
            val _ = fk
            Expr.splice {
              runSafe {
                enumAlgebraBody[Alg]("FunctorK", ctx.algCtorK1, Expr.quote(af))(childBodyFor[Alg](Expr.quote(fk).asInstanceOf[Expr[Any]]))
              }
            }
        }
      }
    }
  }
}

trait ContravariantKEnumRuleImpl { this: ContravariantKMacrosImpl & MacroCommons & StdExtensions =>

  object ContravariantKEnumRule extends ContravariantKDerivationRule("ContravariantK as sealed trait / enum") {
    def apply[Alg[_[_]]: ContravariantKCtx]: MIO[Rule.Applicability[Expr[cats.tagless.ContravariantK[Alg]]]] =
      isEnumAlgebra(ckctx.algCtorK1) match {
        case Right(_) =>
          implicit val CKAlgType: Type[cats.tagless.ContravariantK[Alg]] = ckctx.contravariantKAlgType
          for {
            _ <- MIO.scoped { runSafe =>
              runSafe {
                ckctx.cache.buildCachedWith(
                  "cached-contravariantk-instance",
                  ValDefBuilder.ofLazy[cats.tagless.ContravariantK[Alg]](
                    s"contravariantK_${ckctx.contravariantKAlgType.shortName}"
                  )
                )(_ => buildFactory[Alg](runSafe))
              }
            }
            result <- ContravariantKUseCachedRule[Alg]
          } yield result
        case Left(reason) => MIO.pure(Rule.yielded(reason))
      }

    /** Derives a subtype's body (a method, not a lambda inside the quote: Scala 2 cross-quotes type its parameters). */
    private def childBodyFor[Alg[_[_]]](fk: Expr[Any])(implicit
        ctx: ContravariantKCtx[Alg]
    ): (Type.CtorK1[CatsTaglessFactories.DummyHKT], Expr[Any]) => MIO[Expr[Any]] = {
      val anchor = Type.of[cats.tagless.ContravariantK[CatsTaglessFactories.DummyHKT]].asInstanceOf[Type[Any]]
      (childCtor, child) => {
        val childCtx = ContravariantKCtx[CatsTaglessFactories.DummyHKT](
          childCtor,
          applyTypeClassToAlgebra[Any, cats.tagless.ContravariantK[CatsTaglessFactories.DummyHKT]](anchor, childCtor.asUntyped),
          ctx.cache,
          ctx.derivedType
        )
        deriveContravariantKCaseClassBody[CatsTaglessFactories.DummyHKT](
          child.asInstanceOf[Expr[CatsTaglessFactories.DummyHKT[CatsTaglessFactories.WCtor1]]],
          fk
        )(childCtx).map(_.asInstanceOf[Expr[Any]])
      }
    }

    @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
    private def buildFactory[Alg[_[_]]](
        runSafe: hearth.fp.DirectStyle.RunSafe[hearth.fp.effect.MIO]
    )(implicit ctx: ContravariantKCtx[Alg]): Expr[cats.tagless.ContravariantK[Alg]] = {
      implicit val AlgCtorK1: Type.CtorK1[Alg] = ctx.algCtorK1
      implicit val CKAlgType: Type[cats.tagless.ContravariantK[Alg]] = ctx.contravariantKAlgType
      Expr.quote {
        CatsTaglessFactories.contravariantKInstance[Alg] {
          (
              af: Alg[CatsTaglessFactories.WCtor1],
              fk: cats.arrow.FunctionK[CatsTaglessFactories.WCtor2, CatsTaglessFactories.WCtor1]
          ) =>
            val _ = af
            val _ = fk
            Expr.splice {
              runSafe {
                enumAlgebraBody[Alg]("ContravariantK", ctx.algCtorK1, Expr.quote(af))(childBodyFor[Alg](Expr.quote(fk).asInstanceOf[Expr[Any]]))
              }
            }
        }
      }
    }
  }
}

trait InvariantKEnumRuleImpl { this: InvariantKMacrosImpl & MacroCommons & StdExtensions =>

  object InvariantKEnumRule extends InvariantKDerivationRule("InvariantK as sealed trait / enum") {
    def apply[Alg[_[_]]: InvariantKCtx]: MIO[Rule.Applicability[Expr[cats.tagless.InvariantK[Alg]]]] =
      isEnumAlgebra(ikctx.algCtorK1) match {
        case Right(_) =>
          implicit val IKAlgType: Type[cats.tagless.InvariantK[Alg]] = ikctx.invariantKAlgType
          for {
            _ <- MIO.scoped { runSafe =>
              runSafe {
                ikctx.cache.buildCachedWith(
                  "cached-invariantk-instance",
                  ValDefBuilder.ofLazy[cats.tagless.InvariantK[Alg]](s"invariantK_${ikctx.invariantKAlgType.shortName}")
                )(_ => buildFactory[Alg](runSafe))
              }
            }
            result <- InvariantKUseCachedRule[Alg]
          } yield result
        case Left(reason) => MIO.pure(Rule.yielded(reason))
      }

    /** Derives a subtype's body (a method, not a lambda inside the quote: Scala 2 cross-quotes type its parameters). */
    private def childBodyFor[Alg[_[_]]](fk: Expr[Any], gk: Expr[Any])(implicit
        ctx: InvariantKCtx[Alg]
    ): (Type.CtorK1[CatsTaglessFactories.DummyHKT], Expr[Any]) => MIO[Expr[Any]] = {
      val anchor = Type.of[cats.tagless.InvariantK[CatsTaglessFactories.DummyHKT]].asInstanceOf[Type[Any]]
      (childCtor, child) => {
        val childCtx = InvariantKCtx[CatsTaglessFactories.DummyHKT](
          childCtor,
          applyTypeClassToAlgebra[Any, cats.tagless.InvariantK[CatsTaglessFactories.DummyHKT]](anchor, childCtor.asUntyped),
          ctx.cache,
          ctx.derivedType
        )
        deriveInvariantKCaseClassBody[CatsTaglessFactories.DummyHKT](
          child.asInstanceOf[Expr[CatsTaglessFactories.DummyHKT[CatsTaglessFactories.WCtor1]]],
          fk,
          gk
        )(childCtx).map(_.asInstanceOf[Expr[Any]])
      }
    }

    @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
    private def buildFactory[Alg[_[_]]](
        runSafe: hearth.fp.DirectStyle.RunSafe[hearth.fp.effect.MIO]
    )(implicit ctx: InvariantKCtx[Alg]): Expr[cats.tagless.InvariantK[Alg]] = {
      implicit val AlgCtorK1: Type.CtorK1[Alg] = ctx.algCtorK1
      implicit val IKAlgType: Type[cats.tagless.InvariantK[Alg]] = ctx.invariantKAlgType
      Expr.quote {
        CatsTaglessFactories.invariantKInstance[Alg] {
          (
              af: Alg[CatsTaglessFactories.WCtor1],
              fk: cats.arrow.FunctionK[CatsTaglessFactories.WCtor1, CatsTaglessFactories.WCtor2],
              gk: cats.arrow.FunctionK[CatsTaglessFactories.WCtor2, CatsTaglessFactories.WCtor1]
          ) =>
            val _ = af
            val _ = fk
            val _ = gk
            Expr.splice {
              runSafe {
                enumAlgebraBody[Alg]("InvariantK", ctx.algCtorK1, Expr.quote(af))(childBodyFor[Alg](Expr.quote(fk).asInstanceOf[Expr[Any]], Expr.quote(gk).asInstanceOf[Expr[Any]]))
              }
            }
        }
      }
    }
  }
}
