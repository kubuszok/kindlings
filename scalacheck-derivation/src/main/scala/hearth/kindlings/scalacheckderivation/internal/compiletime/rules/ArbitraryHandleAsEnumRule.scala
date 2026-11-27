package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import org.scalacheck.Gen
import hearth.kindlings.derivation.compiletime.StableChildrenOrder

trait ArbitraryHandleAsEnumRuleImpl { this: ArbitraryMacrosImpl & MacroCommons & StdExtensions =>

  object ArbitraryHandleAsEnumRule extends ArbitraryDerivationRule("handle as enum when possible") {
    def apply[A: ArbitraryCtx]: MIO[Rule.Applicability[Expr[Gen[A]]]] =
      Enum.parse[A].toEither match {
        case Right(enumData) =>
          Log.info(s"Handling ${Type[A].prettyPrint} as enum") >>
            deriveEnumArbitrary[A](enumData).map(Rule.matched(_))
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason))
      }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveEnumArbitrary[A: ArbitraryCtx](enumData: Enum[A]): MIO[Expr[Gen[A]]] = {
      implicit val GenA: Type[Gen[A]] = ArbitraryTypes.Gen[A]

      // The children, with nested sealed traits flattened into their subtypes (each subtype once, even when reachable
      // through several sealed traits), like scalacheck-derived: every leaf has the same chance, and the size goes down
      // once per sealed hierarchy, not once per nesting level.
      val childrenList = StableChildrenOrder.inStableOrder(ArbitraryHandleAsEnumRuleImpl.this)(
        // (the members of a union type are types of their own, e.g. `Option[String] | List[Int]`: never flattened)
        if (Type.isUnionType[A]) enumData.directChildren.toList
        else flattenNestedSealed[A](enumData.directChildren.toList, 0)
      )

      childrenList match {
        case Nil =>
          val err = ArbitraryDerivationError.EnumHasNoCases(Type[A].prettyPrint)
          Log.error(err.message) >> MIO.fail(err)
        case child :: children =>
          // Derive Gen for each enum case
          NonEmptyList(child, children)
            .parTraverse { case (_, enumChild) =>
              import enumChild.Underlying as CaseType
              Log.namedScope(s"Deriving Arbitrary for enum case ${CaseType.prettyPrint}") {
                deriveArbitraryRecursively[CaseType](using arbctx.nest[CaseType]).map { caseGen =>
                  Expr.quote(Expr.splice(caseGen).asInstanceOf[org.scalacheck.Gen[A]])
                }
              }
            }
            .map { caseGens =>
              val oneOfExpr = oneOf[A](caseGens.toList)
              // At size 0 only the cases that cannot reach `A` again are generated, so recursive types terminate
              // without a `Recursive`/`RecursionFallback` marker (when every case recurses, all of them are used).
              val nonRecursive = caseGens.toList.zip(childrenList).collect {
                case (gen, (_, child)) if !mentionsType(child.Underlying.as_??, Type[A].as_??, Set.empty, 0) => gen
              }
              val atZeroExpr =
                if (nonRecursive.isEmpty || nonRecursive.size == caseGens.toList.size) oneOfExpr
                else oneOf[A](nonRecursive)
              // `Either` and `Try` that cannot contain themselves: exactly ScalaCheck's `arbEither`/`arbTry` (what both
              // upstream derivations use), and union types: scalacheck-derived-extras' `arbUnion` - both pick a case
              // without touching the size.
              // (`Try`'s subtypes are `Failure`, `Success` by name and by declaration, ScalaCheck picks `Success` first.)
              if ((isEitherOrTry[A] || Type.isUnionType[A]) && nonRecursive.size == caseGens.toList.size)
                if (isTry[A]) oneOf[A](caseGens.toList.reverse) else oneOfExpr
              else
                Expr.quote {
                  _root_.org.scalacheck.Gen.sized { n =>
                    if (n <= 0) Expr.splice(atZeroExpr)
                    else _root_.org.scalacheck.Gen.resize(n - 1, Expr.splice(oneOfExpr))
                  }
                }
            }
      }
    }

    /** Replaces every nested sealed trait without an `Arbitrary` of its own in scope by its subtypes, dropping
      * duplicates (a subtype reachable through several sealed traits).
      */
    private def flattenNestedSealed[A: Type](children: List[(String, ??<:[A])], depth: Int): List[(String, ??<:[A])] = {
      val flattened = children.flatMap { case entry @ (_, child) =>
        import child.Underlying as C
        val nested =
          if (depth > 16 || !Type[C].isSealed || Type.isUnionType[C] || CaseClass.parse[C].toOption.isDefined) None
          else if (Type[C] match { case IsOption(_) | IsCollection(_) | IsMap(_) => true; case _ => false }) None
          else if (
            ArbitraryTypes
              .Arbitrary[C]
              .summonExprIgnoring(ArbitraryUseImplicitRule.ignoredImplicits*)
              .toOption
              .isDefined
          )
            None
          else Enum.parse[C].toOption
        nested match {
          case Some(nestedEnum) =>
            flattenNestedSealed[C](nestedEnum.directChildren.toList, depth + 1).asInstanceOf[List[(String, ??<:[A])]]
          case None => List(entry)
        }
      }
      // (by name and type: e.g. the values of a Scala 2 `Enumeration` all have the same type)
      flattened.foldLeft(List.empty[(String, ??<:[A])]) { case (acc, entry @ (name, child)) =>
        import child.Underlying as C
        if (acc.exists { case (seenName, seen) => seenName == name && seen.Underlying =:= Type[C] }) acc
        else acc :+ entry
      }
    }

    private def isTry[A: Type]: Boolean = {
      implicit val TryType: Type[scala.util.Try[Any]] = Type.of[scala.util.Try[Any]]
      Type.isSubtypeOf[A, scala.util.Try[Any]]
    }

    private def isEitherOrTry[A: Type]: Boolean = {
      implicit val EitherType: Type[Either[Any, Any]] = Type.of[Either[Any, Any]]
      Type.isSubtypeOf[A, Either[Any, Any]] || isTry[A]
    }

    private def oneOf[A: Type](gens: List[Expr[Gen[A]]]): Expr[Gen[A]] = {
      implicit val GenA: Type[Gen[A]] = ArbitraryTypes.Gen[A]
      gens match {
        case Nil                    => Expr.quote(_root_.org.scalacheck.Gen.fail[A])
        case single :: Nil          => single
        case first :: second :: Nil =>
          Expr.quote(_root_.org.scalacheck.Gen.oneOf(Expr.splice(first), Expr.splice(second)))
        case first :: second :: rest =>
          val restGens: Expr[_root_.scala.List[_root_.org.scalacheck.Gen[A]]] =
            rest.foldRight(Expr.quote(_root_.scala.List.empty[_root_.org.scalacheck.Gen[A]])) { (genExpr, accExpr) =>
              Expr.quote(Expr.splice(genExpr) :: Expr.splice(accExpr))
            }
          // A runtime helper instead of `Gen.oneOf(first, second, Expr.splice(restGens)*)`: Hearth's Scala 2
          // cross-quotes expand a spliced vararg to the arguments of the spliced `Seq(...)` call, so of a `::` chain
          // only its head was passed - a sealed trait / enum with more than 3 subtypes generated only the first 3
          // (docs/research/hearth-bug-cross-quotes-vararg-splice.md).
          Expr.quote {
            hearth.kindlings.scalacheckderivation.internal.runtime.ScalaCheckUtils
              .oneOfGens[A](Expr.splice(first), Expr.splice(second), Expr.splice(restGens))
          }
      }
    }
  }
}
