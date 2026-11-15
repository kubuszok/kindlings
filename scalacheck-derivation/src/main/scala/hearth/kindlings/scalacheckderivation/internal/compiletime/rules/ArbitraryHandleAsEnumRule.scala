package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import org.scalacheck.Gen

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

      // Get all direct children
      val childrenList = enumData.directChildren.toList

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
              Expr.quote {
                _root_.org.scalacheck.Gen.sized { n =>
                  if (n <= 0) Expr.splice(atZeroExpr)
                  else _root_.org.scalacheck.Gen.resize(n - 1, Expr.splice(oneOfExpr))
                }
              }
            }
      }
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
          Expr.quote {
            _root_.org.scalacheck.Gen.oneOf(Expr.splice(first), Expr.splice(second), Expr.splice(restGens)*)
          }
      }
    }

    /** Whether `tpe` can contain a value of `target`: it is `target`, one of its type arguments can, or (for case
      * classes and sealed hierarchies, up to a bounded depth) one of its fields / subtypes can.
      */
    private def mentionsType(tpe: ??, target: ??, visited: Set[String], depth: Int): Boolean = {
      import tpe.Underlying as T
      import target.Underlying as Target
      val key = Type[T].plainPrint
      if (Type[T] =:= Type[Target]) true
      else if (visited(key) || depth > 8) false
      else {
        val seen = visited + key
        Type.typeArguments[T].exists(mentionsType(_, target, seen, depth + 1)) ||
        (CaseClass.parse[T].toOption match {
          case Some(caseClass) =>
            caseClass.primaryConstructor.totalParameters.flatten.exists { case (_, param) =>
              mentionsType(param.tpe, target, seen, depth + 1)
            }
          case None =>
            Enum
              .parse[T]
              .toOption
              .exists(_.directChildren.toList.exists { case (_, child) =>
                mentionsType(child.Underlying.as_??, target, seen, depth + 1)
              })
        })
      }
    }
  }
}
