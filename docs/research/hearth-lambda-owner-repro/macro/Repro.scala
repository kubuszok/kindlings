package repro

import hearth.*
import hearth.fp.instances.*
import hearth.fp.syntax.*
import hearth.fp.data.NonEmptyVector
import scala.quoted.Quotes

/** The macro logic, written with Hearth's cross-platform API. */
trait ReproImpl { this: MacroCommons =>

  /** Shape 2: each attempt is a `ValDefs` val; the next attempt is built in the `else` splice of a quote. */
  def chain(remaining: List[Int])(implicit IntT: Type[Int]): Expr[Int] = remaining match {
    case n :: rest =>
      ValDefs.createVal[Int](Expr(n), "attempt").use { r =>
        Expr.quote(if Expr.splice(r) > 100 then Expr.splice(r) else Expr.splice(chain(rest)))
      }
    case Nil => Expr(0)
  }

  /** Variant A: the chain is built outside of the quoted lambda it ends up in. */
  def builtOutsideQuotedLambda: Expr[Int => Int] = {
    implicit val IntT: Type[Int] = Type.of[Int]
    val body = chain(List(1, 2, 3))
    Expr.quote((i: Int) => i + Expr.splice(body))
  }

  /** Variant D: the chain is built inside the splice of a quoted lambda. */
  def builtInsideQuotedLambda: Expr[Int => Int] = {
    implicit val IntT: Type[Int] = Type.of[Int]
    Expr.quote((i: Int) => i + Expr.splice(chain(List(1, 2, 3))))
  }

  /** Shape 1: `ValDefs` vals, and a quote that defines vals itself (Scala 3 desugars `r :: acc` into
    * `{ val elem$1 = r; acc.::(elem$1) }`), built inside the splice of a quoted lambda.
    */
  def valDefsNextToQuotedValsInsideQuotedLambda: Expr[Int => List[Int]] = {
    implicit val IntT: Type[Int] = Type.of[Int]
    implicit val ListIntT: Type[List[Int]] = Type.of[List[Int]]
    def bindAll(remaining: List[Expr[Int]], bound: List[Expr[Int]]): Expr[List[Int]] = remaining match {
      case r :: rest => ValDefs.createVal[Int](r, "bound").use(ref => bindAll(rest, ref :: bound))
      case Nil       =>
        bound.reverse.foldRight(Expr.quote(List.empty[Int])) { (r, acc) =>
          Expr.quote(Expr.splice(r) :: Expr.splice(acc))
        }
    }
    Expr.quote((i: Int) => Expr.splice(bindAll(List(Expr.quote(i), Expr.quote(i + 1)), Nil)))
  }

  /** Variant G: a quoted lambda (with the chain in its splice) inside a LambdaBuilder body. */
  def quotedLambdaInsideLambdaBuilder: Expr[Int => Int] = {
    implicit val IntT: Type[Int] = Type.of[Int]
    implicit val FnT: Type[Int => Int] = Type.of[Int => Int]
    val fn = LambdaBuilder
      .of1[Int]("start")
      .map(start => Expr.quote(Expr.splice(start) + ((j: Int) => j + Expr.splice(chain(List(1, 2, 3))))(1)))
      .build[Int]
    fn
  }

  /** Variant H: the chain built inside a LambdaBuilder body which is inside the splice of a quoted lambda. */
  def lambdaBuilderInsideQuotedLambda: Expr[Int => Int] = {
    implicit val IntT: Type[Int] = Type.of[Int]
    Expr.quote { (i: Int) =>
      Expr
        .splice {
          LambdaBuilder
            .of1[Int]("start")
            .map(start => Expr.quote(Expr.splice(start) + Expr.splice(chain(List(1, 2, 3)))))
            .build[Int]
        }
        .apply(i)
    }
  }

  /** Variant M (regression in 0.4.4-11): `ValDefs` vals nested through `MatchCase.matchOn` case bodies (how
    * `IsEither.fold` folds an `Either` without closures), built inside the splice of a quoted lambda.
    */
  def matchCaseChainInsideQuotedLambda: Expr[Int => Either[String, Int]] = {
    implicit val IntT: Type[Int] = Type.of[Int]
    implicit val StringT: Type[String] = Type.of[String]
    implicit val EitherT: Type[Either[String, Int]] = Type.of[Either[String, Int]]
    implicit val RightT: Type[Right[String, Int]] = Type.of[Right[String, Int]]
    implicit val LeftT: Type[Left[String, Int]] = Type.of[Left[String, Int]]
    def foldAll(remaining: List[Expr[Either[String, Int]]], sum: Expr[Int]): Expr[Either[String, Int]] =
      remaining match {
        case r :: rest =>
          ValDefs.createVal[Either[String, Int]](r, "either").use { bound =>
            MatchCase.matchOn[Either[String, Int], Either[String, Int]](bound)(
              NonEmptyVector(
                MatchCase.typeMatch[Right[String, Int]]("right").map { right =>
                  foldAll(rest, Expr.quote(Expr.splice(sum) + Expr.splice(right).value))
                },
                MatchCase.typeMatch[Left[String, Int]]("left").map { left =>
                  Expr.quote(Expr.splice(left): Either[String, Int])
                }
              )
            )
          }
        case Nil => Expr.quote(Right(Expr.splice(sum)): Either[String, Int])
      }
    Expr.quote { (i: Int) =>
      Expr.splice(
        foldAll(List(Expr.quote(Right(i): Either[String, Int]), Expr.quote(Right(i + 1): Either[String, Int])), Expr(0))
      )
    }
  }
}

final class Repro(q: Quotes) extends MacroCommonsScala3(using q), ReproImpl

object Repro {
  def builtOutsideQuotedLambdaImpl(using q: Quotes): scala.quoted.Expr[Int => Int] =
    new Repro(q).builtOutsideQuotedLambda
  def builtInsideQuotedLambdaImpl(using q: Quotes): scala.quoted.Expr[Int => Int] =
    new Repro(q).builtInsideQuotedLambda
  def valDefsNextToQuotedValsInsideQuotedLambdaImpl(using q: Quotes): scala.quoted.Expr[Int => List[Int]] =
    new Repro(q).valDefsNextToQuotedValsInsideQuotedLambda
  def quotedLambdaInsideLambdaBuilderImpl(using q: Quotes): scala.quoted.Expr[Int => Int] =
    new Repro(q).quotedLambdaInsideLambdaBuilder
  def lambdaBuilderInsideQuotedLambdaImpl(using q: Quotes): scala.quoted.Expr[Int => Int] =
    new Repro(q).lambdaBuilderInsideQuotedLambda
  def matchCaseChainInsideQuotedLambdaImpl(using q: Quotes): scala.quoted.Expr[Int => Either[String, Int]] =
    new Repro(q).matchCaseChainInsideQuotedLambda
}
