package hearth.kindlings.scalacheckderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

/** Shared by the Arbitrary/Cogen rules for functions, `PartialFunction`, `Future` and `Gen`: recognizing these types by
  * their type constructor (arity-generic for `Function0`-`Function22`) and extracting their type arguments.
  */
trait ScalacheckFunctionSupport { this: MacroCommons & StdExtensions =>

  // Index = arity.
  private lazy val functionTypes: Vector[??] = Vector(
      Type.of[Function0[Any]].as_??,
      Type.of[Function1[Any, Any]].as_??,
      Type.of[Function2[Any, Any, Any]].as_??,
      Type.of[Function3[Any, Any, Any, Any]].as_??,
      Type.of[Function4[Any, Any, Any, Any, Any]].as_??,
      Type.of[Function5[Any, Any, Any, Any, Any, Any]].as_??,
      Type.of[Function6[Any, Any, Any, Any, Any, Any, Any]].as_??,
      Type.of[Function7[Any, Any, Any, Any, Any, Any, Any, Any]].as_??,
      Type.of[Function8[Any, Any, Any, Any, Any, Any, Any, Any, Any]].as_??,
      Type.of[Function9[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]].as_??,
      Type.of[Function10[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]].as_??,
      Type.of[Function11[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]].as_??,
      Type.of[Function12[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]].as_??,
      Type.of[Function13[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]].as_??,
      Type.of[Function14[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]].as_??,
      Type.of[Function15[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]].as_??,
      Type.of[Function16[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]].as_??,
      Type.of[Function17[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]].as_??,
      Type.of[Function18[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]].as_??,
      Type.of[Function19[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]].as_??,
      Type.of[Function20[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]].as_??,
      Type.of[Function21[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]].as_??,
      Type.of[Function22[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]].as_??
  )

  /** `Some(arity)` when `A` is a `FunctionN` (0-22) - compared by type constructor, so e.g. a `Map` (which extends
    * `PartialFunction` and `Function1`) is not one.
    */
  protected def functionArity[A: Type]: Option[Int] = {
    val arity = functionTypes.indexWhere { function =>
      import function.Underlying as F
      Type.hasSameTypeConstructor[A, F]
    }
    if (arity < 0) None else Some(arity)
  }

  protected def isPartialFunctionType[A: Type]: Boolean = {
    implicit val PF: Type[PartialFunction[Any, Any]] = Type.of[PartialFunction[Any, Any]]
    Type.hasSameTypeConstructor[A, PartialFunction[Any, Any]]
  }

  protected def isFutureType[A: Type]: Boolean = {
    implicit val F: Type[scala.concurrent.Future[Any]] = Type.of[scala.concurrent.Future[Any]]
    Type.hasSameTypeConstructor[A, scala.concurrent.Future[Any]]
  }

  protected def isGenType[A: Type]: Boolean = {
    implicit val G: Type[org.scalacheck.Gen[Any]] = Type.of[org.scalacheck.Gen[Any]]
    Type.hasSameTypeConstructor[A, org.scalacheck.Gen[Any]]
  }

  /** Sequentially (so that cache writes happen in order) derives something for every type argument. */
  protected def traverseTypes[B](types: List[??])(f: ?? => MIO[B]): MIO[List[B]] =
    types.foldRight(MIO.pure(List.empty[B])) { (tpe, acc) =>
      for {
        head <- f(tpe)
        tail <- acc
      } yield head :: tail
    }

  /** Builds `List(e1, e2, ...)` from the element expressions. */
  protected def listExpr[B: Type](exprs: List[Expr[B]]): Expr[List[B]] = {
    implicit val ListB: Type[List[B]] = Type.of[List[B]]
    exprs.foldRight(Expr.quote(List.empty[B])) { (head, tail) =>
      Expr.quote(Expr.splice(head) :: Expr.splice(tail))
    }
  }
}
