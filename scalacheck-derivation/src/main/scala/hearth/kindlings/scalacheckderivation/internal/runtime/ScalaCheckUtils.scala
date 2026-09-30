package hearth.kindlings.scalacheckderivation.internal.runtime

import org.scalacheck.Gen

object ScalaCheckUtils {

  /** Generates a collection through the provider's smart constructor (`build`), so that the result has the requested
    * container type even when its intermediate `CtorResult` differs (e.g. `List` for `cats.data.NonEmptyList`).
    *
    * `build` may reject a candidate (e.g. an empty list for a non-empty container). Instead of throwing, a rejected
    * candidate is regenerated once with at least one element, and if that is rejected too the generator fails with
    * ScalaCheck's own `Gen.fail` - no value, which `forAll` counts as discarded, never an exception. An exception
    * thrown by the collection's builder itself (e.g. a negative element for `BitSet`) is treated as a rejection too.
    *
    * The size budget is halved for element generation to prevent infinite recursion on types like `List[TreeNode]`.
    * `elemGen` is by-name for the same reason: for a recursive type it is a call to the cached generator being defined.
    */
  def genCollection[Item, A](elemGen: => Gen[Item], build: List[Item] => Either[Any, A]): Gen[A] =
    Gen.sized { n =>
      val size = scala.math.max(n / 2, 0)
      Gen.resize(size, Gen.listOf(elemGen)).flatMap { items =>
        safeBuild(build, items) match {
          case Right(value) => Gen.const(value)
          case Left(_)      =>
            Gen.resize(size, Gen.nonEmptyListOf(elemGen)).flatMap { nonEmptyItems =>
              safeBuild(build, nonEmptyItems) match {
                case Right(value) => Gen.const(value)
                case Left(_)      => Gen.fail[A]
              }
            }
        }
      }
    }

  /** Runs a provider's build step, treating an exception thrown by the collection's own builder (e.g. a negative
    * element for `BitSet`) as a rejected candidate, like a `Left` from a smart constructor.
    */
  def safeBuild[Item, A](build: List[Item] => Either[Any, A], items: List[Item]): Either[Any, A] =
    try build(items)
    catch { case scala.util.control.NonFatal(e) => Left(e) }

  /** Combines a list of generators using flatMap chaining.
    *
    * This avoids relying on ScalaCheck's Buildable typeclass which has inconsistent behavior across ScalaCheck
    * versions. Instead, we manually chain generators with flatMap and construct the result from a List[Any].
    *
    * The implementation accumulates generated values in reverse order (for efficient prepend), then reverses the list
    * before passing it to the constructor.
    *
    * @param gens
    *   List of generators for each field (cast to Gen[Any] to work around path-dependent types)
    * @param construct
    *   Function that builds the final value from List[Any] (typically a case class constructor with casts)
    * @tparam A
    *   The type of value to construct
    * @return
    *   A generator that sequences all field generators and constructs the result
    */
  def sequenceGens[A](gens: List[Gen[Any]])(construct: List[Any] => A): Gen[A] = {
    def loop(remaining: List[Gen[Any]], acc: List[Any]): Gen[A] = remaining match {
      case Nil         => Gen.const(construct(acc.reverse))
      case gen :: tail => gen.flatMap(value => loop(tail, value :: acc))
    }
    loop(gens, Nil)
  }

  /** Runtime type cast helper to avoid path-dependent type leakage in macros.
    *
    * The gen parameter is unused at runtime due to JVM type erasure, but carries type information at compile-time. This
    * allows `unsafeCast[A]` to infer A from `Gen[A]` without leaking path-dependent types like `param.tpe.FieldType`
    * into the generated code, which would cause "not found: value param" compilation errors.
    *
    * ==Type Witness Pattern==
    *
    * During macro expansion, we:
    *   1. Derive `Expr[Gen[FieldType]]` for each field (where FieldType is a path-dependent type)
    *   2. Create accessor functions that close over the typed generator expression
    *   3. Inside the accessor, call `unsafeCast(listValue, genExpr)` where genExpr is the type witness
    *   4. The compiler infers A = FieldType from Gen[FieldType], without the path dependency leaking
    *
    * Example generated code:
    * {{{
    * ScalaCheckUtils.unsafeCast(
    *   values(0),
    *   Arbitrary.arbString.arbitrary  // Type witness: Gen[String]
    * )
    * }}}
    *
    * At runtime, this is just `values(0).asInstanceOf[String]` due to type erasure.
    *
    * @param value
    *   The value to cast (typically extracted from a List[Any])
    * @param gen
    *   Type witness carrying the target type A (unused at runtime)
    * @tparam A
    *   The target type to cast to
    * @return
    *   value cast to type A
    */
  @scala.annotation.nowarn("msg=unused")
  def unsafeCast[A](value: Any, gen: Gen[A]): A = value.asInstanceOf[A]

  /** Runtime type cast helper using Shrink as type witness. Same pattern as unsafeCast but for Shrink derivation. */
  @scala.annotation.nowarn("msg=unused")
  def unsafeCastViaShrink[A](value: Any, shrink: org.scalacheck.Shrink[A]): A = value.asInstanceOf[A]
}
