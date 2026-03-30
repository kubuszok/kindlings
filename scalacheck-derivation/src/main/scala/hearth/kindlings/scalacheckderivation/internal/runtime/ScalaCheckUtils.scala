package hearth.kindlings.scalacheckderivation.internal.runtime

import org.scalacheck.Gen

object ScalaCheckUtils {

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
    * The gen parameter is unused at runtime due to JVM type erasure, but carries type information at compile-time.
    * This allows `unsafeCast[A]` to infer A from `Gen[A]` without leaking path-dependent types like
    * `param.tpe.FieldType` into the generated code, which would cause "not found: value param" compilation errors.
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
}
