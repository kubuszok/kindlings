package hearth.kindlings.scalacheckderivation.internal.compiletime

import hearth.MacroCommons
import hearth.std.*

/** Literal (constant) types such as `"foo"`, `42` or `true`, alone or in unions (`"a" | "b"`, `1 | 2 | 3`), handled
  * like scalacheck-derived-extras' `arbLiteral` / `cogenLiteral`: the only value is always generated, and every value
  * perturbs a seed alike.
  */
trait ScalacheckLiteralSupport { this: MacroCommons & StdExtensions =>

  /** The value of the literal type `A`, or `None` when `A` is not a literal type. */
  protected def literalValue[A: Type]: Option[Expr[A]] = {
    def of[U](codec: TypeCodec[U])(implicit exprCodec: ExprCodec[U], U: Type[U]): Option[Expr[A]] =
      codec.fromType(Type[A]).map { e =>
        val constant: U = e.value
        val constantExpr: Expr[U] = Expr(constant)
        Expr.quote(Expr.splice(constantExpr).asInstanceOf[A])
      }
    implicit val StringT: Type[String] = Type.of[String]
    implicit val IntT: Type[Int] = Type.of[Int]
    implicit val LongT: Type[Long] = Type.of[Long]
    implicit val BooleanT: Type[Boolean] = Type.of[Boolean]
    implicit val DoubleT: Type[Double] = Type.of[Double]
    implicit val FloatT: Type[Float] = Type.of[Float]
    implicit val CharT: Type[Char] = Type.of[Char]
    implicit val ShortT: Type[Short] = Type.of[Short]
    implicit val ByteT: Type[Byte] = Type.of[Byte]
    of(Type.StringCodec)
      .orElse(of(Type.IntCodec))
      .orElse(of(Type.LongCodec))
      .orElse(of(Type.BooleanCodec))
      .orElse(of(Type.DoubleCodec))
      .orElse(of(Type.FloatCodec))
      .orElse(of(Type.CharCodec))
      .orElse(of(Type.ShortCodec))
      .orElse(of(Type.ByteCodec))
  }
}
