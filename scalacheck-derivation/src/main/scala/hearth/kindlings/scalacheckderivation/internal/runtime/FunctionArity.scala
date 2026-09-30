package hearth.kindlings.scalacheckderivation.internal.runtime

/** Arity-generic bridging between `FunctionN` values and `Array[Any] => Any`, so the macro can derive
  * `Arbitrary`/`Cogen` for functions of any arity (0-22) without arity-specific generated code.
  */
object FunctionArity {

  /** Wraps `f` as a `FunctionN` of the given arity, passing the arguments to `f` as an array. */
  def fromArray(arity: Int, f: Array[Any] => Any): Any = arity match {
    case 0 => () => f(Array[Any]())
    case 1 => (a1: Any) => f(Array[Any](a1))
    case 2 => (a1: Any, a2: Any) => f(Array[Any](a1, a2))
    case 3 => (a1: Any, a2: Any, a3: Any) => f(Array[Any](a1, a2, a3))
    case 4 => (a1: Any, a2: Any, a3: Any, a4: Any) => f(Array[Any](a1, a2, a3, a4))
    case 5 => (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any) => f(Array[Any](a1, a2, a3, a4, a5))
    case 6 => (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any, a6: Any) => f(Array[Any](a1, a2, a3, a4, a5, a6))
    case 7 =>
      (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any, a6: Any, a7: Any) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7))
    case 8 =>
      (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any, a6: Any, a7: Any, a8: Any) =>
        f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8))
    case 9 =>
      (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any, a6: Any, a7: Any, a8: Any, a9: Any) =>
        f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9))
    case 10 =>
      (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any, a6: Any, a7: Any, a8: Any, a9: Any, a10: Any) =>
        f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9, a10))
    case 11 =>
      (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any, a6: Any, a7: Any, a8: Any, a9: Any, a10: Any, a11: Any) =>
        f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11))
    case 12 =>
      (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any, a6: Any, a7: Any, a8: Any, a9: Any, a10: Any, a11: Any, a12: Any) =>
        f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12))
    case 13 =>
      (
          a1: Any,
          a2: Any,
          a3: Any,
          a4: Any,
          a5: Any,
          a6: Any,
          a7: Any,
          a8: Any,
          a9: Any,
          a10: Any,
          a11: Any,
          a12: Any,
          a13: Any
      ) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13))
    case 14 =>
      (
          a1: Any,
          a2: Any,
          a3: Any,
          a4: Any,
          a5: Any,
          a6: Any,
          a7: Any,
          a8: Any,
          a9: Any,
          a10: Any,
          a11: Any,
          a12: Any,
          a13: Any,
          a14: Any
      ) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14))
    case 15 =>
      (
          a1: Any,
          a2: Any,
          a3: Any,
          a4: Any,
          a5: Any,
          a6: Any,
          a7: Any,
          a8: Any,
          a9: Any,
          a10: Any,
          a11: Any,
          a12: Any,
          a13: Any,
          a14: Any,
          a15: Any
      ) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15))
    case 16 =>
      (
          a1: Any,
          a2: Any,
          a3: Any,
          a4: Any,
          a5: Any,
          a6: Any,
          a7: Any,
          a8: Any,
          a9: Any,
          a10: Any,
          a11: Any,
          a12: Any,
          a13: Any,
          a14: Any,
          a15: Any,
          a16: Any
      ) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16))
    case 17 =>
      (
          a1: Any,
          a2: Any,
          a3: Any,
          a4: Any,
          a5: Any,
          a6: Any,
          a7: Any,
          a8: Any,
          a9: Any,
          a10: Any,
          a11: Any,
          a12: Any,
          a13: Any,
          a14: Any,
          a15: Any,
          a16: Any,
          a17: Any
      ) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17))
    case 18 =>
      (
          a1: Any,
          a2: Any,
          a3: Any,
          a4: Any,
          a5: Any,
          a6: Any,
          a7: Any,
          a8: Any,
          a9: Any,
          a10: Any,
          a11: Any,
          a12: Any,
          a13: Any,
          a14: Any,
          a15: Any,
          a16: Any,
          a17: Any,
          a18: Any
      ) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18))
    case 19 =>
      (
          a1: Any,
          a2: Any,
          a3: Any,
          a4: Any,
          a5: Any,
          a6: Any,
          a7: Any,
          a8: Any,
          a9: Any,
          a10: Any,
          a11: Any,
          a12: Any,
          a13: Any,
          a14: Any,
          a15: Any,
          a16: Any,
          a17: Any,
          a18: Any,
          a19: Any
      ) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19))
    case 20 =>
      (
          a1: Any,
          a2: Any,
          a3: Any,
          a4: Any,
          a5: Any,
          a6: Any,
          a7: Any,
          a8: Any,
          a9: Any,
          a10: Any,
          a11: Any,
          a12: Any,
          a13: Any,
          a14: Any,
          a15: Any,
          a16: Any,
          a17: Any,
          a18: Any,
          a19: Any,
          a20: Any
      ) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20))
    case 21 =>
      (
          a1: Any,
          a2: Any,
          a3: Any,
          a4: Any,
          a5: Any,
          a6: Any,
          a7: Any,
          a8: Any,
          a9: Any,
          a10: Any,
          a11: Any,
          a12: Any,
          a13: Any,
          a14: Any,
          a15: Any,
          a16: Any,
          a17: Any,
          a18: Any,
          a19: Any,
          a20: Any,
          a21: Any
      ) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21))
    case 22 =>
      (
          a1: Any,
          a2: Any,
          a3: Any,
          a4: Any,
          a5: Any,
          a6: Any,
          a7: Any,
          a8: Any,
          a9: Any,
          a10: Any,
          a11: Any,
          a12: Any,
          a13: Any,
          a14: Any,
          a15: Any,
          a16: Any,
          a17: Any,
          a18: Any,
          a19: Any,
          a20: Any,
          a21: Any,
          a22: Any
      ) =>
        f(
          Array[Any](
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            a12,
            a13,
            a14,
            a15,
            a16,
            a17,
            a18,
            a19,
            a20,
            a21,
            a22
          )
        )
    case _ => throw new IllegalArgumentException(s"Unsupported function arity: $arity")
  }

  /** Applies a `FunctionN` of the given arity to the arguments in `args`. */
  def apply(arity: Int, function: Any, args: Array[Any]): Any = arity match {
    case 0 => function.asInstanceOf[Function0[Any]]()
    case 1 => function.asInstanceOf[Function1[Any, Any]](args(0))
    case 2 => function.asInstanceOf[Function2[Any, Any, Any]](args(0), args(1))
    case 3 => function.asInstanceOf[Function3[Any, Any, Any, Any]](args(0), args(1), args(2))
    case 4 => function.asInstanceOf[Function4[Any, Any, Any, Any, Any]](args(0), args(1), args(2), args(3))
    case 5 =>
      function.asInstanceOf[Function5[Any, Any, Any, Any, Any, Any]](args(0), args(1), args(2), args(3), args(4))
    case 6 =>
      function.asInstanceOf[Function6[Any, Any, Any, Any, Any, Any, Any]](
        args(0),
        args(1),
        args(2),
        args(3),
        args(4),
        args(5)
      )
    case 7 =>
      function.asInstanceOf[Function7[Any, Any, Any, Any, Any, Any, Any, Any]](
        args(0),
        args(1),
        args(2),
        args(3),
        args(4),
        args(5),
        args(6)
      )
    case 8 =>
      function.asInstanceOf[Function8[Any, Any, Any, Any, Any, Any, Any, Any, Any]](
        args(0),
        args(1),
        args(2),
        args(3),
        args(4),
        args(5),
        args(6),
        args(7)
      )
    case 9 =>
      function.asInstanceOf[Function9[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]](
        args(0),
        args(1),
        args(2),
        args(3),
        args(4),
        args(5),
        args(6),
        args(7),
        args(8)
      )
    case 10 =>
      function.asInstanceOf[Function10[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]](
        args(0),
        args(1),
        args(2),
        args(3),
        args(4),
        args(5),
        args(6),
        args(7),
        args(8),
        args(9)
      )
    case 11 =>
      function.asInstanceOf[Function11[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]](
        args(0),
        args(1),
        args(2),
        args(3),
        args(4),
        args(5),
        args(6),
        args(7),
        args(8),
        args(9),
        args(10)
      )
    case 12 =>
      function.asInstanceOf[Function12[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]](
        args(0),
        args(1),
        args(2),
        args(3),
        args(4),
        args(5),
        args(6),
        args(7),
        args(8),
        args(9),
        args(10),
        args(11)
      )
    case 13 =>
      function.asInstanceOf[Function13[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]](
        args(0),
        args(1),
        args(2),
        args(3),
        args(4),
        args(5),
        args(6),
        args(7),
        args(8),
        args(9),
        args(10),
        args(11),
        args(12)
      )
    case 14 =>
      function.asInstanceOf[Function14[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]](
        args(0),
        args(1),
        args(2),
        args(3),
        args(4),
        args(5),
        args(6),
        args(7),
        args(8),
        args(9),
        args(10),
        args(11),
        args(12),
        args(13)
      )
    case 15 =>
      function.asInstanceOf[Function15[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]](
        args(0),
        args(1),
        args(2),
        args(3),
        args(4),
        args(5),
        args(6),
        args(7),
        args(8),
        args(9),
        args(10),
        args(11),
        args(12),
        args(13),
        args(14)
      )
    case 16 =>
      function
        .asInstanceOf[Function16[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]](
          args(0),
          args(1),
          args(2),
          args(3),
          args(4),
          args(5),
          args(6),
          args(7),
          args(8),
          args(9),
          args(10),
          args(11),
          args(12),
          args(13),
          args(14),
          args(15)
        )
    case 17 =>
      function.asInstanceOf[
        Function17[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]
      ](
        args(0),
        args(1),
        args(2),
        args(3),
        args(4),
        args(5),
        args(6),
        args(7),
        args(8),
        args(9),
        args(10),
        args(11),
        args(12),
        args(13),
        args(14),
        args(15),
        args(16)
      )
    case 18 =>
      function.asInstanceOf[
        Function18[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]
      ](
        args(0),
        args(1),
        args(2),
        args(3),
        args(4),
        args(5),
        args(6),
        args(7),
        args(8),
        args(9),
        args(10),
        args(11),
        args(12),
        args(13),
        args(14),
        args(15),
        args(16),
        args(17)
      )
    case 19 =>
      function.asInstanceOf[
        Function19[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]
      ](
        args(0),
        args(1),
        args(2),
        args(3),
        args(4),
        args(5),
        args(6),
        args(7),
        args(8),
        args(9),
        args(10),
        args(11),
        args(12),
        args(13),
        args(14),
        args(15),
        args(16),
        args(17),
        args(18)
      )
    case 20 =>
      function.asInstanceOf[Function20[
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any
      ]](
        args(0),
        args(1),
        args(2),
        args(3),
        args(4),
        args(5),
        args(6),
        args(7),
        args(8),
        args(9),
        args(10),
        args(11),
        args(12),
        args(13),
        args(14),
        args(15),
        args(16),
        args(17),
        args(18),
        args(19)
      )
    case 21 =>
      function.asInstanceOf[Function21[
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any
      ]](
        args(0),
        args(1),
        args(2),
        args(3),
        args(4),
        args(5),
        args(6),
        args(7),
        args(8),
        args(9),
        args(10),
        args(11),
        args(12),
        args(13),
        args(14),
        args(15),
        args(16),
        args(17),
        args(18),
        args(19),
        args(20)
      )
    case 22 =>
      function.asInstanceOf[Function22[
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any,
        Any
      ]](
        args(0),
        args(1),
        args(2),
        args(3),
        args(4),
        args(5),
        args(6),
        args(7),
        args(8),
        args(9),
        args(10),
        args(11),
        args(12),
        args(13),
        args(14),
        args(15),
        args(16),
        args(17),
        args(18),
        args(19),
        args(20),
        args(21)
      )
    case _ => throw new IllegalArgumentException(s"Unsupported function arity: $arity")
  }
}
