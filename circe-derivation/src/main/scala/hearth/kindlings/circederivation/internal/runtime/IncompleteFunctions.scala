package hearth.kindlings.circederivation.internal.runtime

// A FunctionN of every arity from a function of an argument array (see `KindlingsDecoder.incomplete`). The result is
// cast to the expected function type: it is erased, and the specialized `apply$mc..$sp` variants of Function1 and
// Function2 fall back to the generic `apply`.
private[runtime] object IncompleteFunctions {

  // format: off
  def fromArray(arity: Int, f: Array[Any] => Any): AnyRef = arity match {
    case  1 => (a1: Any) => f(Array[Any](a1))
    case  2 => (a1: Any, a2: Any) => f(Array[Any](a1, a2))
    case  3 => (a1: Any, a2: Any, a3: Any) => f(Array[Any](a1, a2, a3))
    case  4 => (a1: Any, a2: Any, a3: Any, a4: Any) => f(Array[Any](a1, a2, a3, a4))
    case  5 => (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any) => f(Array[Any](a1, a2, a3, a4, a5))
    case  6 => (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any, a6: Any) => f(Array[Any](a1, a2, a3, a4, a5, a6))
    case  7 => (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any, a6: Any, a7: Any) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7))
    case  8 => (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any, a6: Any, a7: Any, a8: Any) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8))
    case  9 => (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any, a6: Any, a7: Any, a8: Any, a9: Any) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9))
    case 10 => (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any, a6: Any, a7: Any, a8: Any, a9: Any, a10: Any) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9, a10))
    case 11 => (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any, a6: Any, a7: Any, a8: Any, a9: Any, a10: Any, a11: Any) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11))
    case 12 => (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any, a6: Any, a7: Any, a8: Any, a9: Any, a10: Any, a11: Any, a12: Any) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12))
    case 13 => (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any, a6: Any, a7: Any, a8: Any, a9: Any, a10: Any, a11: Any, a12: Any, a13: Any) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13))
    case 14 => (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any, a6: Any, a7: Any, a8: Any, a9: Any, a10: Any, a11: Any, a12: Any, a13: Any, a14: Any) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14))
    case 15 => (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any, a6: Any, a7: Any, a8: Any, a9: Any, a10: Any, a11: Any, a12: Any, a13: Any, a14: Any, a15: Any) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15))
    case 16 => (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any, a6: Any, a7: Any, a8: Any, a9: Any, a10: Any, a11: Any, a12: Any, a13: Any, a14: Any, a15: Any, a16: Any) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16))
    case 17 => (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any, a6: Any, a7: Any, a8: Any, a9: Any, a10: Any, a11: Any, a12: Any, a13: Any, a14: Any, a15: Any, a16: Any, a17: Any) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17))
    case 18 => (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any, a6: Any, a7: Any, a8: Any, a9: Any, a10: Any, a11: Any, a12: Any, a13: Any, a14: Any, a15: Any, a16: Any, a17: Any, a18: Any) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18))
    case 19 => (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any, a6: Any, a7: Any, a8: Any, a9: Any, a10: Any, a11: Any, a12: Any, a13: Any, a14: Any, a15: Any, a16: Any, a17: Any, a18: Any, a19: Any) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19))
    case 20 => (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any, a6: Any, a7: Any, a8: Any, a9: Any, a10: Any, a11: Any, a12: Any, a13: Any, a14: Any, a15: Any, a16: Any, a17: Any, a18: Any, a19: Any, a20: Any) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20))
    case 21 => (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any, a6: Any, a7: Any, a8: Any, a9: Any, a10: Any, a11: Any, a12: Any, a13: Any, a14: Any, a15: Any, a16: Any, a17: Any, a18: Any, a19: Any, a20: Any, a21: Any) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21))
    case 22 => (a1: Any, a2: Any, a3: Any, a4: Any, a5: Any, a6: Any, a7: Any, a8: Any, a9: Any, a10: Any, a11: Any, a12: Any, a13: Any, a14: Any, a15: Any, a16: Any, a17: Any, a18: Any, a19: Any, a20: Any, a21: Any, a22: Any) => f(Array[Any](a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22))
    case other => throw new IllegalArgumentException(s"Unsupported function arity: $other")
  }
  // format: on
}
