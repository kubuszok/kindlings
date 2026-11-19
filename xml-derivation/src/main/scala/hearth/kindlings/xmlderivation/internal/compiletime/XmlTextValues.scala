package hearth.kindlings.xmlderivation.internal.compiletime

import hearth.MacroCommons
import hearth.std.*

/** Which field types are a single text value, so that they can be written as an XML attribute.
  *
  * A text value is a built-in type (`String`, the primitives, `BigDecimal`, `BigInt`) or a value type wrapping one.
  * Under `withAttributesByDefault` only these fields (and `Option`s of them) become attributes: collections, maps, case
  * classes, sealed traits and enums stay child elements, so both the encoder and the decoder use this to decide.
  */
trait XmlTextValues { this: MacroCommons & StdExtensions =>

  private lazy val textValueBuiltIns: List[??] = List(
    Type.of[String].as_??,
    Type.of[Boolean].as_??,
    Type.of[Byte].as_??,
    Type.of[Short].as_??,
    Type.of[Int].as_??,
    Type.of[Long].as_??,
    Type.of[Float].as_??,
    Type.of[Double].as_??,
    Type.of[Char].as_??,
    Type.of[BigDecimal].as_??,
    Type.of[BigInt].as_??
  )

  /** Whether `A` is a built-in text type (not a value type over one). */
  def isXmlBuiltInTextValue[A](tpe: Type[A]): Boolean = {
    implicit val A: Type[A] = tpe
    textValueBuiltIns.exists { builtIn =>
      import builtIn.Underlying as B
      Type[A] <:< Type[B]
    }
  }

  /** Whether `A` is a single text value: a built-in type or a value type over one. */
  def isXmlTextValue[A](tpe: Type[A]): Boolean = {
    implicit val A: Type[A] = tpe
    isXmlBuiltInTextValue(tpe) || (Type[A] match {
      case IsValueType(isValueType) =>
        import isValueType.Underlying as Inner
        isXmlTextValue(Type[Inner])
      case _ => false
    })
  }

  /** Whether `A` is a text value or an `Option` of one: what `withAttributesByDefault` writes as an attribute. */
  def isXmlAttributeValue[A](tpe: Type[A]): Boolean = {
    implicit val A: Type[A] = tpe
    Type[A] match {
      case IsOption(isOption) =>
        import isOption.Underlying as Inner
        isXmlTextValue(Type[Inner])
      case _ => isXmlTextValue(Type[A])
    }
  }

  /** Whether `A` is a `String` or a `Char` (or a value type over one): its text is kept exactly, blank text is a value.
    */
  def isXmlStringValue[A](tpe: Type[A]): Boolean = {
    implicit val A: Type[A] = tpe
    Type[A] <:< Type.of[String] || Type[A] <:< Type.of[Char] || (Type[A] match {
      case IsValueType(isValueType) =>
        import isValueType.Underlying as Inner
        isXmlStringValue(Type[Inner])
      case _ => false
    })
  }

  /** The text of a text value (see [[isXmlTextValue]]): value types are unwrapped, as their element encoding does. */
  @scala.annotation.nowarn("msg=is never used")
  def xmlTextOf[A](tpe: Type[A], value: Expr[A]): Expr[String] = {
    implicit val A: Type[A] = tpe
    implicit val StringT: Type[String] = Type.of[String]
    implicit val AnyT: Type[Any] = Type.of[Any]
    if (Type[A] <:< Type[String]) value.upcast[String]
    else
      Type[A] match {
        case IsValueType(isValueType) if !textValueBuiltIns.exists { b =>
              import b.Underlying as B
              Type[A] <:< Type[B]
            } =>
          import isValueType.Underlying as Inner
          xmlTextOf(Type[Inner], isValueType.value.unwrap(value))
        case _ =>
          val any: Expr[Any] = value.upcast[Any]
          Expr.quote(Expr.splice(any).toString)
      }
  }
}
