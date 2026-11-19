package hearth.kindlings.oracle.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.macros.named as uNamed
import hearth.kindlings.jsoniterderivation.annotations.named as kNamed

// ADT and generic-type fixtures of jsoniter-scala's `JsonCodecMakerSpec` (see `jsoniterUpstreamSpecFixtures.scala`);
// upstream declares most of them locally in its tests, here each test's types live in an object named after it.

object JUExtra { // "deserialize ADTs with extra fields"
  sealed trait Base
  final case class A(x: Int) extends Base
  final case class B() extends Base
  case object C extends Base
}

object JUOpenapi { // plain (non-case) objects
  sealed trait Enum
  object EnumValue1 extends Enum {
    override def toString: String = "EnumValue1"
  }
  object EnumValue2 extends Enum {
    override def toString: String = "EnumValue2"
  }
}

object JUCirce {
  sealed trait Enum
  object EnumValue extends Enum {
    override def toString: String = "EnumValue"
  }
  sealed trait Base
  final case class AbbA(x: Int = 1, xs: List[Int], oX: Option[Enum]) extends Base
}

object JUCustomName {
  sealed abstract class Base extends Product with Serializable
  final case class A(b: B) extends Base
  final case class B(c: String) extends Base
}

object JUNamedLeaves {
  sealed abstract class Base extends Product with Serializable
  @kNamed("X") @uNamed("X") final case class A(b: B) extends Base
  @kNamed("Y") @uNamed("Y") final case class B(c: String) extends Base
}

object JUNonAscii {
  sealed abstract class База extends Product with Serializable
  final case class А(б: Б) extends База
  final case class Б(с: String) extends База
}

object JUOperatorNames {
  sealed trait TimeZone extends Product with Serializable
  case object `US/Alaska` extends TimeZone
  case object `Europe/Paris` extends TimeZone
}

object JUDeep {
  sealed trait Base extends Product with Serializable
  sealed trait Base2 extends Base
  sealed trait Base3 extends Base2
  sealed abstract class Base4 extends Base3
  sealed abstract class Base5 extends Base4
  final case class A(a: Int) extends Base3
  final case class B(b: String) extends Base5
}

object JUMixed {
  sealed trait Base extends Product with Serializable
  sealed trait Base2 extends Base
  final case class A(a: Int) extends Base with Base2
  final case class B(b: String) extends Base with Base2
}

object JUDiamond {
  sealed trait Base extends Product with Serializable
  sealed trait Base1 extends Base
  sealed trait Base2 extends Base
  final case class A(a: Int) extends Base1 with Base2
  final case class B(b: String) extends Base1 with Base2
}

object JUNonCaseLeaves {
  sealed trait X
  class A() extends X {
    override def hashCode(): Int = 1
    override def equals(obj: Any): Boolean = obj.isInstanceOf[A]
    override def toString: String = "A()"
  }
  object B extends X {
    override def toString: String = "B"
  }
}

object JUSealedClass {
  sealed class A {
    override def hashCode(): Int = 1
    override def equals(obj: Any): Boolean = obj.isInstanceOf[A]
    override def toString: String = "A()"
  }
  final case class B(n: Int) extends A
  final case class C(s: String) extends A
}

final case class JUHintFoo(hint: String)

final case class JUBasket[T <: JUFruit[T]](fruits: List[T])

object JUTypeAliases {
  type I = Int
  type S = String
  type L = List[I]
  type M = Map[I, S]
  final case class TypeAliases(i: I, s: S, l: L, m: M)
}

final case class JUId[A](id: A) extends AnyVal

final case class JUFirstOrderType[A, B](a: A, b: B, oa: Option[A], bs: List[B])

object JUGadt {
  sealed trait GADT[A] extends Product with Serializable
  case object Foo extends GADT[Boolean]
  case object Bar extends GADT[Unit]
  case object Baz extends GADT[Int]
  case object Qux extends GADT[String]
}

object JUHk {
  sealed trait Foo[A[_]] extends Product with Serializable
  final case class Bar[A[_]](a: A[Int]) extends Foo[A]
  final case class Baz[A[_]](a: A[String]) extends Foo[A]
}

object JUFreeGadt {
  sealed trait GADT[A] extends Product with Serializable
  final case class FixedType(value: Int) extends GADT[Int]
  final case class VarType[B](value: B) extends GADT[B]
}

final case class JUHigherKindedType[F[_]](f: F[Int], fs: F[JUHigherKindedType[F]])

final case class JUAuxiliaryConstructor(i: Int, s: String = "") {
  def this(s: String) = this(0, s)
}

final case class JUMultiListOfArgs(i: Int)(val l: Long)(var s: String) {
  override def toString: String = s"JUMultiListOfArgs($i)($l)($s)"
}

final case class JUMultiListOfArgsDefault(i: Int = 1)(val l: Long)(var s: String) {
  override def toString: String = s"JUMultiListOfArgsDefault($i)($l)($s)"
}

class JUNonCaseClass(val id: Int, var name: String) {
  override def hashCode(): Int = id * 31 + java.util.Objects.hashCode(name)
  override def equals(obj: Any): Boolean = obj match {
    case c: JUNonCaseClass => id == c.id && java.util.Objects.equals(name, c.name)
    case _                 => false
  }
  override def toString: String = s"NonCaseClass(id=$id,name=$name)"
}
