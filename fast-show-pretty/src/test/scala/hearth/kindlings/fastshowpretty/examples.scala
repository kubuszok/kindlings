package hearth.kindlings.fastshowpretty

case class Person(name: String, age: Int)
case class Empty()
case class Single(value: Int)
case class Address(street: String, city: String)
case class PersonWithAddress(name: String, age: Int, address: Address)
case class Team(name: String, members: List[Person])
case class Tree(value: Int, children: List[Tree])
final case class ExampleValueClass(a: Int) extends AnyVal
final case class WrappedString(s: String) extends AnyVal
class NotAHandledType

import hearth.kindlings.fastshowpretty.annotations.sensitiveData

case class UserWithPassword(name: String, @sensitiveData password: String)
case class UserWithPII(name: String, @sensitiveData("PII") email: String, age: Int)
@sensitiveData case class CreditCard(number: String, cvv: String)
@sensitiveData("financial data") case class BankAccount(iban: String, balance: Double)

@sensitiveData sealed trait SecretEnum
case class SecretA(value: Int) extends SecretEnum
case class SecretB(name: String) extends SecretEnum

sealed trait Shape
case class Circle(radius: Double) extends Shape
case class Rectangle(width: Double, height: Double) extends Shape

sealed trait SimpleEnum
case object Yes extends SimpleEnum
case object No extends SimpleEnum
