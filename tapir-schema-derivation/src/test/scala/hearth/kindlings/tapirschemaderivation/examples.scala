package hearth.kindlings.tapirschemaderivation

import sttp.tapir.Schema.annotations.*
import sttp.tapir.Validator

case class SimplePerson(name: String, age: Int)
case class Nested(person: SimplePerson, note: String)
case class CamelCasePerson(firstName: String, lastName: String)

sealed trait Shape
case class Circle(radius: Double) extends Shape
case class Rectangle(width: Double, height: Double) extends Shape

// Tapir annotation test types

@description("A person with metadata")
@title("PersonMeta")
case class AnnotatedPerson(@description("The name") name: String, @format("int32") age: Int)

case class WithEncodedName(@encodedName("user_name") userName: String, normalField: Int)

@deprecated
case class DeprecatedType(value: String)

case class WithHiddenField(visible: String, @hidden secret: String)

case class WithValidation(@validate(Validator.min(0)) age: Int)

case class WithOptional(required: String, optional: Option[Int])

case class WithCollections(tags: List[String], counts: Vector[Int])

case class WithMap(metadata: Map[String, String])

case class RecursiveTree(value: Int, children: List[RecursiveTree])
