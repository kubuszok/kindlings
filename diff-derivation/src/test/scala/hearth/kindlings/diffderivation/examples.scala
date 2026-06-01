package hearth.kindlings.diffderivation

case class Person(name: String, age: Int)
case class Address(street: String, city: String)
case class PersonWithAddress(person: Person, address: Address)
case class Team(name: String, members: List[Person])
case class Config(settings: Map[String, Int])
case class Tree(value: Int, children: List[Tree])

sealed trait Shape
case class Circle(radius: Double) extends Shape
case class Rectangle(width: Double, height: Double) extends Shape

sealed trait SimpleEnum
case object Yes extends SimpleEnum
case object No extends SimpleEnum

sealed trait TreeNode
case class Branch(left: TreeNode, right: TreeNode) extends TreeNode
case class Leaf(value: Int) extends TreeNode

// Combinatorial wrapper x inner-type test types
case class CombOuter(
    optCaseClass: Option[Person],
    optSealedTrait: Option[Shape],
    listCaseClass: List[Person],
    mapCaseClass: Map[String, Person]
)
