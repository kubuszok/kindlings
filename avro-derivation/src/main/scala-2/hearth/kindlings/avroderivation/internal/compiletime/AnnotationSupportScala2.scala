package hearth.kindlings.avroderivation
package internal.compiletime

import hearth.MacroCommonsScala2

trait AnnotationSupportScala2 extends AnnotationSupport { this: MacroCommonsScala2 =>
  import c.universe._

  override protected def findAnnotationOfType[Ann: Type](param: Parameter): Option[UntypedExpr] = {
    val annTpe = UntypedType.fromTyped[Ann]
    param.asUntyped.symbol.annotations.collectFirst {
      case ann if ann.tree.tpe =:= annTpe => c.untypecheck(ann.tree)
    }
  }

  override protected def extractStringLiteralFromAnnotation(annotation: UntypedExpr): Option[String] =
    annotation match {
      case Apply(_, List(Literal(Constant(value: String)))) => Some(value)
      case _                                                => None
    }
}
