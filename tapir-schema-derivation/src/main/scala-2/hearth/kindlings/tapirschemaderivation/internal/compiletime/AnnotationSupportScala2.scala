package hearth.kindlings.tapirschemaderivation
package internal.compiletime

import hearth.MacroCommonsScala2

trait AnnotationSupportScala2 extends AnnotationSupport { this: MacroCommonsScala2 =>
  import c.universe.*

  override protected def findAnnotationOfType[Ann: Type](param: Parameter): Option[UntypedExpr] = {
    val annTpe = UntypedType.fromTyped[Ann]
    param.asUntyped.symbol.annotations.collectFirst {
      case ann if ann.tree.tpe =:= annTpe => c.untypecheck(ann.tree)
    }
  }

  override protected def findTypeAnnotationOfType[Ann: Type, A: Type]: Option[UntypedExpr] = {
    val annTpe = UntypedType.fromTyped[Ann]
    val aTpe = UntypedType.fromTyped[A]
    aTpe.typeSymbol.annotations.collectFirst {
      case ann if ann.tree.tpe =:= annTpe => c.untypecheck(ann.tree)
    }
  }

  override protected def extractStringLiteralFromAnnotation(annotation: UntypedExpr): Option[String] =
    annotation match {
      case Apply(_, List(Literal(Constant(value: String)))) => Some(value)
      case _                                                => None
    }

  override protected def allParamAnnotations(param: Parameter): List[UntypedExpr] =
    param.asUntyped.symbol.annotations.map(ann => c.untypecheck(ann.tree))

  override protected def allTypeAnnotations[A: Type]: List[UntypedExpr] = {
    val aTpe = UntypedType.fromTyped[A]
    aTpe.typeSymbol.annotations.map(ann => c.untypecheck(ann.tree))
  }
}
