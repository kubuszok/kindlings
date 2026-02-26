package hearth.kindlings.tapirschemaderivation
package internal.compiletime

import hearth.MacroCommonsScala3

trait AnnotationSupportScala3 extends AnnotationSupport { this: MacroCommonsScala3 =>
  import quotes.reflect.*

  override protected def findAnnotationOfType[Ann: Type](param: Parameter): Option[UntypedExpr] = {
    val annTpe = UntypedType.fromTyped[Ann]
    param.asUntyped.annotations.find { term =>
      term.tpe =:= annTpe
    }
  }

  override protected def findTypeAnnotationOfType[Ann: Type, A: Type]: Option[UntypedExpr] = {
    val annTpe = UntypedType.fromTyped[Ann]
    val aTpe = UntypedType.fromTyped[A]
    aTpe.typeSymbol.annotations.find { term =>
      term.tpe =:= annTpe
    }
  }

  override protected def extractStringLiteralFromAnnotation(annotation: UntypedExpr): Option[String] =
    annotation match {
      case Apply(_, List(Literal(StringConstant(value)))) => Some(value)
      case _                                              => None
    }

  override protected def allParamAnnotations(param: Parameter): List[UntypedExpr] =
    param.asUntyped.annotations.toList

  override protected def allTypeAnnotations[A: Type]: List[UntypedExpr] = {
    val aTpe = UntypedType.fromTyped[A]
    aTpe.typeSymbol.annotations.toList
  }
}
