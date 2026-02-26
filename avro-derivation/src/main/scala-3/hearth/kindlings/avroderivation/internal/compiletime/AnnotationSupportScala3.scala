package hearth.kindlings.avroderivation
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

  override protected def extractIntLiteralFromAnnotation(annotation: UntypedExpr): Option[Int] =
    annotation match {
      case Apply(_, List(Literal(IntConstant(value)))) => Some(value)
      case _                                           => None
    }

  override protected def findAllAnnotationsOfType[Ann: Type](param: Parameter): List[UntypedExpr] = {
    val annTpe = UntypedType.fromTyped[Ann]
    param.asUntyped.annotations.filter(_.tpe =:= annTpe).toList
  }

  override protected def findAllTypeAnnotationsOfType[Ann: Type, A: Type]: List[UntypedExpr] = {
    val annTpe = UntypedType.fromTyped[Ann]
    val aTpe = UntypedType.fromTyped[A]
    aTpe.typeSymbol.annotations.filter(_.tpe =:= annTpe).toList
  }

  override protected def extractTwoStringLiteralsFromAnnotation(annotation: UntypedExpr): Option[(String, String)] =
    annotation match {
      case Apply(_, List(Literal(StringConstant(v1)), Literal(StringConstant(v2)))) => Some((v1, v2))
      case _                                                                        => None
    }
}
