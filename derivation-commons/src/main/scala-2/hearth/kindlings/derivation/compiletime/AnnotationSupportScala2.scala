package hearth.kindlings.derivation.compiletime

import hearth.MacroCommonsScala2

trait AnnotationSupportScala2 extends AnnotationSupport { this: MacroCommonsScala2 =>
  import c.universe.*

  // Access annotations directly from the symbol to preserve type information.
  // Hearth's param.asUntyped.annotations strips types via c.untypecheck,
  // making typed comparison impossible. We bypass that by reading the raw
  // symbol annotations where .tree.tpe is still intact.
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

  override protected def findAllAnnotationsOfType[Ann: Type](param: Parameter): List[UntypedExpr] = {
    val annTpe = UntypedType.fromTyped[Ann]
    param.asUntyped.symbol.annotations.collect {
      case ann if ann.tree.tpe =:= annTpe => c.untypecheck(ann.tree)
    }
  }

  override protected def findAllTypeAnnotationsOfType[Ann: Type, A: Type]: List[UntypedExpr] = {
    val annTpe = UntypedType.fromTyped[Ann]
    val aTpe = UntypedType.fromTyped[A]
    aTpe.typeSymbol.annotations.collect {
      case ann if ann.tree.tpe =:= annTpe => c.untypecheck(ann.tree)
    }
  }

  override protected def allParamAnnotations(param: Parameter): List[UntypedExpr] =
    param.asUntyped.symbol.annotations.map(ann => c.untypecheck(ann.tree))

  override protected def allTypeAnnotations[A: Type]: List[UntypedExpr] = {
    val aTpe = UntypedType.fromTyped[A]
    aTpe.typeSymbol.annotations.map(ann => c.untypecheck(ann.tree))
  }

  override protected def extractStringLiteralFromAnnotation(annotation: UntypedExpr): Option[String] =
    annotation match {
      case Apply(_, List(Literal(Constant(value: String)))) => Some(value)
      case _                                                => None
    }

  override protected def extractIntLiteralFromAnnotation(annotation: UntypedExpr): Option[Int] =
    annotation match {
      case Apply(_, List(Literal(Constant(value: Int)))) => Some(value)
      case _                                             => None
    }

  override protected def extractTwoStringLiteralsFromAnnotation(annotation: UntypedExpr): Option[(String, String)] =
    annotation match {
      case Apply(_, List(Literal(Constant(v1: String)), Literal(Constant(v2: String)))) => Some((v1, v2))
      case _                                                                            => None
    }

  override protected def extractTwoIntLiteralsFromAnnotation(annotation: UntypedExpr): Option[(Int, Int)] =
    annotation match {
      case Apply(_, List(Literal(Constant(v1: Int)), Literal(Constant(v2: Int)))) => Some((v1, v2))
      case _                                                                      => None
    }
}
