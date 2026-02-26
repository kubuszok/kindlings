package hearth.kindlings.tapirschemaderivation.internal.compiletime

import hearth.MacroCommons
import hearth.std.*

trait AnnotationSupport { this: MacroCommons & StdExtensions =>

  protected def findAnnotationOfType[Ann: Type](param: Parameter): Option[UntypedExpr]

  protected def findTypeAnnotationOfType[Ann: Type, A: Type]: Option[UntypedExpr]

  protected def extractStringLiteralFromAnnotation(annotation: UntypedExpr): Option[String]

  /** Collect ALL annotations on a parameter (regardless of type). */
  protected def allParamAnnotations(param: Parameter): List[UntypedExpr]

  /** Collect ALL annotations on a type (regardless of type). */
  protected def allTypeAnnotations[A: Type]: List[UntypedExpr]

  final def hasAnnotationType[Ann: Type](param: Parameter): Boolean =
    findAnnotationOfType[Ann](param).isDefined

  final def getAnnotationStringArg[Ann: Type](param: Parameter): Option[String] =
    findAnnotationOfType[Ann](param).flatMap(extractStringLiteralFromAnnotation)

  final def getTypeAnnotationStringArg[Ann: Type, A: Type]: Option[String] =
    findTypeAnnotationOfType[Ann, A].flatMap(extractStringLiteralFromAnnotation)
}
