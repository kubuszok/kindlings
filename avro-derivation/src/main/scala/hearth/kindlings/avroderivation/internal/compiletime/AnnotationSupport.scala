package hearth.kindlings.avroderivation.internal.compiletime

import hearth.MacroCommons
import hearth.std.*

trait AnnotationSupport { this: MacroCommons & StdExtensions =>

  protected def findAnnotationOfType[Ann: Type](param: Parameter): Option[UntypedExpr]

  protected def findTypeAnnotationOfType[Ann: Type, A: Type]: Option[UntypedExpr]

  protected def extractStringLiteralFromAnnotation(annotation: UntypedExpr): Option[String]

  protected def extractIntLiteralFromAnnotation(annotation: UntypedExpr): Option[Int]

  final def hasAnnotationType[Ann: Type](param: Parameter): Boolean =
    findAnnotationOfType[Ann](param).isDefined

  final def getAnnotationStringArg[Ann: Type](param: Parameter): Option[String] =
    findAnnotationOfType[Ann](param).flatMap(extractStringLiteralFromAnnotation)

  final def getAnnotationIntArg[Ann: Type](param: Parameter): Option[Int] =
    findAnnotationOfType[Ann](param).flatMap(extractIntLiteralFromAnnotation)

  final def getTypeAnnotationStringArg[Ann: Type, A: Type]: Option[String] =
    findTypeAnnotationOfType[Ann, A].flatMap(extractStringLiteralFromAnnotation)
}
