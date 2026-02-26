package hearth.kindlings.avroderivation.internal.compiletime

import hearth.MacroCommons
import hearth.std.*

trait AnnotationSupport { this: MacroCommons & StdExtensions =>

  protected def findAnnotationOfType[Ann: Type](param: Parameter): Option[UntypedExpr]

  protected def findTypeAnnotationOfType[Ann: Type, A: Type]: Option[UntypedExpr]

  protected def findAllAnnotationsOfType[Ann: Type](param: Parameter): List[UntypedExpr]

  protected def findAllTypeAnnotationsOfType[Ann: Type, A: Type]: List[UntypedExpr]

  protected def extractStringLiteralFromAnnotation(annotation: UntypedExpr): Option[String]

  protected def extractIntLiteralFromAnnotation(annotation: UntypedExpr): Option[Int]

  protected def extractTwoStringLiteralsFromAnnotation(annotation: UntypedExpr): Option[(String, String)]

  final def hasAnnotationType[Ann: Type](param: Parameter): Boolean =
    findAnnotationOfType[Ann](param).isDefined

  final def hasTypeAnnotation[Ann: Type, A: Type]: Boolean =
    findTypeAnnotationOfType[Ann, A].isDefined

  final def getAnnotationStringArg[Ann: Type](param: Parameter): Option[String] =
    findAnnotationOfType[Ann](param).flatMap(extractStringLiteralFromAnnotation)

  final def getAnnotationIntArg[Ann: Type](param: Parameter): Option[Int] =
    findAnnotationOfType[Ann](param).flatMap(extractIntLiteralFromAnnotation)

  final def getTypeAnnotationStringArg[Ann: Type, A: Type]: Option[String] =
    findTypeAnnotationOfType[Ann, A].flatMap(extractStringLiteralFromAnnotation)

  final def getTypeAnnotationIntArg[Ann: Type, A: Type]: Option[Int] =
    findTypeAnnotationOfType[Ann, A].flatMap(extractIntLiteralFromAnnotation)

  final def getAllAnnotationStringArgs[Ann: Type](param: Parameter): List[String] =
    findAllAnnotationsOfType[Ann](param).flatMap(extractStringLiteralFromAnnotation)

  final def getAllTypeAnnotationStringArgs[Ann: Type, A: Type]: List[String] =
    findAllTypeAnnotationsOfType[Ann, A].flatMap(extractStringLiteralFromAnnotation)

  final def getAllAnnotationTwoStringArgs[Ann: Type](param: Parameter): List[(String, String)] =
    findAllAnnotationsOfType[Ann](param).flatMap(extractTwoStringLiteralsFromAnnotation)

  final def getAllTypeAnnotationTwoStringArgs[Ann: Type, A: Type]: List[(String, String)] =
    findAllTypeAnnotationsOfType[Ann, A].flatMap(extractTwoStringLiteralsFromAnnotation)
}
