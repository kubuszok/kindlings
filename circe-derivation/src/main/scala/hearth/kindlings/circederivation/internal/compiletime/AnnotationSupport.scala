package hearth.kindlings.circederivation.internal.compiletime

import hearth.MacroCommons
import hearth.std.*

trait AnnotationSupport { this: MacroCommons & StdExtensions =>

  protected def findAnnotationOfType[Ann: Type](param: Parameter): Option[UntypedExpr]

  protected def extractStringLiteralFromAnnotation(annotation: UntypedExpr): Option[String]

  final def hasAnnotationType[Ann: Type](param: Parameter): Boolean =
    findAnnotationOfType[Ann](param).isDefined

  final def getAnnotationStringArg[Ann: Type](param: Parameter): Option[String] =
    findAnnotationOfType[Ann](param).flatMap(extractStringLiteralFromAnnotation)
}
