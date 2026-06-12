package hearth.kindlings.derivation.compiletime

import hearth.MacroCommons
import hearth.std.*

/** Cross-platform access to annotations on constructor parameters and types, plus literal extraction from annotation
  * trees.
  *
  * Platform implementations live in [[AnnotationSupportScala2]] / [[AnnotationSupportScala3]]; each derivation module
  * re-exports all three traits via thin delegates in its own `internal.compiletime` package (same approach as
  * [[LoadStandardExtensionsOnce]]).
  *
  * This trait exists only because Hearth currently has no typed cross-platform annotation API — see
  * `docs/research/hearth-gap-annotation-extraction.md`. Once Hearth provides one, this whole hierarchy should be
  * deleted.
  */
trait AnnotationSupport { this: MacroCommons & StdExtensions =>

  protected def findAnnotationOfType[Ann: Type](param: Parameter): Option[UntypedExpr]

  protected def findTypeAnnotationOfType[Ann: Type, A: Type]: Option[UntypedExpr]

  protected def findAllAnnotationsOfType[Ann: Type](param: Parameter): List[UntypedExpr]

  protected def findAllTypeAnnotationsOfType[Ann: Type, A: Type]: List[UntypedExpr]

  /** Collect ALL annotations on a parameter (regardless of type). */
  protected def allParamAnnotations(param: Parameter): List[UntypedExpr]

  /** Collect ALL annotations on a type (regardless of type). */
  protected def allTypeAnnotations[A: Type]: List[UntypedExpr]

  protected def extractStringLiteralFromAnnotation(annotation: UntypedExpr): Option[String]

  protected def extractIntLiteralFromAnnotation(annotation: UntypedExpr): Option[Int]

  protected def extractTwoStringLiteralsFromAnnotation(annotation: UntypedExpr): Option[(String, String)]

  protected def extractTwoIntLiteralsFromAnnotation(annotation: UntypedExpr): Option[(Int, Int)]

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

  final def getAnnotationTwoIntArgs[Ann: Type](param: Parameter): Option[(Int, Int)] =
    findAnnotationOfType[Ann](param).flatMap(extractTwoIntLiteralsFromAnnotation)

  final def getAllAnnotationTwoStringArgs[Ann: Type](param: Parameter): List[(String, String)] =
    findAllAnnotationsOfType[Ann](param).flatMap(extractTwoStringLiteralsFromAnnotation)

  final def getAllTypeAnnotationTwoStringArgs[Ann: Type, A: Type]: List[(String, String)] =
    findAllTypeAnnotationsOfType[Ann, A].flatMap(extractTwoStringLiteralsFromAnnotation)
}
