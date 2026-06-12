package hearth.kindlings.derivation.compiletime

import hearth.MacroCommons
import hearth.std.*

/** Cross-platform access to annotations on constructor parameters and types, plus literal extraction from annotation
  * trees.
  *
  * As of Hearth issue #283, this is implemented entirely on top of Hearth's typed cross-platform annotation API
  * (`Parameter#annotationsOfType` / `Type#annotationsOfType` for `<:<` lookup, and
  * `Annotations.decodedConstructorArguments` for literal extraction). No platform-specific code is required — the
  * former `AnnotationSupportScala2` / `AnnotationSupportScala3` halves are obsolete.
  */
trait AnnotationSupport { this: MacroCommons & StdExtensions =>

  // ----- Lookup (was abstract / platform-specific; now shared via Hearth's typed annotation API) -----

  protected def findAnnotationOfType[Ann: Type](param: Parameter): Option[Expr[Ann]] =
    param.annotationsOfType[Ann].headOption

  protected def findTypeAnnotationOfType[Ann: Type, A: Type]: Option[Expr[Ann]] =
    Type[A].annotationsOfType[Ann].headOption

  protected def findAllAnnotationsOfType[Ann: Type](param: Parameter): List[Expr[Ann]] =
    param.annotationsOfType[Ann]

  protected def findAllTypeAnnotationsOfType[Ann: Type, A: Type]: List[Expr[Ann]] =
    Type[A].annotationsOfType[Ann]

  // ----- Literal extraction from annotation constructor arguments (shared) -----

  private def decodedArgs[Ann](annotation: Expr[Ann]): List[Either[String, Any]] =
    Annotations.decodedConstructorArguments(annotation).getOrElse(Nil)

  protected def extractStringLiteralFromAnnotation[Ann](annotation: Expr[Ann]): Option[String] =
    decodedArgs(annotation) match {
      case List(Right(value: String)) => Some(value)
      case _                          => None
    }

  protected def extractIntLiteralFromAnnotation[Ann](annotation: Expr[Ann]): Option[Int] =
    decodedArgs(annotation) match {
      case List(Right(value: Int)) => Some(value)
      case _                       => None
    }

  protected def extractTwoStringLiteralsFromAnnotation[Ann](annotation: Expr[Ann]): Option[(String, String)] =
    decodedArgs(annotation) match {
      case List(Right(v1: String), Right(v2: String)) => Some((v1, v2))
      case _                                          => None
    }

  protected def extractTwoIntLiteralsFromAnnotation[Ann](annotation: Expr[Ann]): Option[(Int, Int)] =
    decodedArgs(annotation) match {
      case List(Right(v1: Int), Right(v2: Int)) => Some((v1, v2))
      case _                                    => None
    }

  // ----- Public API (signatures unchanged; call sites unaffected) -----

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
