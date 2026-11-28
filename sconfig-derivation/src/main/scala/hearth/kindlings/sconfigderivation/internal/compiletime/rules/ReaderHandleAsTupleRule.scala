package hearth.kindlings.sconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.instances.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.sconfigderivation.{ConfigDecodingError, ConfigReader}
import hearth.kindlings.sconfigderivation.internal.runtime.SConfigDerivationUtils
import org.ekrich.config.{ConfigList, ConfigValue}

trait ReaderHandleAsTupleRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  /** Tuples are read the way upstream PureConfig's derivations read them: from a list of exactly N elements
    * (`WrongSizeList` otherwise, `WrongType` when the value is neither a list nor an object), every element read with
    * the reader of its type and every failure reported under its index.
    *
    * A value that is not a list but is an object is read from a `TupleN`'s product representation (`{ _1 = ..., _2 =
    * ... }`), as upstream's Scala 2 derivation does.
    *
    * Scala 3 tuples which are not a `TupleN` (`EmptyTuple`, `H *: T`, more than 22 elements) are read only from lists,
    * as upstream's Scala 3 derivation reads them.
    */
  object ReaderHandleAsTupleRule extends ReaderDerivationRule("handle as tuple when possible") {

    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a tuple") >> {
        if (!Type[A].isTuple || Type[A].isNamedTuple)
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a tuple"))
        else
          genericTupleElementTypes[A] match {
            case Some(elementTypes) => decodeGenericTuple[A](elementTypes).map(Rule.matched)
            case None               =>
              CaseClass.parse[A].toEither match {
                case Right(caseClass) =>
                  NonEmptyList.fromList(
                    caseClass.primaryConstructor.totalParameters.flatten.toList.zipWithIndex
                  ) match {
                    case Some(elements) => decodeTuple[A](caseClass, elements).map(Rule.matched)
                    case None           => MIO.pure(Rule.yielded(s"The tuple ${Type[A].prettyPrint} has no elements"))
                  }
                case Left(reason) => MIO.pure(Rule.yielded(reason))
              }
          }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeTuple[A: ReaderCtx](
        caseClass: CaseClass[A],
        elements: NonEmptyList[((String, Parameter), Int)]
    ): MIO[Expr[Either[ConfigDecodingError, A]]] = {
      implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
      implicit val ConfigListT: Type[ConfigList] = RTypes.ConfigList
      implicit val ErrorT: Type[ConfigDecodingError] = RTypes.ConfigDecodingError
      implicit val IntT: Type[Int] = RTypes.Int
      implicit val AnyT: Type[Any] = RTypes.Any
      implicit val EitherErrorAnyT: Type[Either[ConfigDecodingError, Any]] = RTypes.EitherErrorAny
      implicit val EitherErrorA: Type[Either[ConfigDecodingError, A]] = RTypes.ReaderResult[A]

      val size = elements.toList.size

      elements
        .parTraverse { case ((name, param), index) =>
          import param.tpe.Underlying as Element
          Log.namedScope(s"Deriving reader for tuple element $name: ${Type[Element].prettyPrint}") {
            ReaderHandleAsCaseClassRule.deriveFieldReader[Element].map { readerExpr =>
              val argument: Expr[Any] => (String, Expr_??) = { value =>
                (name, ReaderHandleAsCaseClassRule.castTo[Element](value).as_??)
              }
              (readElement[Element](readerExpr, index), argument)
            }
          }
        }
        .flatMap { elementData =>
          val reads = elementData.toList.map(_._1)
          val arguments = elementData.toList.map(_._2)

          def construct(values: List[Expr[Any]]): Expr[A] =
            foldInstanceFree(caseClass.primaryConstructor, "Constructor")(
              onTypes = _ => Map.empty,
              onValues = _ => arguments.zip(values).map { case (argument, value) => argument(value) }.toMap
            ) match {
              case Right(constructed) => constructed.value.asInstanceOf[Expr[A]]
              case Left(error)        =>
                Environment.reportErrorAndAbort(
                  ReaderDerivationError
                    .CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
                    .message
                )
            }

          // The product representation, read like any case class (keys `_1`, `_2`, ... through the field mapping).
          ReaderHandleAsCaseClassRule.decodeCaseClassFields[A](caseClass).map { asObject =>
            Expr.quote {
              SConfigDerivationUtils.tupleList(Expr.splice(rctx.value), Expr.splice(Expr(size))) match {
                case Right(tupleElements) => Expr.splice(readElements[A](reads, construct)(Expr.quote(tupleElements)))
                case Left(tupleFailure)   =>
                  if (SConfigDerivationUtils.isTupleAsObject(Expr.splice(rctx.value))) Expr.splice(asObject)
                  else Left(tupleFailure): Either[ConfigDecodingError, A]
              }
            }
          }
        }
    }

    /** A Scala 3 tuple which is not a `TupleN` (`EmptyTuple`, `H *: T`, more than 22 elements): read only from a list,
      * like upstream's Scala 3 derivation reads every tuple, and built with `Tuple.fromArray`.
      */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeGenericTuple[A: ReaderCtx](elementTypes: List[??]): MIO[Expr[Either[ConfigDecodingError, A]]] = {
      implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
      implicit val ConfigListT: Type[ConfigList] = RTypes.ConfigList
      implicit val ErrorT: Type[ConfigDecodingError] = RTypes.ConfigDecodingError
      implicit val EitherErrorA: Type[Either[ConfigDecodingError, A]] = RTypes.ReaderResult[A]

      val size = elementTypes.size

      elementTypes.zipWithIndex
        .parTraverse { case (elementType, index) =>
          import elementType.Underlying as Element
          Log.namedScope(s"Deriving reader for tuple element ${index + 1}: ${Type[Element].prettyPrint}") {
            ReaderHandleAsCaseClassRule.deriveFieldReader[Element].map(readElement[Element](_, index))
          }
        }
        .map { reads =>
          def construct(values: List[Expr[Any]]): Expr[A] =
            constructGenericTuple[A](values).getOrElse(
              Environment.reportErrorAndAbort(
                ReaderDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false).message
              )
            )

          Expr.quote {
            SConfigDerivationUtils.tupleList(Expr.splice(rctx.value), Expr.splice(Expr(size))) match {
              case Right(tupleElements) => Expr.splice(readElements[A](reads, construct)(Expr.quote(tupleElements)))
              case Left(tupleFailure)   => Left(tupleFailure): Either[ConfigDecodingError, A]
            }
          }
        }
    }

    /** Reads the element at `index` of the tuple's list (failures reported under the index). */
    @scala.annotation.nowarn("msg=is never used")
    private def readElement[Element: Type](
        readerExpr: Expr[ConfigReader[Element]],
        index: Int
    ): Expr[ConfigList] => Expr[Either[ConfigDecodingError, Any]] = {
      implicit val ConfigListT: Type[ConfigList] = RTypes.ConfigList
      implicit val ConfigReaderElement: Type[ConfigReader[Element]] = RTypes.ConfigReader[Element]
      implicit val EitherErrorAnyT: Type[Either[ConfigDecodingError, Any]] = RTypes.EitherErrorAny
      list =>
        Expr.quote {
          SConfigDerivationUtils
            .readElement(Expr.splice(list), Expr.splice(Expr(index)), Expr.splice(readerExpr))
            .asInstanceOf[Either[ConfigDecodingError, Any]]
        }
    }

    /** Reads every element (flat: one local `def` and one statement per element) and builds the tuple from them. */
    @scala.annotation.nowarn("msg=is never used")
    private def readElements[A: ReaderCtx](
        reads: List[Expr[ConfigList] => Expr[Either[ConfigDecodingError, Any]]],
        construct: List[Expr[Any]] => Expr[A]
    )(list: Expr[ConfigList]): Expr[Either[ConfigDecodingError, A]] = {
      implicit val ErrorT: Type[ConfigDecodingError] = RTypes.ConfigDecodingError
      implicit val AnyT: Type[Any] = RTypes.Any
      implicit val EitherErrorAnyT: Type[Either[ConfigDecodingError, Any]] = RTypes.EitherErrorAny
      implicit val EitherErrorA: Type[Either[ConfigDecodingError, A]] = RTypes.ReaderResult[A]
      constructAccumulating[Either[ConfigDecodingError, Any], ConfigDecodingError, A, Either[
        ConfigDecodingError,
        A
      ]](reads.map(_(list)))(
        mergeErrors =
          (failure, r) => Expr.quote(SConfigDerivationUtils.mergeFailure(Expr.splice(failure), Expr.splice(r))),
        valueOf = r => Expr.quote(Expr.splice(r).asInstanceOf[Right[ConfigDecodingError, Any]].value),
        construct = construct,
        onSuccess = a => Expr.quote(Right(Expr.splice(a)): Either[ConfigDecodingError, A]),
        onFailure = failure => Expr.quote(Left(Expr.splice(failure)): Either[ConfigDecodingError, A])
      )
    }
  }
}
