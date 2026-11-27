package hearth.kindlings.pureconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.pureconfigderivation.internal.runtime.PureConfigDerivationUtils
import pureconfig.{ConfigCursor, ConfigListCursor}
import pureconfig.error.ConfigReaderFailures

trait ReaderHandleAsTupleRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  /** Tuples (`TupleN`) are read the way upstream pureconfig's derivations read them: from a list of exactly N elements
    * (`WrongSizeList` otherwise, `WrongType` when the value is neither a list nor an object), every element read with
    * the reader of its type and every failure reported.
    *
    * A value that is not a list but is an object is read from the tuple's product representation (`{ _1 = ..., _2 =
    * ... }`), as upstream's Shapeless derivation (Scala 2) does; upstream's Scala 3 derivation reads only lists.
    */
  object ReaderHandleAsTupleRule extends ReaderDerivationRule("handle as tuple when possible") {

    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigReaderFailures, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a tuple") >> {
        if (!Type[A].isTuple || Type[A].isNamedTuple)
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a tuple"))
        else
          CaseClass.parse[A].toEither match {
            case Right(caseClass) =>
              NonEmptyList.fromList(caseClass.primaryConstructor.totalParameters.flatten.toList.zipWithIndex) match {
                case Some(elements) => decodeTuple[A](caseClass, elements).map(Rule.matched)
                case None           => MIO.pure(Rule.yielded(s"The tuple ${Type[A].prettyPrint} has no elements"))
              }
            case Left(reason) => MIO.pure(Rule.yielded(reason))
          }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeTuple[A: ReaderCtx](
        caseClass: CaseClass[A],
        elements: NonEmptyList[((String, Parameter), Int)]
    ): MIO[Expr[Either[ConfigReaderFailures, A]]] = {
      implicit val ConfigCursorT: Type[ConfigCursor] = RTypes.ConfigCursor
      implicit val ConfigListCursorT: Type[ConfigListCursor] = RTypes.ConfigListCursor
      implicit val FailuresT: Type[ConfigReaderFailures] = RTypes.ConfigReaderFailures
      implicit val IntT: Type[Int] = RTypes.Int
      implicit val AnyT: Type[Any] = RTypes.Any
      implicit val EitherFailuresAnyT: Type[Either[ConfigReaderFailures, Any]] = RTypes.EitherFailuresAny
      implicit val EitherFailuresA: Type[Either[ConfigReaderFailures, A]] = RTypes.ReaderResult[A]

      val size = elements.toList.size

      elements
        .parTraverse { case ((name, param), index) =>
          import param.tpe.Underlying as Element
          Log.namedScope(s"Deriving reader for tuple element $name: ${Type[Element].prettyPrint}") {
            ReaderHandleAsCaseClassRule.deriveFieldReader[Element].map { readerExpr =>
              val read: Expr[ConfigListCursor] => Expr[Either[ConfigReaderFailures, Any]] = { listCursor =>
                Expr.quote {
                  Expr
                    .splice(readerExpr)
                    .from(Expr.splice(listCursor).atIndexOrUndefined(Expr.splice(Expr(index))))
                    .asInstanceOf[Either[ConfigReaderFailures, Any]]
                }
              }
              val argument: Expr[Any] => (String, Expr_??) = { value =>
                (name, ReaderHandleAsCaseClassRule.castTo[Element](value).as_??)
              }
              (read, argument)
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
                  ReaderDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error)).message
                )
            }

          def readList(listCursor: Expr[ConfigListCursor]): Expr[Either[ConfigReaderFailures, A]] =
            constructAccumulating[Either[ConfigReaderFailures, Any], ConfigReaderFailures, A, Either[
              ConfigReaderFailures,
              A
            ]](reads.map(_(listCursor)))(
              mergeErrors = (failures, r) =>
                Expr.quote(PureConfigDerivationUtils.mergeFieldFailures(Expr.splice(failures), Expr.splice(r))),
              valueOf = r => Expr.quote(Expr.splice(r).asInstanceOf[Right[ConfigReaderFailures, Any]].value),
              construct = construct,
              onSuccess = a => Expr.quote(Right(Expr.splice(a)): Either[ConfigReaderFailures, A]),
              onFailure = failures => Expr.quote(Left(Expr.splice(failures)): Either[ConfigReaderFailures, A])
            )

          // The product representation, read like any case class (keys `_1`, `_2`, ... through the field mapping).
          ReaderHandleAsCaseClassRule.decodeCaseClassFields[A](caseClass).map { asObject =>
            Expr.quote {
              PureConfigDerivationUtils.tupleListCursor(Expr.splice(rctx.cursor), Expr.splice(Expr(size))) match {
                case Right(tupleElements) => Expr.splice(readList(Expr.quote(tupleElements)))
                case Left(tupleFailures)  =>
                  if (PureConfigDerivationUtils.isTupleAsObject(Expr.splice(rctx.cursor))) Expr.splice(asObject)
                  else Left(tupleFailures): Either[ConfigReaderFailures, A]
              }
            }
          }
        }
    }
  }
}
