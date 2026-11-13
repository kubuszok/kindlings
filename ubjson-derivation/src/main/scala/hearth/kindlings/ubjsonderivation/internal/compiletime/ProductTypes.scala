package hearth.kindlings.ubjsonderivation.internal.compiletime

import hearth.MacroCommons
import hearth.std.*

import scala.collection.immutable.ListMap

/** Classes encoded as JSON objects of their primary-constructor parameters, as in jsoniter-scala: case classes and
  * non-abstract Scala classes (`class C(val a: Int, var b: String)`), with any number of parameter lists.
  *
  * Every encoded parameter is read through its public accessor: the case field, or the `val`/`var` getter of a
  * parameter of a non-case class or of a non-first parameter list of a case class.
  */
trait ProductTypes { this: MacroCommons & StdExtensions =>

  final protected class ProductType[A](
      val tpe: Type[A],
      val primaryConstructor: Method,
      val caseClass: Option[CaseClass[A]]
  ) {

    /** All primary-constructor parameters (of all parameter lists), in declaration order. */
    lazy val params: List[(String, Parameter)] = primaryConstructor.totalParameters.flatten.toList

    /** Values of the parameters which have an accessible accessor, read off `instance`, in declaration order. */
    def fieldValuesAt(instance: Expr[A]): ListMap[String, Expr_??] = {
      val caseValues: ListMap[String, Expr_??] = caseClass.fold(ListMap.empty[String, Expr_??])(_.caseFieldValuesAt(instance))
      ListMap.from(params.flatMap { case (name, _) =>
        caseValues.get(name).orElse(accessorValue(name, instance)).map(name -> _)
      })
    }

    private def accessorValue(name: String, instance: Expr[A]): Option[Expr_??] = {
      Method
        .unsortedMethodsNamed[A](name)(tpe)
        .collectFirst {
          case oi: Method.OnInstance if oi.isNullary && !oi.asUntyped.isConstructor && oi.isAvailable(AtCallSite) => oi
        }
        .flatMap { oi =>
          oi.apply(instance.asInstanceOf[Expr[oi.Instance]]) match {
            case r: Method.Result[?] =>
              import r.Returned
              r.build().toOption.map(_.as_??)
            case _ => None
          }
        }
    }
  }

  protected object ProductType {

    /** A case class, or (as in jsoniter-scala) a non-abstract, non-sealed Scala class with an accessible primary
      * constructor. Non-abstract sealed classes are ADT bases (handled by the enum rule, which encodes the base itself
      * as one of its leaves through [[asLeaf]]).
      */
    def parse[A: Type]: Either[String, ProductType[A]] =
      CaseClass.parse[A].toEither match {
        case Right(cc)    => Right(new ProductType[A](Type[A], cc.primaryConstructor, Some(cc)))
        case Left(reason) =>
          if (Type[A].isSealed) Left(reason)
          else asLeaf[A].toRight(reason)
      }

    /** A non-abstract Scala class as a product, also when it is sealed. */
    def asLeaf[A: Type]: Option[ProductType[A]] =
      CaseClass.parse[A].toOption.map(cc => new ProductType[A](Type[A], cc.primaryConstructor, Some(cc))).orElse {
        if (isPlainClass[A])
          Type[A].primaryConstructor
            .filter(_.isAvailable(AtCallSite))
            .map(ctor => new ProductType[A](Type[A], ctor, None))
        else None
      }

    private def isPlainClass[A: Type]: Boolean = {
      val t = Type[A]
      t.isClass && !t.isAbstract && !t.isTrait && !t.isObject && !t.isCase && !t.isPrimitive && !t.isArray &&
      !t.isIArray && !t.isTuple && !t.isNamedTuple && !t.isOpaqueType && !t.isFunction && !t.isJavaEnum &&
      !t.isJavaEnumValue && !t.isEnumeration && !t.isJvmBuiltIn && !t.isTypeSystemSpecial && !t.isUnionType
    }
  }
}
