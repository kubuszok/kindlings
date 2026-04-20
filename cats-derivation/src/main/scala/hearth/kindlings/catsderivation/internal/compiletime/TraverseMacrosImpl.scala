package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** Traverse derivation: traverse + map + foldLeft + foldRight.
  *
  * Uses an erased approach with LambdaBuilder for the traverse body. Since G[_] is unknown at macro time, we build two
  * helper functions at macro time:
  *   - extractDirect: F[Any] => List[Any] — extracts direct type parameter fields
  *   - reconstructFromList: (List[Any], F[Any]) => F[Any] — reconstructs from new direct field values + original
  *     (invariant fields)
  *
  * These are spliced at the top level of the traverse method body, then used with Applicative[G] operations at runtime.
  */
trait TraverseMacrosImpl extends CatsDerivationTimeout { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveTraverse[F[_]](
      FCtor0: Type.Ctor1[F],
      TraverseFType: Type[cats.Traverse[F]]
  ): Expr[cats.Traverse[F]] = {
    val macroName = "Traverse.derived"

    implicit val FCtor: Type.Ctor1[F] = FCtor0
    implicit val TraverseFT: Type[cats.Traverse[F]] = TraverseFType
    implicit val AnyType: Type[Any] = TraverseTypes.Any
    implicit val FAnyType: Type[F[Any]] = FCtor.apply[Any]
    implicit val EvalAnyType: Type[cats.Eval[Any]] = TraverseTypes.EvalAny
    implicit val ListAnyType: Type[List[Any]] = TraverseTypes.ListAny

    Log
      .namedScope(s"Deriving Traverse at: ${Environment.currentPosition.prettyPrint}") {
        CaseClass.parse[F[Any]].toEither match {
          case Right(caseClass) =>
            MIO.scoped { runSafe =>
              implicit val IntType: Type[Int] = TraverseTypes.Int
              implicit val StringType: Type[String] = TraverseTypes.String

              val ccInt = CaseClass.parse(using FCtor.apply[Int]).toEither match {
                case Right(cc) => cc
                case Left(e)   => throw new RuntimeException(s"Cannot parse F[Int]: $e")
              }
              val ccString = CaseClass.parse(using FCtor.apply[String]).toEither match {
                case Right(cc) => cc
                case Left(e)   => throw new RuntimeException(s"Cannot parse F[String]: $e")
              }

              val fieldsInt = ccInt.primaryConstructor.parameters.flatten.toList
              val fieldsString = ccString.primaryConstructor.parameters.flatten.toList

              val directFields = scala.collection.mutable.Set.empty[String]
              val nestedFields = scala.collection.mutable.ListBuffer.empty[String]

              fieldsInt.zip(fieldsString).foreach { case ((name, pInt), (_, pString)) =>
                val tInt = pInt.tpe.Underlying
                val tString = pString.tpe.Underlying
                if (tInt =:= IntType && tString =:= StringType) {
                  directFields += name
                } else if (tInt =:= tString) {
                  // Invariant — skip in traverse
                } else {
                  nestedFields += name
                }
              }

              if (nestedFields.nonEmpty) {
                throw new RuntimeException(
                  s"Cannot derive Traverse: fields ${nestedFields.mkString(", ")} contain nested type constructors. " +
                    "Only direct type parameter fields (A) and invariant fields are supported."
                )
              }

              val directFieldSet: Set[String] = directFields.toSet

              // Pre-load extensions eagerly to avoid Scala 3 sibling splice isolation issues
              val _ = runSafe {
                Environment.loadStandardExtensions().toMIO(allowFailures = false)
              }

              // Build extraction lambda: Any => List[Any]
              // Uses Any instead of F[Any] to avoid F references inside lambdas on Scala 2
              val extractDirect: Expr[Any => List[Any]] = runSafe {
                deriveExtractDirectFields[F](caseClass, directFieldSet)
              }

              // Build reconstruction lambda: (List[Any], Any) => Any
              // Uses Any instead of F[Any] to avoid F references inside lambdas on Scala 2
              val reconstructFn: Expr[(List[Any], Any) => Any] = runSafe {
                deriveReconstructFromList[F](caseClass, directFieldSet)
              }

              // Map body (same as Functor)
              val doMap: (Expr[F[Any]], Expr[Any => Any]) => Expr[F[Any]] = (faExpr, fExpr) =>
                runSafe {
                  deriveTraverseMapBody[F](caseClass, directFieldSet, faExpr, fExpr)
                }

              // FoldLeft body (same as Foldable)
              val doFoldLeft: (Expr[F[Any]], Expr[Any], Expr[(Any, Any) => Any]) => Expr[Any] =
                (faExpr, bExpr, fExpr) =>
                  runSafe {
                    deriveTraverseFoldLeftBody[F](caseClass, directFieldSet, faExpr, bExpr, fExpr)
                  }

              // FoldRight body (same as Foldable)
              val doFoldRight
                  : (Expr[F[Any]], Expr[cats.Eval[Any]], Expr[(Any, cats.Eval[Any]) => cats.Eval[Any]]) => Expr[
                    cats.Eval[Any]
                  ] =
                (faExpr, lbExpr, fExpr) =>
                  runSafe {
                    deriveTraverseFoldRightBody[F](caseClass, directFieldSet, faExpr, lbExpr, fExpr)
                  }

              Expr.quote {
                new cats.Traverse[F] {
                  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit
                      G: cats.Applicative[G]
                  ): G[F[B]] = {
                    val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                    val _ = anyFa
                    val extract: Any => List[Any] = Expr.splice(extractDirect)
                    val _ = extract
                    val recon: (List[Any], Any) => Any = Expr.splice(reconstructFn)
                    val _ = recon
                    val directValues: List[Any] = extract(anyFa)
                    val gList: G[List[Any]] =
                      directValues.foldRight(G.pure(Nil: List[Any])) { (v, gacc) =>
                        G.map2(f(v.asInstanceOf[A]).asInstanceOf[G[Any]], gacc) { (a, acc) =>
                          (a: Any) :: acc
                        }
                      }
                    G.map(gList)(newVals => recon(newVals, anyFa)).asInstanceOf[G[F[B]]]
                  }

                  override def map[A, B](fa: F[A])(f: A => B): F[B] = {
                    val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                    val anyF: Any => Any = f.asInstanceOf[Any => Any]
                    val _ = anyFa
                    val _ = anyF
                    Expr.splice(doMap(Expr.quote(anyFa), Expr.quote(anyF))).asInstanceOf[F[B]]
                  }

                  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B = {
                    val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                    val anyB: Any = b.asInstanceOf[Any]
                    val anyF: (Any, Any) => Any = f.asInstanceOf[(Any, Any) => Any]
                    val _ = anyFa
                    val _ = anyB
                    val _ = anyF
                    Expr
                      .splice(doFoldLeft(Expr.quote(anyFa), Expr.quote(anyB), Expr.quote(anyF)))
                      .asInstanceOf[B]
                  }

                  def foldRight[A, B](fa: F[A], lb: cats.Eval[B])(
                      f: (A, cats.Eval[B]) => cats.Eval[B]
                  ): cats.Eval[B] = {
                    val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                    val anyLb: cats.Eval[Any] = lb.asInstanceOf[cats.Eval[Any]]
                    val anyF: (Any, cats.Eval[Any]) => cats.Eval[Any] =
                      f.asInstanceOf[(Any, cats.Eval[Any]) => cats.Eval[Any]]
                    val _ = anyFa
                    val _ = anyLb
                    val _ = anyF
                    Expr
                      .splice(doFoldRight(Expr.quote(anyFa), Expr.quote(anyLb), Expr.quote(anyF)))
                      .asInstanceOf[cats.Eval[B]]
                  }
                }
              }
            }
          case Left(reason) =>
            MIO.fail(
              new RuntimeException(
                s"$macroName: Cannot derive for type: $reason. Can only be derived for case classes."
              )
            )
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogTraverseDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogTraverseDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveExtractDirectFields[F[_]](
      caseClass: CaseClass[F[Any]],
      directFields: Set[String]
  )(implicit
      FCtor: Type.Ctor1[F],
      FAnyType: Type[F[Any]],
      AnyType: Type[Any],
      ListAnyType: Type[List[Any]]
  ): MIO[Expr[Any => List[Any]]] = {
    val lambda = LambdaBuilder.of1[Any]("fa").buildWith { faExpr0 =>
      // Cast the Any parameter to F[Any] to access case class fields
      val faExpr: Expr[F[Any]] = Expr.quote(Expr.splice(faExpr0).asInstanceOf[F[Any]])
      val fields = caseClass.caseFieldValuesAt(faExpr).toList
      val directExprs: List[Expr[Any]] = fields.collect {
        case (fieldName, fieldValue) if directFields.contains(fieldName) =>
          import fieldValue.Underlying as Field
          fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any]
      }
      // Build: field1 :: field2 :: ... :: Nil
      directExprs.foldRight(Expr.quote(Nil: List[Any])) { (elem, acc) =>
        Expr.quote(Expr.splice(elem) :: Expr.splice(acc))
      }
    }
    MIO.pure(lambda)
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveReconstructFromList[F[_]](
      caseClass: CaseClass[F[Any]],
      directFields: Set[String]
  )(implicit
      FCtor: Type.Ctor1[F],
      FAnyType: Type[F[Any]],
      AnyType: Type[Any],
      ListAnyType: Type[List[Any]]
  ): MIO[Expr[(List[Any], Any) => Any]] = {
    val lambda =
      LambdaBuilder.of2[List[Any], Any]("newVals", "original").buildWith { case (newValsExpr, originalExpr) =>
        // Cast the Any parameter to F[Any] to access case class fields
        val faExpr: Expr[F[Any]] = Expr.quote(Expr.splice(originalExpr).asInstanceOf[F[Any]])
        val fields = caseClass.caseFieldValuesAt(faExpr).toList
        var currentList: Expr[List[Any]] = newValsExpr
        val fieldExprs: List[(String, Expr_??)] = fields.map { case (fieldName, fieldValue) =>
          import fieldValue.Underlying as Field
          if (directFields.contains(fieldName)) {
            val headExpr: Expr[Any] = Expr.quote(Expr.splice(currentList).head)
            val tailExpr: Expr[List[Any]] = Expr.quote(Expr.splice(currentList).tail)
            currentList = tailExpr
            (fieldName, headExpr.as_??)
          } else {
            (fieldName, fieldValue.value.asInstanceOf[Expr[Field]].as_??)
          }
        }
        caseClass.primaryConstructor(fieldExprs.toMap) match {
          case Right(constructExpr) => constructExpr.upcast[Any]
          case Left(error)          =>
            throw new RuntimeException(s"Cannot construct traversed result: $error")
        }
      }
    MIO.pure(lambda)
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveTraverseMapBody[F[_]](
      caseClass: CaseClass[F[Any]],
      directFields: Set[String],
      faExpr: Expr[F[Any]],
      fExpr: Expr[Any => Any]
  )(implicit FCtor: Type.Ctor1[F], FAnyType: Type[F[Any]], AnyType: Type[Any]): MIO[Expr[F[Any]]] = {
    val fields = caseClass.caseFieldValuesAt(faExpr).toList

    val mappedFields: List[(String, Expr_??)] = fields.map { case (fieldName, fieldValue) =>
      import fieldValue.Underlying as Field
      val fieldExpr = fieldValue.value.asInstanceOf[Expr[Field]]

      if (directFields.contains(fieldName)) {
        val mapped: Expr[Any] = Expr.quote(Expr.splice(fExpr)(Expr.splice(fieldExpr.upcast[Any])))
        (fieldName, mapped.as_??)
      } else {
        (fieldName, fieldExpr.as_??)
      }
    }

    caseClass.primaryConstructor(mappedFields.toMap) match {
      case Right(constructExpr) => MIO.pure(constructExpr)
      case Left(error)          =>
        MIO.fail(new RuntimeException(s"Cannot construct mapped result: $error"))
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveTraverseFoldLeftBody[F[_]](
      caseClass: CaseClass[F[Any]],
      directFields: Set[String],
      faExpr: Expr[F[Any]],
      bExpr: Expr[Any],
      fExpr: Expr[(Any, Any) => Any]
  )(implicit FCtor: Type.Ctor1[F], FAnyType: Type[F[Any]], AnyType: Type[Any]): MIO[Expr[Any]] = {
    val fields = caseClass.caseFieldValuesAt(faExpr).toList

    val directFieldExprs: List[Expr[Any]] = fields.collect {
      case (fieldName, fieldValue) if directFields.contains(fieldName) =>
        import fieldValue.Underlying as Field
        fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any]
    }

    val result = directFieldExprs.foldLeft(bExpr) { (acc, fieldExpr) =>
      Expr.quote(Expr.splice(fExpr)(Expr.splice(acc), Expr.splice(fieldExpr)))
    }

    MIO.pure(result)
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveTraverseFoldRightBody[F[_]](
      caseClass: CaseClass[F[Any]],
      directFields: Set[String],
      faExpr: Expr[F[Any]],
      lbExpr: Expr[cats.Eval[Any]],
      fExpr: Expr[(Any, cats.Eval[Any]) => cats.Eval[Any]]
  )(implicit
      FCtor: Type.Ctor1[F],
      FAnyType: Type[F[Any]],
      AnyType: Type[Any],
      EvalAnyType: Type[cats.Eval[Any]]
  ): MIO[Expr[cats.Eval[Any]]] = {
    val fields = caseClass.caseFieldValuesAt(faExpr).toList

    val directFieldExprs: List[Expr[Any]] = fields.collect {
      case (fieldName, fieldValue) if directFields.contains(fieldName) =>
        import fieldValue.Underlying as Field
        fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any]
    }

    val result = directFieldExprs.foldRight(lbExpr) { (fieldExpr, acc) =>
      Expr.quote(Expr.splice(fExpr)(Expr.splice(fieldExpr), Expr.splice(acc)))
    }

    MIO.pure(result)
  }

  protected object TraverseTypes {
    val Any: Type[Any] = Type.of[Any]
    val Int: Type[Int] = Type.of[Int]
    val String: Type[String] = Type.of[String]
    def EvalAny: Type[cats.Eval[Any]] = Type.of[cats.Eval[Any]]
    def ListAny: Type[List[Any]] = Type.of[List[Any]]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogTraverseDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = TraverseTypes.LogDerivation
    Expr.summonImplicit[LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("catsDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}
