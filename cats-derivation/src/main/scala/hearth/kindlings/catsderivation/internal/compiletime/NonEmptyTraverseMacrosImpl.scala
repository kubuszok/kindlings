package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.catsderivation.LogDerivation

/** NonEmptyTraverse derivation: nonEmptyTraverse + traverse + reduceLeftTo + reduceRightTo + foldLeft + foldRight +
  * map.
  *
  * Extends both Traverse and Reducible. The key method `nonEmptyTraverse` uses Apply[G] (not Applicative[G]) since the
  * structure is guaranteed non-empty. Uses the same LambdaBuilder-based extract/reconstruct approach as Traverse.
  *
  * For nonEmptyTraverse, instead of seeding with G.pure (which requires Applicative), we seed with the first field
  * mapped through f (giving G[B]), then combine remaining fields using G.map2.
  */
trait NonEmptyTraverseMacrosImpl extends CatsDerivationTimeout { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  def deriveNonEmptyTraverse[F[_]](
      FCtor0: Type.Ctor1[F],
      NETFType: Type[cats.NonEmptyTraverse[F]]
  ): Expr[cats.NonEmptyTraverse[F]] = {
    val macroName = "NonEmptyTraverse.derived"

    implicit val FCtor: Type.Ctor1[F] = FCtor0
    implicit val NETFT: Type[cats.NonEmptyTraverse[F]] = NETFType
    implicit val AnyType: Type[Any] = NETTypes.Any
    implicit val FAnyType: Type[F[Any]] = FCtor.apply[Any]
    implicit val EvalAnyType: Type[cats.Eval[Any]] = NETTypes.EvalAny
    implicit val ListAnyType: Type[List[Any]] = NETTypes.ListAny

    Log
      .namedScope(s"Deriving NonEmptyTraverse at: ${Environment.currentPosition.prettyPrint}") {
        CaseClass.parse[F[Any]].toEither match {
          case Right(caseClass) =>
            MIO.scoped { runSafe =>
              implicit val IntType: Type[Int] = NETTypes.Int
              implicit val StringType: Type[String] = NETTypes.String

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
                  // Invariant
                } else {
                  nestedFields += name
                }
              }

              if (nestedFields.nonEmpty) {
                throw new RuntimeException(
                  s"Cannot derive NonEmptyTraverse: fields ${nestedFields.mkString(", ")} contain nested type constructors. " +
                    "Only direct type parameter fields (A) and invariant fields are supported."
                )
              }

              if (directFields.isEmpty) {
                throw new RuntimeException(
                  "Cannot derive NonEmptyTraverse: no direct type parameter fields found. " +
                    "NonEmptyTraverse requires at least one field of the type parameter."
                )
              }

              val directFieldSet: Set[String] = directFields.toSet

              // Pre-load extensions eagerly
              val _ = runSafe {
                Environment.loadStandardExtensions().toMIO(allowFailures = false)
              }

              // Extract direct fields: Any => List[Any]
              val extractDirect: Expr[Any => List[Any]] = runSafe {
                deriveNETExtractDirectFields[F](caseClass, directFieldSet)
              }

              // Reconstruct: (List[Any], Any) => Any
              val reconstructFn: Expr[(List[Any], Any) => Any] = runSafe {
                deriveNETReconstructFromList[F](caseClass, directFieldSet)
              }

              // Map body
              val doMap: (Expr[F[Any]], Expr[Any => Any]) => Expr[F[Any]] = (faExpr, fExpr) =>
                runSafe {
                  deriveNETMapBody[F](caseClass, directFieldSet, faExpr, fExpr)
                }

              // FoldLeft body
              val doFoldLeft: (Expr[F[Any]], Expr[Any], Expr[(Any, Any) => Any]) => Expr[Any] =
                (faExpr, bExpr, fExpr) =>
                  runSafe {
                    deriveNETFoldLeftBody[F](caseClass, directFieldSet, faExpr, bExpr, fExpr)
                  }

              // FoldRight body
              val doFoldRight
                  : (Expr[F[Any]], Expr[cats.Eval[Any]], Expr[(Any, cats.Eval[Any]) => cats.Eval[Any]]) => Expr[
                    cats.Eval[Any]
                  ] =
                (faExpr, lbExpr, fExpr) =>
                  runSafe {
                    deriveNETFoldRightBody[F](caseClass, directFieldSet, faExpr, lbExpr, fExpr)
                  }

              // ReduceLeftTo body
              val doReduceLeftTo: (Expr[F[Any]], Expr[Any => Any], Expr[(Any, Any) => Any]) => Expr[Any] =
                (faExpr, fExpr, gExpr) =>
                  runSafe {
                    deriveNETReduceLeftToBody[F](caseClass, directFieldSet, faExpr, fExpr, gExpr)
                  }

              // ReduceRightTo body
              val doReduceRightTo
                  : (Expr[F[Any]], Expr[Any => Any], Expr[(Any, cats.Eval[Any]) => cats.Eval[Any]]) => Expr[
                    cats.Eval[Any]
                  ] =
                (faExpr, fExpr, gExpr) =>
                  runSafe {
                    deriveNETReduceRightToBody[F](caseClass, directFieldSet, faExpr, fExpr, gExpr)
                  }

              Expr.quote {
                new cats.NonEmptyTraverse[F] {
                  def nonEmptyTraverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit
                      G: cats.Apply[G]
                  ): G[F[B]] = {
                    val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                    val extract: Any => List[Any] = Expr.splice(extractDirect)
                    val recon: (List[Any], Any) => Any = Expr.splice(reconstructFn)
                    val _ = anyFa
                    val _ = extract
                    val _ = recon
                    val directValues: List[Any] = extract(anyFa)
                    // Non-empty: seed with first element mapped through f, combine rest with G.map2
                    val gSeed: G[List[Any]] =
                      G.map(f(directValues.head.asInstanceOf[A]).asInstanceOf[G[Any]])((a: Any) => List(a))
                    val gList: G[List[Any]] = directValues.tail.foldLeft(gSeed) { (gacc, v) =>
                      G.map2(gacc, f(v.asInstanceOf[A]).asInstanceOf[G[Any]]) { (acc, a) =>
                        acc :+ (a: Any)
                      }
                    }
                    G.map(gList)(newVals => recon(newVals, anyFa)).asInstanceOf[G[F[B]]]
                  }

                  override def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit
                      G: cats.Applicative[G]
                  ): G[F[B]] = {
                    val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                    val extract: Any => List[Any] = Expr.splice(extractDirect)
                    val recon: (List[Any], Any) => Any = Expr.splice(reconstructFn)
                    val _ = anyFa
                    val _ = extract
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

                  def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B = {
                    val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                    val anyF: Any => Any = f.asInstanceOf[Any => Any]
                    val anyG: (Any, Any) => Any = g.asInstanceOf[(Any, Any) => Any]
                    val _ = anyFa
                    val _ = anyF
                    val _ = anyG
                    Expr
                      .splice(doReduceLeftTo(Expr.quote(anyFa), Expr.quote(anyF), Expr.quote(anyG)))
                      .asInstanceOf[B]
                  }

                  def reduceRightTo[A, B](fa: F[A])(f: A => B)(
                      g: (A, cats.Eval[B]) => cats.Eval[B]
                  ): cats.Eval[B] = {
                    val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
                    val anyF: Any => Any = f.asInstanceOf[Any => Any]
                    val anyG: (Any, cats.Eval[Any]) => cats.Eval[Any] =
                      g.asInstanceOf[(Any, cats.Eval[Any]) => cats.Eval[Any]]
                    val _ = anyFa
                    val _ = anyF
                    val _ = anyG
                    Expr
                      .splice(doReduceRightTo(Expr.quote(anyFa), Expr.quote(anyF), Expr.quote(anyG)))
                      .asInstanceOf[cats.Eval[B]]
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
        infoRendering = if (shouldWeLogNETDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogNETDerivation) RenderFrom(Log.Level.Info) else DontRender,
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
  private def deriveNETExtractDirectFields[F[_]](
      caseClass: CaseClass[F[Any]],
      directFields: Set[String]
  )(implicit
      FCtor: Type.Ctor1[F],
      FAnyType: Type[F[Any]],
      AnyType: Type[Any],
      ListAnyType: Type[List[Any]]
  ): MIO[Expr[Any => List[Any]]] = {
    val lambda = LambdaBuilder.of1[Any]("fa").buildWith { faExpr0 =>
      val faExpr: Expr[F[Any]] = Expr.quote(Expr.splice(faExpr0).asInstanceOf[F[Any]])
      val fields = caseClass.caseFieldValuesAt(faExpr).toList
      val directExprs: List[Expr[Any]] = fields.collect {
        case (fieldName, fieldValue) if directFields.contains(fieldName) =>
          import fieldValue.Underlying as Field
          fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any]
      }
      directExprs.foldRight(Expr.quote(Nil: List[Any])) { (elem, acc) =>
        Expr.quote(Expr.splice(elem) :: Expr.splice(acc))
      }
    }
    MIO.pure(lambda)
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveNETReconstructFromList[F[_]](
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
  private def deriveNETMapBody[F[_]](
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
      case Left(error)          => MIO.fail(new RuntimeException(s"Cannot construct mapped result: $error"))
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveNETFoldLeftBody[F[_]](
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
  private def deriveNETFoldRightBody[F[_]](
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

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveNETReduceLeftToBody[F[_]](
      caseClass: CaseClass[F[Any]],
      directFields: Set[String],
      faExpr: Expr[F[Any]],
      fExpr: Expr[Any => Any],
      gExpr: Expr[(Any, Any) => Any]
  )(implicit FCtor: Type.Ctor1[F], FAnyType: Type[F[Any]], AnyType: Type[Any]): MIO[Expr[Any]] = {
    val fields = caseClass.caseFieldValuesAt(faExpr).toList
    val directFieldExprs: List[Expr[Any]] = fields.collect {
      case (fieldName, fieldValue) if directFields.contains(fieldName) =>
        import fieldValue.Underlying as Field
        fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any]
    }
    val head = directFieldExprs.head
    val tail = directFieldExprs.tail
    val seed: Expr[Any] = Expr.quote(Expr.splice(fExpr)(Expr.splice(head)))
    val result = tail.foldLeft(seed) { (acc, fieldExpr) =>
      Expr.quote(Expr.splice(gExpr)(Expr.splice(acc), Expr.splice(fieldExpr)))
    }
    MIO.pure(result)
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def deriveNETReduceRightToBody[F[_]](
      caseClass: CaseClass[F[Any]],
      directFields: Set[String],
      faExpr: Expr[F[Any]],
      fExpr: Expr[Any => Any],
      gExpr: Expr[(Any, cats.Eval[Any]) => cats.Eval[Any]]
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
    val last = directFieldExprs.last
    val init = directFieldExprs.init
    val seed: Expr[cats.Eval[Any]] = Expr.quote(cats.Eval.now(Expr.splice(fExpr)(Expr.splice(last)): Any))
    val result = init.foldRight(seed) { (fieldExpr, acc) =>
      Expr.quote(Expr.splice(gExpr)(Expr.splice(fieldExpr), Expr.splice(acc)))
    }
    MIO.pure(result)
  }

  protected object NETTypes {
    val Any: Type[Any] = Type.of[Any]
    val Int: Type[Int] = Type.of[Int]
    val String: Type[String] = Type.of[String]
    def EvalAny: Type[cats.Eval[Any]] = Type.of[cats.Eval[Any]]
    def ListAny: Type[List[Any]] = Type.of[List[Any]]
    val LogDerivation: Type[hearth.kindlings.catsderivation.LogDerivation] =
      Type.of[hearth.kindlings.catsderivation.LogDerivation]
  }

  def shouldWeLogNETDerivation: Boolean = {
    implicit val LogDerivationType: Type[LogDerivation] = NETTypes.LogDerivation
    Expr.summonImplicit[LogDerivation].isDefined || (for {
      data <- Environment.typedSettings.toOption
      cd <- data.get("catsDerivation")
      shouldLog <- cd.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)
  }
}
