package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.derivation.compiletime.StableChildrenOrder
import hearth.kindlings.yamlderivation.YamlKeyEncoder
import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils
import org.virtuslab.yaml.Node

trait EncoderHandleAsMapRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  @scala.annotation.nowarn("msg=Infinite loop")
  object EncoderHandleAsMapRule extends EncoderDerivationRule("handle as map when possible") {
    implicit val NodeT: Type[Node] = Types.Node
    implicit val StringT: Type[String] = Types.String
    implicit val StringNodePairT: Type[(String, Node)] = Type.of[(String, Node)]

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Node]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a map") >> {
        Type[A] match {
          case IsMap(isMap) =>
            import isMap.Underlying as Pair
            deriveMapEntries[A, Pair](isMap.value)

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a map"))
        }
      }

    /** The built-in key encoders of `YamlKeyEncoder`'s companion: not user-provided (the macro writes these types
      * itself).
      */
    private lazy val builtInKeyEncoders: Seq[UntypedMethod] =
      Type.of[YamlKeyEncoder.type].unsortedMethods.collect {
        case method if method.isImplicit => method.asUntyped
      }

    /** A user-provided `YamlKeyEncoder[K]` in scope (not one of the built-in ones). */
    private def userKeyEncoder[K: Type]: Option[Expr[YamlKeyEncoder[K]]] =
      Types.YamlKeyEncoder[K].summonExprIgnoring(builtInKeyEncoders*).toOption

    /** The node of a map key: the scalar its key encoder (user-provided, built-in or derived for enums of case objects
      * and value types) writes; when it has none, a complex key, written with its `YamlEncoder` (user-provided or
      * derived), as scala-yaml writes every key.
      */
    private def deriveKeyNode[K: Type](key: Expr[K])(implicit ctx: EncoderCtx[?]): MIO[Expr[Node]] =
      deriveKeyString[K](key).flatMap {
        case Some(string) => MIO.pure(Expr.quote(YamlDerivationUtils.stringKeyNode(Expr.splice(string))))
        case None         =>
          Log.info(s"No key encoder for ${Type[K].prettyPrint}, encoding it as a complex key") >>
            deriveEncoderRecursively[K](using ctx.nest(key)).handleErrorWith { errors =>
              MIO.fail(EncoderDerivationError.NoKeyEncoder(Type[K].prettyPrint, errors.toList.map(_.getMessage)))
            }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveKeyString[K: Type](key: Expr[K])(implicit ctx: EncoderCtx[?]): MIO[Option[Expr[String]]] = {
      implicit val KeyEncoderK: Type[YamlKeyEncoder[K]] = Types.YamlKeyEncoder[K]
      userKeyEncoder[K] match {
        case Some(codec) =>
          Log.info(s"Using the user-provided YamlKeyEncoder[${Type[K].prettyPrint}]") >>
            MIO.pure(Some(Expr.quote(Expr.splice(codec).encodeKey(Expr.splice(key)))))
        case None =>
          if (Type[K] =:= Type[String]) MIO.pure(Some(key.asInstanceOf[Expr[String]]))
          else if (
            Type[K] =:= Types.Int || Type[K] =:= Types.Long || Type[K] =:= Types.Short || Type[K] =:= Types.Byte ||
            Type[K] =:= Types.Double || Type[K] =:= Types.Float || Type[K] =:= Types.Boolean ||
            Type[K] =:= Types.Char || Type[K] =:= Types.BigInt || Type[K] =:= Types.BigDecimal ||
            Type[K] =:= Types.UUID
          )
            // as scala-yaml's encoders of these types write them
            MIO.pure(Some(Expr.quote(Expr.splice(key).toString)))
          else
            Type[K] match {
              case IsValueType(isValueType) if !Type[K].isNamedTuple =>
                import isValueType.Underlying as Inner
                deriveKeyString[Inner](isValueType.value.unwrap(key))
              case _ =>
                Enum.parse[K].toOption match {
                  case Some(enumm) => deriveEnumKeyString[K](enumm, key)
                  case None        => MIO.pure(None)
                }
            }
      }
    }

    /** An enum of case objects (Scala 3 enum, Java enum, `scala.Enumeration`, sealed trait of case objects) written as
      * the (configured) name of the case.
      */
    private def deriveEnumKeyString[K: Type](enumm: Enum[K], key: Expr[K])(implicit
        ctx: EncoderCtx[?]
    ): MIO[Option[Expr[String]]] = {
      val children = StableChildrenOrder.directChildren(EncoderHandleAsMapRuleImpl.this)(enumm)
      val allCaseObjects = Type[K].isEnumeration || Type[K].isJavaEnum ||
        children.forall { case (_, child) => SingletonValue.unapply(child.Underlying).isDefined }
      if (children.isEmpty || !allCaseObjects) MIO.pure(None)
      else {
        val namesMIO: MIO[String => Expr[String]] = ctx.evaluatedConfig match {
          case Some(config) => MIO.pure((name: String) => Expr(config.transformConstructorNames(name)))
          case None         => ctx.transformedConstructorNames(children.map(_._1))
        }
        namesMIO.flatMap { nameOf =>
          enumm
            .parMatchOn[MIO, String](key) { matched =>
              import matched.Underlying as EnumCase
              val caseName: String = children
                .find { case (_, child) =>
                  import child.Underlying as ChildType
                  Type[EnumCase] <:< Type[ChildType]
                }
                .map(_._1)
                .getOrElse(Type[EnumCase].shortName)
              MIO.pure(nameOf(caseName))
            }
            .map(identity)
        }
      }
    }

    // Exposed so the combined collection-or-map rule can call it after a single IsCollection parse.
    private[rules] def deriveMapEntries[A: EncoderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Node]]] = {
      import isMap.{Key, Value}
      implicit val NodePairT: Type[(Node, Node)] = Type.of[(Node, Node)]
      LambdaBuilder
        .of1[Pair]("pair")
        .traverse { pairExpr =>
          val keyExpr = isMap.key(pairExpr)
          val valueExpr = isMap.value(pairExpr)
          // Keys are written with their key encoder (`YamlKeyEncoder`: user-provided, built-in or derived), or, for a
          // complex key type without one, with their `YamlEncoder`. Only keys of exactly `String` (not its subtypes)
          // without a user-provided `YamlKeyEncoder[String]` take the fast path below (the built-in `String` key
          // encoder, written as scala-yaml's `YamlEncoder.forString` writes a string).
          val keyNodeMIO: MIO[Expr[Node]] =
            if (Key =:= Type[String] && userKeyEncoder[String].isEmpty)
              MIO.pure(Expr.quote {
                YamlDerivationUtils.stringKeyNode(Expr.splice(keyExpr).asInstanceOf[String])
              })
            else deriveKeyNode[Key](keyExpr)
          keyNodeMIO.flatMap { keyNode =>
            deriveEncoderRecursively[Value](using ectx.nest(valueExpr)).map { valueNode =>
              Expr.quote {
                (Expr.splice(keyNode), Expr.splice(valueNode))
              }
            }
          }
        }
        .map { builder =>
          val pairLambda = builder.build[(Node, Node)]
          val iterableExpr = isMap.asIterable(ectx.value)
          Rule.matched(Expr.quote {
            YamlDerivationUtils.encodeMapEntries[Pair](
              Expr.splice(iterableExpr),
              Expr.splice(pairLambda)
            )
          })
        }
    }
  }
}
