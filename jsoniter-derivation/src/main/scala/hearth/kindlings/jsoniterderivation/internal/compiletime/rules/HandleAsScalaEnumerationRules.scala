package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterScalaEnumeration
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonWriter}

/** `scala.Enumeration` values, as in jsoniter-scala: written and read by name (`toString`) or by id
  * (`useScalaEnumValueId`), looked up among the values the enumeration has at runtime (see
  * [[JsoniterScalaEnumeration]]): the values do not have to be public `val`s known to the macro (private, lazy and
  * mutable ones, values created by methods and values added after the codec was created work too).
  */
trait HandleAsScalaEnumerationRulesImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  /** The `scala.Enumeration` object of the enumeration value type `A` (`Obj.Value`, or an alias of it), when `A` is
    * referred to through a stable path.
    */
  protected def enumerationObjectOf[A: Type]: Option[Expr[Enumeration]]

  /** The per-codec-instance [[JsoniterScalaEnumeration]] of `A` (a cached `lazy val`: a plain `val` among the cached
    * defs would be a forward reference), if `A` is an enumeration value type whose enumeration object is known.
    */
  private[compiletime] def scalaEnumerationOf[A: Type](
      cache: MLocal[ValDefsCache]
  ): MIO[Option[Expr[JsoniterScalaEnumeration[A]]]] =
    if (!Type[A].isEnumeration) MIO.pure(None)
    else
      enumerationObjectOf[A] match {
        case None             => MIO.pure(None)
        case Some(enumObject) =>
          implicit val SupportT: Type[JsoniterScalaEnumeration[A]] = Type.of[JsoniterScalaEnumeration[A]]
          val key = "cached-scala-enumeration"
          cache.get0Ary[JsoniterScalaEnumeration[A]](key).flatMap {
            case found @ Some(_) => MIO.pure(found)
            case None            =>
              cache.buildCachedWith(
                key,
                ValDefBuilder.ofLazy[JsoniterScalaEnumeration[A]](s"enumeration_${Type[A].shortName}")
              )(_ => Expr.quote(new JsoniterScalaEnumeration[A](Expr.splice(enumObject)))) >>
                cache.get0Ary[JsoniterScalaEnumeration[A]](key)
          }
      }

  private def notAnEnumeration[A: Type]: String =
    s"${Type[A].prettyPrint} is not a scala.Enumeration value with a known enumeration object"

  object EncoderHandleAsScalaEnumerationRule
      extends EncoderDerivationRule("handle as scala.Enumeration value when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a scala.Enumeration value") >>
        scalaEnumerationOf[A](ectx.cache).map {
          case None          => Rule.yielded(notAnEnumeration[A])
          case Some(support) =>
            implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
            def byName: Expr[Unit] = Expr.quote {
              Expr.splice(support).writeVal(Expr.splice(ectx.value), Expr.splice(ectx.writer))
            }
            // ids need no lookup: written inline
            def byId: Expr[Unit] = ectx.stringifiedMode match {
              case Some(true) =>
                Expr.quote {
                  Expr
                    .splice(ectx.writer)
                    .writeValAsString(Expr.splice(ectx.value).asInstanceOf[Enumeration#Value].id)
                }
              case Some(false) =>
                Expr.quote {
                  Expr.splice(ectx.writer).writeVal(Expr.splice(ectx.value).asInstanceOf[Enumeration#Value].id)
                }
              case None =>
                Expr.quote {
                  Expr
                    .splice(support)
                    .writeIdVal(Expr.splice(ectx.value), Expr.splice(ectx.writer), Expr.splice(ectx.stringifiedExpr))
                }
            }
            Rule.matched(ectx.evaluatedConfig.map(_.useScalaEnumValueId) match {
              case Some(true)  => byId
              case Some(false) => byName
              case None        =>
                Expr.quote {
                  if (Expr.splice(ectx.config).useScalaEnumValueId) Expr.splice(byId) else Expr.splice(byName)
                }
            })
        }
  }

  object DecoderHandleAsScalaEnumerationRule
      extends DecoderDerivationRule("handle as scala.Enumeration value when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a scala.Enumeration value") >>
        scalaEnumerationOf[A](dctx.cache).map {
          case None          => Rule.yielded(notAnEnumeration[A])
          case Some(support) =>
            implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
            // `null` as the default: a JSON `null` is an error here (the defaults of fields are applied around the
            // field's decoder).
            def byName: Expr[A] = Expr.quote {
              Expr.splice(support).readVal(Expr.splice(dctx.reader), null.asInstanceOf[A])
            }
            def byId: Expr[A] = Expr.quote {
              Expr
                .splice(support)
                .readIdVal(Expr.splice(dctx.reader), null.asInstanceOf[A], Expr.splice(dctx.stringifiedExpr))
            }
            Rule.matched(dctx.evaluatedConfig.map(_.useScalaEnumValueId) match {
              case Some(true)  => byId
              case Some(false) => byName
              case None        =>
                Expr.quote {
                  if (Expr.splice(dctx.config).useScalaEnumValueId) Expr.splice(byId) else Expr.splice(byName)
                }
            })
        }
  }

  /** The key encoder of an enumeration value type (names, or ids with `useScalaEnumValueId`). */
  private[compiletime] def scalaEnumerationKeyEncoder[K: Type](implicit
      ctx: EncoderCtx[?]
  ): MIO[Option[Expr[(K, JsonWriter) => Unit]]] = {
    implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
    implicit val UnitT: Type[Unit] = CTypes.Unit
    scalaEnumerationOf[K](ctx.cache).flatMap {
      case None          => MIO.pure(None)
      case Some(support) =>
        LambdaBuilder
          .of2[K, JsonWriter]("key", "writer")
          .traverse { case (keyExpr, writerExpr) =>
            def byName: Expr[Unit] = Expr.quote {
              Expr.splice(support).writeKey(Expr.splice(keyExpr), Expr.splice(writerExpr))
            }
            def byId: Expr[Unit] = Expr.quote {
              Expr.splice(writerExpr).writeKey(Expr.splice(keyExpr).asInstanceOf[Enumeration#Value].id)
            }
            MIO.pure(ctx.evaluatedConfig.map(_.useScalaEnumValueId) match {
              case Some(true)  => byId
              case Some(false) => byName
              case None        =>
                Expr.quote {
                  if (Expr.splice(ctx.config).useScalaEnumValueId) Expr.splice(byId) else Expr.splice(byName)
                }
            })
          }
          .map(builder => Some(builder.build[Unit]): Option[Expr[(K, JsonWriter) => Unit]])
    }
  }

  /** The key decoder of an enumeration value type (names, or ids with `useScalaEnumValueId`). */
  private[compiletime] def scalaEnumerationKeyDecoder[K: Type](implicit
      ctx: DecoderCtx[?]
  ): MIO[Option[Expr[JsonReader => K]]] = {
    implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
    scalaEnumerationOf[K](ctx.cache).flatMap {
      case None          => MIO.pure(None)
      case Some(support) =>
        LambdaBuilder
          .of1[JsonReader]("reader")
          .traverse { readerExpr =>
            def byName: Expr[K] = Expr.quote(Expr.splice(support).readKey(Expr.splice(readerExpr)))
            def byId: Expr[K] = Expr.quote(Expr.splice(support).readIdKey(Expr.splice(readerExpr)))
            MIO.pure(ctx.evaluatedConfig.map(_.useScalaEnumValueId) match {
              case Some(true)  => byId
              case Some(false) => byName
              case None        =>
                Expr.quote {
                  if (Expr.splice(ctx.config).useScalaEnumValueId) Expr.splice(byId) else Expr.splice(byName)
                }
            })
          }
          .map(builder => Some(builder.build[K]): Option[Expr[JsonReader => K]])
    }
  }
}
