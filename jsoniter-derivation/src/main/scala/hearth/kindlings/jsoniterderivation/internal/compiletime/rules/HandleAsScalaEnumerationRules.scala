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

  /** As in jsoniter-scala, the public non-lazy `val`s of type `A` of its enumeration object, with the names of the
    * `val`s, in the order of declaration (empty when they are not known).
    */
  protected def enumerationStableValuesOf[A: Type]: List[(String, Expr[A])]

  /** Above this many stable values the names are looked up at runtime only (the generated dispatch over the stable
    * values grows with their number; see the bytecode size guards).
    */
  private val maxStableValuesWrittenAsConstants = 128

  private def isNonEscapedAscii(s: String): Boolean = s.forall(ch => ch >= ' ' && ch < 127 && ch != '"' && ch != '\\')

  /** Writes the name of enumeration value `value` (as a key with `asKey`), as jsoniter-scala: a stable value (see
    * [[JsoniterScalaEnumeration.stableIndex]]) with its name as a constant (a balanced binary dispatch over its
    * position), any other value with the name looked up at runtime.
    */
  @scala.annotation.nowarn("msg=is never used")
  private def writeEnumerationName[A: Type](
      support: Expr[JsoniterScalaEnumeration[A]],
      value: Expr[A],
      writer: Expr[JsonWriter],
      asKey: Boolean
  ): Expr[Unit] = {
    implicit val UnitT: Type[Unit] = CTypes.Unit
    implicit val IntT: Type[Int] = CTypes.Int
    implicit val StringT: Type[String] = CTypes.String
    implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
    val lookedUp: Expr[Unit] =
      if (asKey) Expr.quote(Expr.splice(support).writeKey(Expr.splice(value), Expr.splice(writer)))
      else Expr.quote(Expr.splice(support).writeVal(Expr.splice(value), Expr.splice(writer)))
    val names = enumerationStableValuesOf[A].map(_._1).toVector
    if (names.isEmpty || names.size > maxStableValuesWrittenAsConstants) lookedUp
    else {
      def writeConstant(name: String): Expr[Unit] = {
        val nameExpr = Expr(name)
        (asKey, isNonEscapedAscii(name)) match {
          case (true, true)   => Expr.quote(Expr.splice(writer).writeNonEscapedAsciiKey(Expr.splice(nameExpr)))
          case (true, false)  => Expr.quote(Expr.splice(writer).writeKey(Expr.splice(nameExpr)))
          case (false, true)  => Expr.quote(Expr.splice(writer).writeNonEscapedAsciiVal(Expr.splice(nameExpr)))
          case (false, false) => Expr.quote(Expr.splice(writer).writeVal(Expr.splice(nameExpr)))
        }
      }
      // positions `from` to `to` (from 1), `index` known to be one of them
      def dispatch(index: Expr[Int], from: Int, to: Int): Expr[Unit] =
        if (from == to) writeConstant(names(from - 1))
        else {
          val mid = (from + to) >>> 1
          val midExpr = Expr(mid)
          val lower = dispatch(index, from, mid)
          val upper = dispatch(index, mid + 1, to)
          Expr.quote(if (Expr.splice(index) <= Expr.splice(midExpr)) Expr.splice(lower) else Expr.splice(upper))
        }
      Expr.quote {
        val index = Expr.splice(support).stableIndex(Expr.splice(value))
        if (index == 0) Expr.splice(lookedUp)
        else Expr.splice(dispatch(Expr.quote(index), 1, names.size))
      }
    }
  }

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
              )(_ => newScalaEnumeration[A](enumObject)) >>
                cache.get0Ary[JsoniterScalaEnumeration[A]](key)
          }
      }

  /** Reads an enumeration value by its name (a key with `asKey`; a JSON `null` value is an error), as jsoniter-scala:
    * with at most 8 stable values (and 64 characters of their names), the name read is compared with their names first
    * (constants, no hashing); any other name is looked up at runtime.
    */
  @scala.annotation.nowarn("msg=is never used")
  private def readEnumerationName[A: Type](
      support: Expr[JsoniterScalaEnumeration[A]],
      reader: Expr[JsonReader],
      asKey: Boolean
  ): Expr[A] = {
    implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
    implicit val IntT: Type[Int] = CTypes.Int
    implicit val BooleanT: Type[Boolean] = CTypes.Boolean
    implicit val StringT: Type[String] = CTypes.String
    val stable = enumerationStableValuesOf[A]
    if (stable.isEmpty || stable.size > 8 || stable.map(_._1.length).sum > 64) {
      if (asKey) Expr.quote(Expr.splice(support).readKey(Expr.splice(reader)))
      else Expr.quote(Expr.splice(support).readVal(Expr.splice(reader), null.asInstanceOf[A]))
    } else {
      def compare(length: Expr[Int]): Expr[A] =
        stable.zipWithIndex.foldRight(
          Expr.quote(Expr.splice(support).readName(Expr.splice(reader), Expr.splice(length)))
        ) { case (((name, value), i), orElse) =>
          val nameExpr = Expr(name)
          val iExpr = Expr(i)
          Expr.quote {
            if (
              Expr.splice(reader).isCharBufEqualsTo(Expr.splice(length), Expr.splice(nameExpr)) &&
              Expr.splice(support).isStableName(Expr.splice(iExpr))
            ) Expr.splice(value)
            else Expr.splice(orElse)
          }
        }
      if (asKey) Expr.quote {
        val length = Expr.splice(reader).readKeyAsCharBuf()
        Expr.splice(compare(Expr.quote(length)))
      }
      else
        Expr.quote {
          val in = Expr.splice(reader)
          if (in.isNextToken('"')) {
            in.rollbackToken()
            val length = in.readStringAsCharBuf()
            Expr.splice(compare(Expr.quote(length)))
          } else in.readNullOrTokenError(null.asInstanceOf[A], '"')
        }
    }
  }

  /** `new JsoniterScalaEnumeration` with the stable values of `A` (an array filled when it is first needed). */
  @scala.annotation.nowarn("msg=is never used")
  private def newScalaEnumeration[A: Type](enumObject: Expr[Enumeration]): Expr[JsoniterScalaEnumeration[A]] = {
    implicit val StringArrayT: Type[Array[String]] = Type.of[Array[String]]
    implicit val AnyRefArrayT: Type[Array[AnyRef]] = Type.of[Array[AnyRef]]
    implicit val UnitT: Type[Unit] = CTypes.Unit
    implicit val IntT: Type[Int] = CTypes.Int
    implicit val StringT: Type[String] = CTypes.String
    val stable = enumerationStableValuesOf[A]
    if (stable.isEmpty || stable.size > maxStableValuesWrittenAsConstants)
      Expr.quote(new JsoniterScalaEnumeration[A](Expr.splice(enumObject)))
    else {
      val namesExpr: Expr[Array[String]] = Expr(stable.map(_._1).toArray)
      val sizeExpr = Expr(stable.size)
      Expr.quote {
        new JsoniterScalaEnumeration[A](
          Expr.splice(enumObject), {
            val values = new Array[AnyRef](Expr.splice(sizeExpr))
            Expr.splice {
              stable.zipWithIndex
                .map { case ((_, v), i) =>
                  val iExpr = Expr(i)
                  Expr.quote(values(Expr.splice(iExpr)) = Expr.splice(v).asInstanceOf[AnyRef])
                }
                .reduce((a, b) => Expr.quote { Expr.splice(a); Expr.splice(b) })
            }
            values
          },
          Expr.splice(namesExpr)
        )
      }
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
            def byName: Expr[Unit] = writeEnumerationName[A](support, ectx.value, ectx.writer, asKey = false)
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

  /** The decoding of an enumeration value field read in place by the case class decoder (as the encoder writes it in
    * place, see `EncoderHandleAsCaseClassRule`): only with the config known at compile time.
    */
  @scala.annotation.nowarn("msg=is never used")
  private[compiletime] def scalaEnumerationFieldDecoder[A: Type](
      ctx: DecoderCtx[?]
  ): MIO[Option[Expr[JsonReader] => Expr[A]]] =
    ctx.evaluatedConfig match {
      case None         => MIO.pure(None)
      case Some(config) =>
        scalaEnumerationOf[A](ctx.cache).map(_.map { support => (reader: Expr[JsonReader]) =>
          implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
          implicit val BooleanT: Type[Boolean] = CTypes.Boolean
          if (config.useScalaEnumValueId) {
            val stringified = Expr(ctx.stringifiedMode.getOrElse(false))
            Expr.quote(
              Expr.splice(support).readIdVal(Expr.splice(reader), null.asInstanceOf[A], Expr.splice(stringified))
            )
          } else readEnumerationName[A](support, reader, asKey = false)
        })
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
            def byName: Expr[A] = readEnumerationName[A](support, dctx.reader, asKey = false)
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
            def byName: Expr[Unit] = writeEnumerationName[K](support, keyExpr, writerExpr, asKey = true)
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
            def byName: Expr[K] = readEnumerationName[K](support, readerExpr, asKey = true)
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
