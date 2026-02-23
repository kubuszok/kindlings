package hearth.kindlings.jsoniterderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReaderException, JsonValueCodec}
import scala.reflect.macros.blackbox

final private[jsoniterderivation] class CodecMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with CodecMacrosImpl {

  def deriveCodecImpl[A: c.WeakTypeTag](
      config: c.Expr[JsoniterConfig]
  ): c.Expr[JsonValueCodec[A]] = deriveCodecTypeClass[A](config).asInstanceOf[c.Expr[JsonValueCodec[A]]]

  def deriveKindlingsCodecImpl[A: c.WeakTypeTag](
      config: c.Expr[JsoniterConfig]
  ): c.Expr[KindlingsJsonValueCodec[A]] = deriveCodecTypeClass[A](config)

  def deriveInlineWriteToStringImpl[A: c.WeakTypeTag](
      value: c.Expr[A]
  )(config: c.Expr[JsoniterConfig]): c.Expr[String] =
    deriveInlineWriteToString[A](value, config)

  def deriveInlineReadFromStringImpl[A: c.WeakTypeTag](
      json: c.Expr[String]
  )(config: c.Expr[JsoniterConfig]): c.Expr[Either[JsonReaderException, A]] =
    deriveInlineReadFromString[A](json, config)

  @scala.annotation.nowarn("msg=unchecked")
  def deriveInlineWriteToStringOpsImpl[A: c.WeakTypeTag](
      config: c.Expr[JsoniterConfig]
  ): c.Expr[String] = {
    val value = c.Expr[A](c.prefix.tree match {
      case c.universe.Apply(_, List(arg)) => arg
      case other                          => c.abort(c.enclosingPosition, s"Unexpected prefix: $other")
    })
    deriveInlineWriteToString[A](value, config)
  }

  @scala.annotation.nowarn("msg=unchecked")
  def deriveInlineReadFromStringOpsImpl[A: c.WeakTypeTag](
      config: c.Expr[JsoniterConfig]
  ): c.Expr[Either[JsonReaderException, A]] = {
    val json = c.Expr[String](c.prefix.tree match {
      case c.universe.Apply(_, List(arg)) => arg
      case other                          => c.abort(c.enclosingPosition, s"Unexpected prefix: $other")
    })
    deriveInlineReadFromString[A](json, config)
  }
}
