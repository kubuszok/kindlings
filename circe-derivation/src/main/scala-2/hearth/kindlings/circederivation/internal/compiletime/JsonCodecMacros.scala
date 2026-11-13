package hearth.kindlings.circederivation
package internal.compiletime

import scala.reflect.macros.whitebox

/** The `@JsonCodec` / `@ConfiguredJsonCodec` macro annotations (Scala 2 only), ported from circe-generic's
  * `JsonCodecMacros`. A macro annotation only rewrites the annottee's definition, so it is written with plain Scala 2
  * reflection (Hearth has no macro-annotation API): the generated members call the regular Kindlings derivation.
  */
final private[circederivation] class JsonCodecMacros(val c: whitebox.Context) {
  import c.universe.*

  def jsonCodec(annottees: Tree*): Tree = construct(annottees, configured = false)
  def configuredJsonCodec(annottees: Tree*): Tree = construct(annottees, configured = true)

  private def isCaseClassOrSealed(clsDef: ClassDef): Boolean =
    clsDef.mods.hasFlag(Flag.CASE) || clsDef.mods.hasFlag(Flag.SEALED)

  private def construct(annottees: Seq[Tree], configured: Boolean): Tree = annottees match {
    case List(clsDef: ClassDef) if isCaseClassOrSealed(clsDef) =>
      q"""
       $clsDef
       object ${clsDef.name.toTermName} {
         ..${codec(clsDef, configured)}
       }
       """
    case List(
          clsDef: ClassDef,
          q"..$mods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }"
        ) if isCaseClassOrSealed(clsDef) =>
      q"""
       $clsDef
       $mods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf =>
         ..$objDefs
         ..${codec(clsDef, configured)}
       }
       """
    case _ =>
      c.abort(c.enclosingPosition, "Invalid annotation target: must be a case class or a sealed trait/class")
  }

  sealed private trait CodecType
  private case object Both extends CodecType
  private case object EncodeOnly extends CodecType
  private case object DecodeOnly extends CodecType

  private def codecType: CodecType = {
    def flag(value: Tree): Boolean = value match {
      case Literal(Constant(b: Boolean)) => b
      case _ => c.abort(c.enclosingPosition, "The codec annotation's arguments must be boolean literals")
    }
    val args: List[Tree] = c.prefix.tree match {
      case Apply(Select(New(_), _), as) => as
      case _                            => c.abort(c.enclosingPosition, "Unexpected macro application")
    }
    // (encodeOnly, decodeOnly), positional or named
    val (encodeOnly, decodeOnly) = args.zipWithIndex.foldLeft((false, false)) {
      case ((_, d), (NamedArg(Ident(TermName("encodeOnly")), v), _)) => (flag(v), d)
      case ((e, _), (NamedArg(Ident(TermName("decodeOnly")), v), _)) => (e, flag(v))
      case ((_, d), (v, 0))                                          => (flag(v), d)
      case ((e, _), (v, 1))                                          => (e, flag(v))
      case _ => c.abort(c.enclosingPosition, "Unsupported arguments supplied to the codec annotation")
    }
    (encodeOnly, decodeOnly) match {
      case (false, false) => Both
      case (true, false)  => EncodeOnly
      case (false, true)  => DecodeOnly
      case _              => c.abort(c.enclosingPosition, "encodeOnly and decodeOnly cannot both be true")
    }
  }

  private def codec(clsDef: ClassDef, configured: Boolean): List[Tree] = {
    val tpname = clsDef.name
    val tparams = clsDef.tparams
    val decodeName = TermName("decode" + tpname.decodedName)
    val encodeName = TermName("encode" + tpname.decodedName)
    val codecName = TermName("codecFor" + tpname.decodedName)
    val pkg = q"_root_.hearth.kindlings.circederivation"
    // @JsonCodec ignores a configuration in scope (circe-generic is not configurable); @ConfiguredJsonCodec uses it.
    val config: Tree =
      if (configured) q"_root_.scala.Predef.implicitly[$pkg.Configuration]" else q"$pkg.Configuration.default"
    val Decoder = tq"_root_.io.circe.Decoder"
    val Encoder = tq"_root_.io.circe.Encoder"
    val EncoderAsObject = tq"_root_.io.circe.Encoder.AsObject"
    val CodecAsObject = tq"$pkg.KindlingsCodecAsObject"
    if (tparams.isEmpty) {
      val tpe = tq"$tpname"
      codecType match {
        case Both =>
          List(
            q"implicit lazy val $codecName: $CodecAsObject[$tpe] = $pkg.KindlingsCodecAsObject.derived[$tpe]($config)"
          )
        case DecodeOnly =>
          List(q"implicit lazy val $decodeName: $Decoder[$tpe] = $pkg.KindlingsDecoder.derived[$tpe]($config)")
        case EncodeOnly =>
          List(
            q"implicit lazy val $encodeName: $EncoderAsObject[$tpe] = $pkg.KindlingsEncoder.deriveAsObject[$tpe]($config)"
          )
      }
    } else {
      val tparamNames = tparams.map(_.name)
      def implicitParams(prefix: String, tc: Tree): List[ValDef] = tparamNames.zipWithIndex.map { case (name, i) =>
        ValDef(Modifiers(Flag.PARAM | Flag.IMPLICIT), TermName(s"$prefix$i"), tq"$tc[$name]", EmptyTree)
      }
      val tpe = tq"$tpname[..$tparamNames]"
      val decoder =
        q"""implicit def $decodeName[..$tparams](implicit ..${implicitParams("decode", Decoder)}): $Decoder[$tpe] =
              $pkg.KindlingsDecoder.derived[$tpe]($config)"""
      val encoder =
        q"""implicit def $encodeName[..$tparams](implicit ..${implicitParams(
            "encode",
            Encoder
          )}): $EncoderAsObject[$tpe] =
              $pkg.KindlingsEncoder.deriveAsObject[$tpe]($config)"""
      codecType match {
        case Both       => List(decoder, encoder)
        case DecodeOnly => List(decoder)
        case EncodeOnly => List(encoder)
      }
    }
  }
}
