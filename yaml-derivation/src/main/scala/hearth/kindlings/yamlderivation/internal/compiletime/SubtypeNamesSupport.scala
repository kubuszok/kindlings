package hearth.kindlings.yamlderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*

import hearth.kindlings.yamlderivation.YamlConfig
import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils

trait SubtypeNamesSupport {
  this: MacroCommons =>

  /** Subtype names transformed by a configuration only known at runtime, shared by the encoder and the decoder: an
    * array computed once per instance (a lazy val of the instance, `cache`), read by index; computed where used when
    * the definitions are evaluated per call.
    */
  protected def transformedNamesOf(
      names: List[String],
      config: Expr[YamlConfig],
      perCall: Boolean,
      cache: MLocal[ValDefsCache]
  ): MIO[String => Expr[String]] = {
    implicit val StringT: Type[String] = SNTypes.String
    implicit val ArrayStringT: Type[Array[String]] = SNTypes.ArrayString
    def transformedAtCall(name: String): Expr[String] =
      Expr.quote(Expr.splice(config).transformConstructorNames(Expr.splice(Expr(name))))
    if (perCall || names.isEmpty) MIO.pure(transformedAtCall)
    else {
      val distinct = names.distinct
      val key = s"cached-constructor-names:${distinct.mkString("\u0000")}"
      val namesExpr: Expr[List[String]] = Expr(distinct)
      val arrayMIO: MIO[Expr[Array[String]]] = cache.get0Ary[Array[String]](key).flatMap {
        case Some(cached) => MIO.pure(cached)
        case None         =>
          cache.buildCachedWith(key, ValDefBuilder.ofLazy[Array[String]]("constructorNames")) { _ =>
            Expr.quote(
              YamlDerivationUtils.transformNames(Expr.splice(config).transformConstructorNames, Expr.splice(namesExpr))
            )
          } >> cache.get0Ary[Array[String]](key).map(_.get)
      }
      arrayMIO.map { array => (name: String) =>
        val index = distinct.indexOf(name)
        if (index < 0) transformedAtCall(name)
        else Expr.quote(Expr.splice(array)(Expr.splice(Expr(index))))
      }
    }
  }

  private object SNTypes {
    val String: Type.Lazy[String] = Type.Lazy(Type.of[String])
    val ArrayString: Type.Lazy[Array[String]] = Type.Lazy(Type.of[Array[String]])
  }
}
