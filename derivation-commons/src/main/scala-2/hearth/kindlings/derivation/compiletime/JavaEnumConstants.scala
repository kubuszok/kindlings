package hearth.kindlings.derivation.compiletime

/** The constants of a Java-defined enum, in declaration order (the order of the static fields of its companion).
  *
  * On Scala 3 this also completes the constants, which the compiler needs before it reports them as the enum's children
  * (see the Scala 3 version); Scala 2 registers all of them when it reads the class file.
  */
private[compiletime] object JavaEnumConstants {

  def apply(commons: hearth.MacroCommons)(tpe: commons.UntypedType): Option[List[String]] = commons match {
    case scala2: hearth.MacroCommonsScala2 =>
      val c = scala2.c
      import c.universe.*
      val repr = tpe.asInstanceOf[Type]
      val symbol = repr.typeSymbol
      if (symbol != NoSymbol && symbol.isClass && symbol.isJava && repr <:< typeOf[java.lang.Enum[?]]) {
        val module = symbol.companion
        if (module == NoSymbol) None
        else {
          val constants = module.info.decls.toList.collect {
            case field if field.isTerm && field.isStatic && field.typeSignature <:< repr =>
              field.name.decodedName.toString.trim
          }
          if (constants.isEmpty) None else Some(constants)
        }
      } else None
    case _ => None
  }
}
