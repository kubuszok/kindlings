package hearth.kindlings.tapirschemaderivation.internal.compiletime

/** The error shown when several JSON library configurations are in implicit scope and none is preferred. Every line it
  * suggests compiles as written (fully qualified names).
  */
private[compiletime] object PreferSchemaConfigHint {

  /** One configuration found in scope: the library name accepted by the `preferConfig` setting (e.g. `"circe"`) and the
    * fully qualified name of its configuration type.
    */
  final case class Found(libraryName: String, configClassName: String)

  def multipleConfigurations(macroName: String, settingsNamespace: String, found: List[Found]): String = {
    val prefer = "hearth.kindlings.tapirschemaderivation.PreferSchemaConfig"
    s"$macroName: Multiple JSON library configurations found: ${found.map(_.libraryName).mkString(", ")}.\n" +
      "Disambiguate by adding one of the scalac settings:\n" +
      found
        .map(f => s"""  scalacOptions += "-Xmacro-settings:$settingsNamespace.preferConfig=${f.libraryName}"""")
        .mkString("\n") +
      "\nOr one of the implicits in an enclosing object (not a class body):\n" +
      found
        .map(f => s"  implicit val prefer: $prefer[${f.configClassName}] = $prefer[${f.configClassName}]")
        .mkString("\n")
  }
}
