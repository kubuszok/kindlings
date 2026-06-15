package hearth.kindlings.openapijsoniter
package internal

import hearth.kindlings.jsoniterjson.Json
import sttp.apispec._
import sttp.apispec.openapi._

import scala.collection.immutable.ListMap

/** Port of `sttp.apispec.openapi.internal.InternalSttpOpenAPICirceDecoders` onto the kindlings [[Json]] AST. */
private[openapijsoniter] trait OpenApiDecoders extends JsonSchemaDecoders {

  implicit val referenceDecoder: Decoder[Reference] = Decoder.fromCursor { c =>
    for {
      ref <- c.get[String]("$ref")
      summary <- c.get[Option[String]]("summary")
      description <- c.get[Option[String]]("description")
    } yield Reference(ref, summary, description)
  }

  implicit def decodeReferenceOr[A](implicit d: Decoder[A]): Decoder[ReferenceOr[A]] =
    referenceDecoder.map(Left(_): ReferenceOr[A]).or(d.map(Right(_): ReferenceOr[A]))

  private def listADecoder[A](implicit d: Decoder[A]): Decoder[List[A]] =
    Decoder.decodeOption(Decoder.decodeList[A]).map(_.getOrElse(Nil))

  implicit val tagDecoder: Decoder[Tag] = withExtensions(Decoder.fromCursor { c =>
    for {
      name <- c.get[String]("name")
      description <- c.get[Option[String]]("description")
      externalDocs <- c.get[Option[ExternalDocumentation]]("externalDocs")
      extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
    } yield Tag(name, description, externalDocs, extensions)
  })

  implicit val oauthFlowDecoder: Decoder[OAuthFlow] = withExtensions(Decoder.fromCursor { c =>
    for {
      authorizationUrl <- c.get[Option[String]]("authorizationUrl")
      tokenUrl <- c.get[Option[String]]("tokenUrl")
      refreshUrl <- c.get[Option[String]]("refreshUrl")
      scopes <- c.getOrElse[ListMap[String, String]]("scopes")(ListMap.empty)
      extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
    } yield OAuthFlow(authorizationUrl, tokenUrl, refreshUrl, scopes, extensions)
  })

  implicit val oauthFlowsDecoder: Decoder[OAuthFlows] = withExtensions(Decoder.fromCursor { c =>
    for {
      `implicit` <- c.get[Option[OAuthFlow]]("implicit")
      password <- c.get[Option[OAuthFlow]]("password")
      clientCredentials <- c.get[Option[OAuthFlow]]("clientCredentials")
      authorizationCode <- c.get[Option[OAuthFlow]]("authorizationCode")
      extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
    } yield OAuthFlows(`implicit`, password, clientCredentials, authorizationCode, extensions)
  })

  implicit val securitySchemeDecoder: Decoder[SecurityScheme] = withExtensions(Decoder.fromCursor { c =>
    for {
      tpe <- c.get[String]("type")
      description <- c.get[Option[String]]("description")
      name <- c.get[Option[String]]("name")
      in <- c.get[Option[String]]("in")
      scheme <- c.get[Option[String]]("scheme")
      bearerFormat <- c.get[Option[String]]("bearerFormat")
      flows <- c.get[Option[OAuthFlows]]("flows")
      openIdConnectUrl <- c.get[Option[String]]("openIdConnectUrl")
      extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
    } yield SecurityScheme(tpe, description, name, in, scheme, bearerFormat, flows, openIdConnectUrl, extensions)
  })

  implicit val contactDecoder: Decoder[Contact] = withExtensions(Decoder.fromCursor { c =>
    for {
      name <- c.get[Option[String]]("name")
      email <- c.get[Option[String]]("email")
      url <- c.get[Option[String]]("url")
      extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
    } yield Contact(name, email, url, extensions)
  })

  implicit val licenseDecoder: Decoder[License] = withExtensions(Decoder.fromCursor { c =>
    for {
      name <- c.get[String]("name")
      url <- c.get[Option[String]]("url")
      extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
    } yield License(name, url, extensions)
  })

  implicit val infoDecoder: Decoder[Info] = withExtensions(Decoder.fromCursor { c =>
    for {
      title <- c.get[String]("title")
      version <- c.get[String]("version")
      summary <- c.get[Option[String]]("summary")
      description <- c.get[Option[String]]("description")
      termsOfService <- c.get[Option[String]]("termsOfService")
      contact <- c.get[Option[Contact]]("contact")
      license <- c.get[Option[License]]("license")
      extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
    } yield Info(title, version, summary, description, termsOfService, contact, license, extensions)
  })

  implicit val serverVariableDecoder: Decoder[ServerVariable] = Decoder.fromCursor { c =>
    for {
      enum_ <- c.get[Option[List[String]]]("enum")
      default <- c.get[String]("default")
      description <- c.get[Option[String]]("description")
      extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
    } yield ServerVariable(enum_, default, description, extensions)
  }

  implicit val serverDecoder: Decoder[Server] = withExtensions(Decoder.fromCursor { c =>
    for {
      url <- c.get[String]("url")
      description <- c.get[Option[String]]("description")
      variables <- c.get[Option[ListMap[String, ServerVariable]]]("variables")
      extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
    } yield Server(url, description, variables, extensions)
  })

  implicit val linkDecoder: Decoder[Link] = Decoder.fromCursor { c =>
    for {
      operationRef <- c.get[Option[String]]("operationRef")
      operationId <- c.get[Option[String]]("operationId")
      parameters <- c.getOrElse[ListMap[String, String]]("parameters")(ListMap.empty)
      requestBody <- c.get[Option[String]]("requestBody")
      description <- c.get[Option[String]]("description")
      server <- c.get[Option[Server]]("server")
      extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
    } yield Link(operationRef, operationId, parameters, requestBody, description, server, extensions)
  }

  implicit val parameterInDecoder: Decoder[ParameterIn] = Decoder.decodeString.emap {
    case ParameterIn.Path.value   => Right(ParameterIn.Path)
    case ParameterIn.Query.value  => Right(ParameterIn.Query)
    case ParameterIn.Cookie.value => Right(ParameterIn.Cookie)
    case ParameterIn.Header.value => Right(ParameterIn.Header)
    case err                      => Left(s"$err is not a valid ParameterIn value")
  }
  implicit val parameterStyleDecoder: Decoder[ParameterStyle] = Decoder.decodeString.emap {
    case ParameterStyle.Form.value           => Right(ParameterStyle.Form)
    case ParameterStyle.Label.value          => Right(ParameterStyle.Label)
    case ParameterStyle.Matrix.value         => Right(ParameterStyle.Matrix)
    case ParameterStyle.Simple.value         => Right(ParameterStyle.Simple)
    case ParameterStyle.SpaceDelimited.value => Right(ParameterStyle.SpaceDelimited)
    case ParameterStyle.DeepObject.value     => Right(ParameterStyle.DeepObject)
    case ParameterStyle.PipeDelimited.value  => Right(ParameterStyle.PipeDelimited)
    case err                                 => Left(s"$err is not a valid ParameterStyle value")
  }

  implicit val exampleDecoder: Decoder[Example] = withExtensions(Decoder.fromCursor { c =>
    for {
      summary <- c.get[Option[String]]("summary")
      description <- c.get[Option[String]]("description")
      value <- c.get[Option[ExampleValue]]("value")
      externalValue <- c.get[Option[String]]("externalValue")
      extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
    } yield Example(summary, description, value, externalValue, extensions)
  })

  implicit val encodingDecoder: Decoder[Encoding] = withExtensions(Decoder.fromCursor { c =>
    for {
      contentType <- c.get[Option[String]]("contentType")
      headers <- c.getOrElse[ListMap[String, ReferenceOr[Header]]]("headers")(ListMap.empty)
      style <- c.get[Option[ParameterStyle]]("style")
      explode <- c.get[Option[Boolean]]("explode")
      allowReserved <- c.get[Option[Boolean]]("allowReserved")
      extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
    } yield Encoding(contentType, headers, style, explode, allowReserved, extensions)
  })

  implicit val headerDecoder: Decoder[Header] = Decoder.fromCursor { c =>
    for {
      description <- c.get[Option[String]]("description")
      required <- c.get[Option[Boolean]]("required")
      deprecated <- c.get[Option[Boolean]]("deprecated")
      allowEmptyValue <- c.get[Option[Boolean]]("allowEmptyValue")
      style <- c.get[Option[ParameterStyle]]("style")
      explode <- c.get[Option[Boolean]]("explode")
      allowReserved <- c.get[Option[Boolean]]("allowReserved")
      schema <- c.get[Option[SchemaLike]]("schema")
      example <- c.get[Option[ExampleValue]]("example")
      examples <- c.getOrElse[ListMap[String, ReferenceOr[Example]]]("examples")(ListMap.empty)
      content <- c.getOrElse[ListMap[String, MediaType]]("content")(ListMap.empty)
    } yield Header(description, required, deprecated, allowEmptyValue, style, explode, allowReserved, schema,
      example, examples, content)
  }

  implicit val mediaTypeDecoder: Decoder[MediaType] = withExtensions(Decoder.fromCursor { c =>
    for {
      schema <- c.get[Option[SchemaLike]]("schema")
      example <- c.get[Option[ExampleValue]]("example")
      examples <- c.getOrElse[ListMap[String, ReferenceOr[Example]]]("examples")(ListMap.empty)
      encoding <- c.getOrElse[ListMap[String, Encoding]]("encoding")(ListMap.empty)
      extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
    } yield MediaType(schema, example, examples, encoding, extensions)
  })

  implicit val requestBodyDecoder: Decoder[RequestBody] = withExtensions(Decoder.fromCursor { c =>
    for {
      description <- c.get[Option[String]]("description")
      content <- c.getOrElse[ListMap[String, MediaType]]("content")(ListMap.empty)
      required <- c.get[Option[Boolean]]("required")
      extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
    } yield RequestBody(description, content, required, extensions)
  })

  implicit val responseDecoder: Decoder[Response] = withExtensions(Decoder.fromCursor { c =>
    for {
      description <- c.getOrElse[String]("description")("")
      headers <- c.getOrElse[ListMap[String, ReferenceOr[Header]]]("headers")(ListMap.empty)
      content <- c.getOrElse[ListMap[String, MediaType]]("content")(ListMap.empty)
      links <- c.getOrElse[ListMap[String, ReferenceOr[Link]]]("links")(ListMap.empty)
      extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
    } yield Response(description, headers, content, links, extensions)
  })

  implicit val responsesKeyDecoder: KeyDecoder[ResponsesKey] = {
    val ResponseRange = "(1|2|3|4|5)XX".r
    val ResponseCode = "([1|2|3|4|5]\\d\\d)".r
    KeyDecoder.instance {
      case "default"            => Some(ResponsesDefaultKey)
      case ResponseRange(range) => Some(ResponsesRangeKey(range.toInt))
      case ResponseCode(code)   => Some(ResponsesCodeKey(code.toInt))
      case _                    => None
    }
  }

  implicit val responsesDecoder: Decoder[Responses] = withExtensions(Decoder.fromCursor { c =>
    val responsesJson = c.focus match {
      case Json.Obj(obj) => Json.Obj(obj.remove("extensions"))
      case other         => other
    }
    for {
      responses <- Decoder.decodeListMap[ResponsesKey, ReferenceOr[Response]].apply(responsesJson)
      extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
    } yield Responses(responses, extensions)
  })

  implicit val parameterDecoder: Decoder[Parameter] = withExtensions(Decoder.fromCursor { c =>
    for {
      name <- c.get[String]("name")
      in <- c.get[ParameterIn]("in")
      description <- c.get[Option[String]]("description")
      required <- c.get[Option[Boolean]]("required")
      deprecated <- c.get[Option[Boolean]]("deprecated")
      allowEmptyValue <- c.get[Option[Boolean]]("allowEmptyValue")
      style <- c.get[Option[ParameterStyle]]("style")
      explode <- c.get[Option[Boolean]]("explode")
      allowReserved <- c.get[Option[Boolean]]("allowReserved")
      schema <- c.get[Option[SchemaLike]]("schema")
      example <- c.get[Option[ExampleValue]]("example")
      examples <- c.getOrElse[ListMap[String, ReferenceOr[Example]]]("examples")(ListMap.empty)
      content <- c.getOrElse[ListMap[String, MediaType]]("content")(ListMap.empty)
      extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
    } yield Parameter(name, in, description, required, deprecated, allowEmptyValue, style, explode, allowReserved,
      schema, example, examples, content, extensions)
  })

  implicit val callbackDecoder: Decoder[Callback] = Decoder.fromCursor { c =>
    Decoder.decodeListMap[String, ReferenceOr[PathItem]].apply(c.focus).map(Callback(_))
  }

  implicit val operationDecoder: Decoder[Operation] = {
    implicit def ll[A: Decoder]: Decoder[List[A]] = listADecoder
    withExtensions(Decoder.fromCursor { c =>
      for {
        tags <- c.getOrElse[List[String]]("tags")(List.empty)
        summary <- c.get[Option[String]]("summary")
        description <- c.get[Option[String]]("description")
        externalDocs <- c.get[Option[ExternalDocumentation]]("externalDocs")
        operationId <- c.get[Option[String]]("operationId")
        parameters <- c.getOrElse[List[ReferenceOr[Parameter]]]("parameters")(List.empty)
        requestBody <- c.get[Option[ReferenceOr[RequestBody]]]("requestBody")
        responses <- c.getOrElse[Responses]("responses")(Responses.Empty)
        callbacks <- c.getOrElse[ListMap[String, ReferenceOr[Callback]]]("callbacks")(ListMap.empty)
        deprecated <- c.get[Option[Boolean]]("deprecated")
        security <- c.getOrElse[List[SecurityRequirement]]("security")(List.empty)
        servers <- c.getOrElse[List[Server]]("servers")(List.empty)
        extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
      } yield Operation(tags, summary, description, externalDocs, operationId, parameters, requestBody, responses,
        callbacks, deprecated, security, servers, extensions)
    })
  }

  implicit val pathItemDecoder: Decoder[PathItem] = {
    implicit def ll[A: Decoder]: Decoder[List[A]] = listADecoder
    withExtensions(Decoder.fromCursor { c =>
      for {
        ref <- c.get[Option[Reference]]("$ref")
        summary <- c.get[Option[String]]("summary")
        description <- c.get[Option[String]]("description")
        get <- c.get[Option[Operation]]("get")
        put <- c.get[Option[Operation]]("put")
        post <- c.get[Option[Operation]]("post")
        delete <- c.get[Option[Operation]]("delete")
        options <- c.get[Option[Operation]]("options")
        head <- c.get[Option[Operation]]("head")
        patch <- c.get[Option[Operation]]("patch")
        trace <- c.get[Option[Operation]]("trace")
        servers <- c.getOrElse[List[Server]]("servers")(List.empty)
        parameters <- c.getOrElse[List[ReferenceOr[Parameter]]]("parameters")(List.empty)
        extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
      } yield PathItem(ref, summary, description, get, put, post, delete, options, head, patch, trace, servers,
        parameters, extensions)
    })
  }

  implicit val pathsDecoder: Decoder[Paths] = withExtensions(Decoder.fromCursor { c =>
    val pathItemsJson = c.focus match {
      case Json.Obj(obj) => Json.Obj(obj.remove("extensions"))
      case other         => other
    }
    for {
      pathItems <- Decoder.decodeListMap[String, PathItem].apply(pathItemsJson)
      extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
    } yield Paths(pathItems, extensions)
  })

  implicit val componentsDecoder: Decoder[Components] = withExtensions(Decoder.fromCursor { c =>
    def getComp[A: Decoder](name: String): Either[DecodingFailure, ListMap[String, ReferenceOr[A]]] =
      c.get[Option[ListMap[String, ReferenceOr[A]]]](name).map(_.getOrElse(ListMap.empty))
    for {
      schemas <- c.get[Option[ListMap[String, SchemaLike]]]("schemas").map(_.getOrElse(ListMap.empty))
      responses <- getComp[Response]("responses")
      parameters <- getComp[Parameter]("parameters")
      examples <- getComp[Example]("examples")
      requestBodies <- getComp[RequestBody]("requestBodies")
      headers <- getComp[Header]("headers")
      securitySchemes <- getComp[SecurityScheme]("securitySchemes")
      links <- getComp[Link]("links")
      callbacks <- getComp[Callback]("callbacks")
      extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
    } yield Components(schemas, responses, parameters, examples, requestBodies, headers, securitySchemes, links,
      callbacks, extensions)
  })

  implicit val openAPIDecoder: Decoder[OpenAPI] = withExtensions(Decoder.fromCursor { c =>
    for {
      openapi <- c.get[String]("openapi")
      info <- c.get[Info]("info")
      jsonSchemaDialect <- c.get[Option[String]]("jsonSchemaDialect")
      tags <- c.getOrElse[List[Tag]]("tags")(Nil)
      servers <- c.getOrElse[List[Server]]("servers")(Nil)
      paths <- c.getOrElse[Paths]("paths")(Paths.Empty)
      webhooks <- c.get[Option[Map[String, ReferenceOr[PathItem]]]]("webhooks")
      components <- c.get[Option[Components]]("components")
      security <- c.getOrElse[List[SecurityRequirement]]("security")(Nil)
      extensions <- c.getOrElse[ListMap[String, ExtensionValue]]("extensions")(ListMap.empty)
    } yield OpenAPI(openapi, info, jsonSchemaDialect, tags, servers, paths, webhooks, components, security, extensions)
  })

  // SecurityRequirement = ListMap[String, Vector[String]]
  implicit val securityRequirementDecoder: Decoder[SecurityRequirement] =
    Decoder.decodeListMap[String, Vector[String]]

  // Map[String, ReferenceOr[PathItem]] for webhooks
  implicit val webhooksMapDecoder: Decoder[Map[String, ReferenceOr[PathItem]]] =
    Decoder.decodeListMap[String, ReferenceOr[PathItem]].map(_.toMap)
}
