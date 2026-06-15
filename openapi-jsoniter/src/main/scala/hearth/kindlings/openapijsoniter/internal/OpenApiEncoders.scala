package hearth.kindlings.openapijsoniter
package internal

import hearth.kindlings.jsoniterjson.{Json, JsonObject}
import sttp.apispec.*
import sttp.apispec.openapi.*

import scala.collection.immutable.ListMap

/** Port of `sttp.apispec.openapi.internal.InternalSttpOpenAPICirceEncoders` onto the kindlings [[Json]] AST. */
private[openapijsoniter] trait OpenApiEncoders extends JsonSchemaEncoders {

  // Small helper to drop nulls / drop-nulls-and-expand from a built ordered object.
  private def obj(entries: Vector[(String, Json)]): Json = Json.Obj(JsonObject(entries))
  private def dropNulls(entries: Vector[(String, Json)]): Json = Json.Obj(JsonAstOps.dropNulls(JsonObject(entries)))
  private def dropNullsExpand(entries: Vector[(String, Json)]): Json =
    Json.Obj(JsonAstOps.dropNullsExpandExtensions(JsonObject(entries)))

  implicit val encoderReference: Encoder[Reference] = Encoder.instance { r =>
    dropNulls(
      Vector(
        "$ref" := r.$ref,
        "summary" := r.summary,
        "description" := r.description
      )
    )
  }

  implicit def encoderReferenceOr[T](implicit e: Encoder[T]): Encoder[ReferenceOr[T]] = Encoder.instance {
    case Left(Reference(ref, summary, description)) =>
      Json.Obj(
        JsonAstOps.dropNulls(
          JsonObject(
            Vector(
              "$ref" := ref,
              "summary" := summary,
              "description" := description
            )
          )
        )
      )
    case Right(t) => e(t)
  }

  implicit val encoderOAuthFlow: Encoder[OAuthFlow] = Encoder.instance { f =>
    // #79: scopes MUST always be present (emit `{}` when empty).
    val scopesEnc = doEncodeListMap[String, String](nullWhenEmpty = false)
    dropNullsExpand(
      Vector(
        "authorizationUrl" := f.authorizationUrl,
        "tokenUrl" := f.tokenUrl,
        "refreshUrl" := f.refreshUrl,
        "scopes" -> scopesEnc(f.scopes),
        "extensions" := f.extensions
      )
    )
  }

  implicit val encoderOAuthFlows: Encoder[OAuthFlows] = Encoder.instance { f =>
    dropNullsExpand(
      Vector(
        "implicit" := f.`implicit`,
        "password" := f.password,
        "clientCredentials" := f.clientCredentials,
        "authorizationCode" := f.authorizationCode,
        "extensions" := f.extensions
      )
    )
  }

  implicit val encoderSecurityScheme: Encoder[SecurityScheme] = Encoder.instance { s =>
    dropNullsExpand(
      Vector(
        "type" := s.`type`,
        "description" := s.description,
        "name" := s.name,
        "in" := s.in,
        "scheme" := s.scheme,
        "bearerFormat" := s.bearerFormat,
        "flows" := s.flows,
        "openIdConnectUrl" := s.openIdConnectUrl,
        "extensions" := s.extensions
      )
    )
  }

  implicit val encoderParameterStyle: Encoder[ParameterStyle] = Encoder.instance(s => Json.fromString(s.value))
  implicit val encoderParameterIn: Encoder[ParameterIn] = Encoder.instance(s => Json.fromString(s.value))

  implicit val encoderHeader: Encoder[Header] = Encoder.instance { h =>
    dropNulls(
      Vector(
        "description" := h.description,
        "required" := h.required,
        "deprecated" := h.deprecated,
        "allowEmptyValue" := h.allowEmptyValue,
        "style" := h.style,
        "explode" := h.explode,
        "allowReserved" := h.allowReserved,
        "schema" := h.schema,
        "example" := h.example,
        "examples" := h.examples,
        "content" := h.content
      )
    )
  }

  implicit val encoderExample: Encoder[Example] = Encoder.instance { e =>
    dropNullsExpand(
      Vector(
        "summary" := e.summary,
        "description" := e.description,
        "value" := e.value,
        "externalValue" := e.externalValue,
        "extensions" := e.extensions
      )
    )
  }

  implicit val encoderResponse: Encoder[Response] = Encoder.instance { r =>
    dropNullsExpand(
      Vector(
        "description" := r.description,
        "headers" := r.headers,
        "content" := r.content,
        "links" := r.links,
        "extensions" := r.extensions
      )
    )
  }

  implicit val encoderLink: Encoder[Link] = Encoder.instance { l =>
    dropNullsExpand(
      Vector(
        "operationRef" := l.operationRef,
        "operationId" := l.operationId,
        "parameters" := l.parameters,
        "requestBody" := l.requestBody,
        "description" := l.description,
        "server" := l.server,
        "extensions" := l.extensions
      )
    )
  }

  implicit val encoderEncoding: Encoder[Encoding] = Encoder.instance { e =>
    dropNullsExpand(
      Vector(
        "contentType" := e.contentType,
        "headers" := e.headers,
        "style" := e.style,
        "explode" := e.explode,
        "allowReserved" := e.allowReserved,
        "extensions" := e.extensions
      )
    )
  }

  implicit val encoderMediaType: Encoder[MediaType] = Encoder.instance { m =>
    dropNullsExpand(
      Vector(
        "schema" := m.schema,
        "example" := m.example,
        "examples" := m.examples,
        "encoding" := m.encoding,
        "extensions" := m.extensions
      )
    )
  }

  implicit val encoderRequestBody: Encoder[RequestBody] = Encoder.instance { r =>
    dropNullsExpand(
      Vector(
        "description" := r.description,
        "content" := r.content,
        "required" := r.required,
        "extensions" := r.extensions
      )
    )
  }

  implicit val encoderParameter: Encoder[Parameter] = Encoder.instance { p =>
    dropNullsExpand(
      Vector(
        "name" := p.name,
        "in" := p.in,
        "description" := p.description,
        "required" := p.required,
        "deprecated" := p.deprecated,
        "allowEmptyValue" := p.allowEmptyValue,
        "style" := p.style,
        "explode" := p.explode,
        "allowReserved" := p.allowReserved,
        "schema" := p.schema,
        "example" := p.example,
        "examples" := p.examples,
        "content" := p.content,
        "extensions" := p.extensions
      )
    )
  }

  implicit val encoderCallback: Encoder[Callback] = Encoder.instance { callback =>
    obj(callback.pathItems.iterator.map { case (path, pathItem) =>
      path -> encoderReferenceOr(encoderPathItem)(pathItem)
    }.toVector)
  }

  implicit val encoderResponseMap: Encoder[ListMap[ResponsesKey, ReferenceOr[Response]]] = Encoder.instance {
    responses =>
      val fields = responses.iterator.map {
        case (ResponsesDefaultKey, r)      => ("default", encoderReferenceOr(encoderResponse)(r))
        case (ResponsesCodeKey(code), r)   => (code.toString, encoderReferenceOr(encoderResponse)(r))
        case (ResponsesRangeKey(range), r) => (s"${range}XX", encoderReferenceOr(encoderResponse)(r))
      }.toVector
      obj(fields)
  }

  implicit val encoderResponses: Encoder[Responses] = Encoder.instance { resp =>
    val extensions = extensionsAsObject(resp.extensions)
    val respJson = encoderResponseMap(resp.responses)
    respJson.asObject
      .map(_ => JsonAstOps.deepMerge(respJson, Json.Obj(extensions)))
      .getOrElse(respJson)
  }

  implicit val encoderOperation: Encoder[Operation] = Encoder.instance { op =>
    // security: empty SecurityRequirement -> `{}` (nullWhenEmpty = false); callbacks keep nullWhenEmpty = true.
    val secReqEnc = doEncodeListMap[String, Vector[String]](nullWhenEmpty = false)
    val securityEnc: Encoder[List[SecurityRequirement]] = Encoder.encodeList(secReqEnc)
    dropNullsExpand(
      Vector(
        "tags" := op.tags,
        "summary" := op.summary,
        "description" := op.description,
        "externalDocs" := op.externalDocs,
        "operationId" := op.operationId,
        "parameters" := op.parameters,
        "requestBody" := op.requestBody,
        "responses" := op.responses,
        "callbacks" := op.callbacks,
        "deprecated" := op.deprecated,
        "security" -> securityEnc(op.security),
        "servers" := op.servers,
        "extensions" := op.extensions
      )
    )
  }

  implicit val encoderPathItem: Encoder[PathItem] = Encoder.instance { p =>
    dropNullsExpand(
      Vector(
        "$ref" := p.ref,
        "summary" := p.summary,
        "description" := p.description,
        "get" := p.get,
        "put" := p.put,
        "post" := p.post,
        "delete" := p.delete,
        "options" := p.options,
        "head" := p.head,
        "patch" := p.patch,
        "trace" := p.trace,
        "servers" := p.servers,
        "parameters" := p.parameters,
        "extensions" := p.extensions
      )
    )
  }

  implicit val encoderPaths: Encoder[Paths] = Encoder.instance { paths =>
    val extensions = extensionsAsObject(paths.extensions)
    val pathItems = encodeListMap[String, PathItem].apply(paths.pathItems)
    pathItems.asObject
      .map(_ => JsonAstOps.deepMerge(pathItems, Json.Obj(extensions)))
      .getOrElse(pathItems)
  }

  implicit val encoderComponents: Encoder[Components] = Encoder.instance { c =>
    dropNullsExpand(
      Vector(
        "schemas" := c.schemas,
        "responses" := c.responses,
        "parameters" := c.parameters,
        "examples" := c.examples,
        "requestBodies" := c.requestBodies,
        "headers" := c.headers,
        "securitySchemes" := c.securitySchemes,
        "links" := c.links,
        "callbacks" := c.callbacks,
        "extensions" := c.extensions
      )
    )
  }

  implicit val encoderServerVariable: Encoder[ServerVariable] = Encoder.instance { v =>
    dropNullsExpand(
      Vector(
        "enum" := v.`enum`,
        "default" := v.default,
        "description" := v.description,
        "extensions" := v.extensions
      )
    )
  }

  implicit val encoderServer: Encoder[Server] = Encoder.instance { s =>
    dropNullsExpand(
      Vector(
        "url" := s.url,
        "description" := s.description,
        "variables" := s.variables,
        "extensions" := s.extensions
      )
    )
  }

  implicit val encoderTag: Encoder[Tag] = Encoder.instance { t =>
    dropNullsExpand(
      Vector(
        "name" := t.name,
        "description" := t.description,
        "externalDocs" := t.externalDocs,
        "extensions" := t.extensions
      )
    )
  }

  implicit val encoderInfo: Encoder[Info] = Encoder.instance { i =>
    dropNullsExpand(
      Vector(
        "title" := i.title,
        "version" := i.version,
        "summary" := i.summary,
        "description" := i.description,
        "termsOfService" := i.termsOfService,
        "contact" := i.contact,
        "license" := i.license,
        "extensions" := i.extensions
      )
    )
  }

  implicit val encoderContact: Encoder[Contact] = Encoder.instance { c =>
    dropNullsExpand(
      Vector(
        "name" := c.name,
        "email" := c.email,
        "url" := c.url,
        "extensions" := c.extensions
      )
    )
  }

  implicit val encoderLicense: Encoder[License] = Encoder.instance { l =>
    dropNullsExpand(
      Vector(
        "name" := l.name,
        "url" := l.url,
        "extensions" := l.extensions
      )
    )
  }

  implicit val encoderOpenAPI: Encoder[OpenAPI] = Encoder.instance { o =>
    val secReqEnc = doEncodeListMap[String, Vector[String]](nullWhenEmpty = true)
    val securityEnc: Encoder[List[SecurityRequirement]] = Encoder.encodeList(secReqEnc)
    val webhooksEnc: Encoder[Option[Map[String, ReferenceOr[PathItem]]]] = Encoder.instance {
      case None    => Json.Null
      case Some(m) =>
        Json.Obj(JsonObject(m.iterator.map { case (k, v) => k -> encoderReferenceOr(encoderPathItem)(v) }.toVector))
    }
    dropNullsExpand(
      Vector(
        "openapi" := o.openapi,
        "info" := o.info,
        "jsonSchemaDialect" := o.jsonSchemaDialect,
        "tags" := o.tags,
        "servers" := o.servers,
        "paths" := o.paths,
        "webhooks" -> webhooksEnc(o.webhooks),
        "components" := o.components,
        "security" -> securityEnc(o.security),
        "extensions" := o.extensions
      )
    )
  }

  // ExtensionValue ListMap -> raw JsonObject (used by Responses/Paths deepMerge).
  private def extensionsAsObject(exts: ListMap[String, ExtensionValue]): JsonObject =
    JsonObject(exts.iterator.map { case (k, v) => k -> extensionValue(v) }.toVector)
}
