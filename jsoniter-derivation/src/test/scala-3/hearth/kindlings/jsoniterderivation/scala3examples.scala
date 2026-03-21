package hearth.kindlings.jsoniterderivation

enum Fruit {
  case Apple(weight: Double)
  case Banana(length: Double)
}

object JsoniterOpaqueTypes {
  opaque type UserId = Int
  object UserId {
    def apply(value: Int): UserId = value
    extension (id: UserId) def value: Int = id
  }
}

case class JsoniterUserWithOpaque(id: JsoniterOpaqueTypes.UserId, name: String)

// Literal type test types
case class JsoniterWithLiteralString(tag: "hello", name: String)
case class JsoniterWithLiteralInt(code: 42, name: String)
case class JsoniterWithLiteralBoolean(flag: true, name: String)

// Union type test types
type StringOrInt = String | Int
case class Parrot(name: String, vocabulary: Int)
case class Hamster(name: String, wheelSize: Double)
type ParrotOrHamster = Parrot | Hamster

// Companion-object given derivation (reproduces sge-porting pattern)
// Tests that derive calls work from companion objects via type aliases
type TestJsonCodec[A] = com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[A]
inline def TestJsonCodec: KindlingsJsonValueCodec.type = KindlingsJsonValueCodec

// Full reproduction of G3dModelJson-like type graph with companion given
final case class CompKeyframeV1(
    keytime: Float = 0f,
    translation: Option[List[Float]] = None,
    rotation: Option[List[Float]] = None,
    scale: Option[List[Float]] = None
)
final case class CompKeyframeV2(keytime: Float = 0f, value: List[Float])
final case class CompAnimBone(
    boneId: String,
    keyframes: Option[List[CompKeyframeV1]] = None,
    translation: Option[List[CompKeyframeV2]] = None,
    rotation: Option[List[CompKeyframeV2]] = None,
    scaling: Option[List[CompKeyframeV2]] = None
)
final case class CompAnimation(id: String = "", bones: List[CompAnimBone] = Nil)
final case class CompMeshPart(id: String, tpe: String, indices: List[Short])
final case class CompMesh(id: String = "", attributes: List[String], vertices: List[Float], parts: List[CompMeshPart])
final case class CompTexture(
    id: String,
    filename: String,
    uvTranslation: Option[List[Float]] = None,
    uvScaling: Option[List[Float]] = None,
    tpe: String
)
final case class CompMaterial(
    id: String,
    diffuse: Option[List[Float]] = None,
    ambient: Option[List[Float]] = None,
    emissive: Option[List[Float]] = None,
    specular: Option[List[Float]] = None,
    reflection: Option[List[Float]] = None,
    shininess: Float = 0f,
    opacity: Float = 1f,
    textures: List[CompTexture] = Nil
)
final case class CompBone(
    node: String,
    translation: Option[List[Float]] = None,
    rotation: Option[List[Float]] = None,
    scale: Option[List[Float]] = None
)
final case class CompNodePart(meshpartid: String, materialid: String, bones: List[CompBone] = Nil)
final case class CompNode(
    id: String,
    translation: Option[List[Float]] = None,
    rotation: Option[List[Float]] = None,
    scale: Option[List[Float]] = None,
    mesh: Option[String] = None,
    parts: List[CompNodePart] = Nil,
    children: List[CompNode] = Nil
)
final case class CompModel(
    version: List[Short],
    id: String = "",
    meshes: List[CompMesh] = Nil,
    materials: List[CompMaterial] = Nil,
    nodes: List[CompNode] = Nil,
    animations: List[CompAnimation] = Nil
)
object CompModel {
  val codec: TestJsonCodec[CompModel] = KindlingsJsonValueCodec.derive[CompModel]
}
