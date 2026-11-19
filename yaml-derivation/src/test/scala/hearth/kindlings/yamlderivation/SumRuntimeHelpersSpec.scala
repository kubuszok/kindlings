package hearth.kindlings.yamlderivation

import hearth.MacroSuite
import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils
import org.virtuslab.yaml.Node

/** The runtime helpers of sum types derived with a configuration only known at runtime. */
final class SumRuntimeHelpersSpec extends MacroSuite {

  test("transformNames transforms every name, in order") {
    YamlDerivationUtils.transformNames(_.toUpperCase, List("a", "bc", "d")).toList ==> List("A", "BC", "D")
    YamlDerivationUtils.transformNames(identity, Nil).toList ==> Nil
  }

  test("tagSubtype tags a node as the configuration says") {
    val node: Node = YamlDerivationUtils.nodeFromFields(List("x" -> YamlDerivationUtils.encodeEnumAsString("1")))
    def tag(config: YamlConfig, alwaysWrapped: Boolean = false, nameWhenUntagged: Boolean = false): Node =
      YamlDerivationUtils.tagSubtype(config, "Name", node, alwaysWrapped, nameWhenUntagged)

    tag(YamlConfig(discriminator = Some("type"))) ==> YamlDerivationUtils.addDiscriminator("type", "Name", node)
    tag(YamlConfig(discriminator = Some("type")), alwaysWrapped = true) ==>
      YamlDerivationUtils.addDiscriminator("type", "Name", node)
    tag(YamlConfig(wrappedSubtypes = true)) ==> YamlDerivationUtils.wrapWithTypeName("Name", node)
    tag(YamlConfig(), alwaysWrapped = true) ==> YamlDerivationUtils.wrapWithTypeName("Name", node)
    tag(YamlConfig()) ==> node
    tag(YamlConfig(), nameWhenUntagged = true) ==> YamlDerivationUtils.encodeEnumAsString("Name")
  }
}
