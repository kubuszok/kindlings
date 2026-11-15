package hearth.kindlings.pureconfigderivation.internal.runtime

import pureconfig.ConfigCursor
import pureconfig.error.ConfigReaderFailures
import pureconfig.generic.error.NoValidCoproductOptionFound

/** The failure upstream pureconfig reports for a string that names no case object of an enumeration (a sealed family of
  * case objects read as strings). It differs between Scala versions: on Scala 2 (`pureconfig-generic`'s
  * `deriveEnumerationReader`) it is `NoValidCoproductOptionFound` with no option failures.
  */
object EnumerationFailures {

  @scala.annotation.nowarn("msg=never used")
  def invalidEnumValue(cursor: ConfigCursor, value: String, typeName: String): ConfigReaderFailures =
    cursor.asConfigValue.fold(
      identity,
      v => ConfigReaderFailures(cursor.failureFor(NoValidCoproductOptionFound(v, Seq.empty)))
    )
}
