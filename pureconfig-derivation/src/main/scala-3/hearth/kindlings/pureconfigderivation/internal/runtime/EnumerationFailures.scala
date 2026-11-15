package hearth.kindlings.pureconfigderivation.internal.runtime

import pureconfig.ConfigCursor
import pureconfig.error.{CannotConvert, ConfigReaderFailures}

/** The failure upstream pureconfig reports for a string that names no case object of an enumeration (a sealed family
  * of case objects read as strings). It differs between Scala versions: on Scala 3 (`EnumConfigReader`, used by
  * `deriveEnumerationReader`) it is `CannotConvert` to the enumeration's name.
  */
object EnumerationFailures {

  def invalidEnumValue(cursor: ConfigCursor, value: String, typeName: String): ConfigReaderFailures =
    ConfigReaderFailures(cursor.failureFor(CannotConvert(value, typeName, "The value is not a valid enum option.")))
}
