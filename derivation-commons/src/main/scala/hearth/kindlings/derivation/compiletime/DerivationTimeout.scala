package hearth.kindlings.derivation.compiletime

trait DerivationTimeout {
  this: hearth.MacroCommons =>

  protected def derivationSettingsNamespace: String

  protected lazy val derivationTimeout: scala.concurrent.duration.FiniteDuration = {
    import scala.concurrent.duration.FiniteDuration
    import java.util.concurrent.TimeUnit

    (for {
      data <- Environment.typedSettings.toOption
      moduleSettings <- data.get(derivationSettingsNamespace)
      timeoutData <- moduleSettings.get("timeout")
      duration <- timeoutData.asInt
        .filter(_ > 0)
        .map(n => FiniteDuration(n.toLong, TimeUnit.SECONDS))
        .orElse(timeoutData.asLong.filter(_ > 0).map(n => FiniteDuration(n, TimeUnit.SECONDS)))
        .orElse(timeoutData.asString.flatMap(parseDurationString))
    } yield duration).getOrElse(DerivationTimeout.Default)
  }

  private def parseDurationString(str: String): Option[scala.concurrent.duration.FiniteDuration] = {
    import scala.concurrent.duration.FiniteDuration
    import java.util.concurrent.TimeUnit
    str.trim match {
      case DerivationTimeout.DurationPattern(num, unit) =>
        val n = num.toLong
        if (n > 0) {
          val tu = unit match {
            case "ms" | "millis" | "milliseconds" => TimeUnit.MILLISECONDS
            case "s" | "second" | "seconds"       => TimeUnit.SECONDS
            case "m" | "minute" | "minutes"       => TimeUnit.MINUTES
          }
          Some(FiniteDuration(n, tu))
        } else {
          Environment.reportWarn(
            s"$derivationSettingsNamespace.timeout: value must be positive, got '$str'. " +
              s"Using default of ${DerivationTimeout.Default.toSeconds}s."
          )
          None
        }
      case _ =>
        Environment.reportWarn(
          s"$derivationSettingsNamespace.timeout: unrecognized format '$str'. " +
            s"Expected formats: 120, 120s, 5000ms, 5m. " +
            s"Using default of ${DerivationTimeout.Default.toSeconds}s."
        )
        None
    }
  }
}

object DerivationTimeout {

  val Default: scala.concurrent.duration.FiniteDuration =
    scala.concurrent.duration.FiniteDuration(120, java.util.concurrent.TimeUnit.SECONDS)

  private val DurationPattern = """^\s*(\d+)\s*(ms|millis|milliseconds|s|seconds?|m|minutes?)\s*$""".r
}
