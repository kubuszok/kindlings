package hearth.kindlings.parser

import cats.effect.kernel.{Async, Sync}

/** `import hearth.kindlings.parser.catseffect._` provides a [[ParserEngine]] for every `Async[F]` (preferred) or
  * `Sync[F]`, so that `Grammar.grammar[R, IO] { ... }` compiles, and an [[ErrorChannel]] for them.
  */
package object catseffect extends LowPriorityCatsEffectEngines {

  implicit def asyncParserEngine[F[_]](implicit F: Async[F]): ParserEngine[F] = CatsEffectEngine.async[F]()

  /** The cats-effect engines raise parse errors (and rejected values) in `F`. */
  @scala.annotation.nowarn("msg=unused")
  implicit def syncErrorChannel[F[_]](implicit F: Sync[F]): ErrorChannel[F] = ErrorChannel.assumed[F]
}

trait LowPriorityCatsEffectEngines {

  implicit def syncParserEngine[F[_]](implicit F: Sync[F]): ParserEngine[F] = catseffect.CatsEffectEngine.sync[F]()
}
