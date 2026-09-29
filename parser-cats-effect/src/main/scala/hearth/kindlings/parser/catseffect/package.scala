package hearth.kindlings.parser

import cats.effect.kernel.{Async, Sync}

/** `import hearth.kindlings.parser.catseffect._` provides a [[ParserEngine]] for every `Async[F]` (preferred) or
  * `Sync[F]`, so that `Grammar.grammar[R, IO] { ... }` compiles.
  */
package object catseffect extends LowPriorityCatsEffectEngines {

  implicit def asyncParserEngine[F[_]](implicit F: Async[F]): ParserEngine[F] = CatsEffectEngine.async[F]()
}

trait LowPriorityCatsEffectEngines {

  implicit def syncParserEngine[F[_]](implicit F: Sync[F]): ParserEngine[F] = catseffect.CatsEffectEngine.sync[F]()
}
