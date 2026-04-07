package hearth.kindlings.sconfigderivation.internal.compiletime

import hearth.MacroCommons
import hearth.std.*

import hearth.kindlings.sconfigderivation.{ConfigCodec, ConfigReader, ConfigWriter, SConfig}
import hearth.kindlings.sconfigderivation.internal.runtime.SConfigDerivationUtils

trait CodecMacrosImpl {
  this: MacroCommons & StdExtensions & AnnotationSupport & LoadStandardExtensionsOnce & ReaderMacrosImpl &
    WriterMacrosImpl =>

  // Used only by Scala 2 — Scala 3 uses inline composition in ConfigCodecCompanionCompat
  // because the two derivations cannot share a single macro expansion (sibling-splice
  // isolation).
  @scala.annotation.nowarn("msg=is never used")
  def deriveCodecTypeClass[A: Type](configExpr: Expr[SConfig]): Expr[ConfigCodec[A]] = {
    implicit val ConfigCodecA: Type[ConfigCodec[A]] = CTypes.ConfigCodec[A]

    val readerExpr: Expr[ConfigReader[A]] = deriveReaderTypeClass[A](configExpr)
    val writerExpr: Expr[ConfigWriter[A]] = deriveWriterTypeClass[A](configExpr)

    Expr.quote {
      SConfigDerivationUtils.configCodec[A](
        Expr.splice(readerExpr),
        Expr.splice(writerExpr)
      )
    }
  }

  private object CTypes {
    def ConfigCodec: Type.Ctor1[ConfigCodec] = Type.Ctor1.of[ConfigCodec]
  }
}
