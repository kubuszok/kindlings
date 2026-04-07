package hearth.kindlings.pureconfigderivation.internal.compiletime

import hearth.MacroCommons
import hearth.std.*

import hearth.kindlings.pureconfigderivation.{KindlingsConfigConvert, PureConfig}
import hearth.kindlings.pureconfigderivation.internal.runtime.PureConfigDerivationUtils
import pureconfig.{ConfigReader, ConfigWriter}

trait ConvertMacrosImpl {
  this: MacroCommons & StdExtensions & AnnotationSupport & LoadStandardExtensionsOnce & ReaderMacrosImpl &
    WriterMacrosImpl =>

  @scala.annotation.nowarn("msg=is never used")
  def deriveConvertTypeClass[A: Type](configExpr: Expr[PureConfig]): Expr[KindlingsConfigConvert[A]] = {
    implicit val KindlingsConfigConvertA: Type[KindlingsConfigConvert[A]] = CTypes.KindlingsConfigConvert[A]

    // Step 1: derive ConfigReader[A]
    val readerExpr: Expr[ConfigReader[A]] =
      deriveReaderTypeClass[A](configExpr).asInstanceOf[Expr[ConfigReader[A]]]
    // Step 2: derive ConfigWriter[A]
    val writerExpr: Expr[ConfigWriter[A]] =
      deriveWriterTypeClass[A](configExpr).asInstanceOf[Expr[ConfigWriter[A]]]
    // Step 3: combine at runtime
    Expr.quote {
      PureConfigDerivationUtils.configConvert[A](
        Expr.splice(readerExpr),
        Expr.splice(writerExpr)
      )
    }
  }

  private object CTypes {
    def KindlingsConfigConvert: Type.Ctor1[KindlingsConfigConvert] = Type.Ctor1.of[KindlingsConfigConvert]
  }
}
