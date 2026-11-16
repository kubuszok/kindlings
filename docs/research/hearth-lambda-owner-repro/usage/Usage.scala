package repro

object ReproMacros {
  inline def builtOutsideQuotedLambda: Int => Int = ${ Repro.builtOutsideQuotedLambdaImpl }
  inline def builtInsideQuotedLambda: Int => Int = ${ Repro.builtInsideQuotedLambdaImpl }
  inline def valDefsNextToQuotedValsInsideQuotedLambda: Int => List[Int] = ${
    Repro.valDefsNextToQuotedValsInsideQuotedLambdaImpl
  }
  inline def quotedLambdaInsideLambdaBuilder: Int => Int = ${ Repro.quotedLambdaInsideLambdaBuilderImpl }
  inline def lambdaBuilderInsideQuotedLambda: Int => Int = ${ Repro.lambdaBuilderInsideQuotedLambdaImpl }
  inline def matchCaseChainInsideQuotedLambda: Int => Either[String, Int] = ${
    Repro.matchCaseChainInsideQuotedLambdaImpl
  }
}

@main def runRepro(): Unit = {
  println(ReproMacros.builtOutsideQuotedLambda(10))
  println(ReproMacros.builtInsideQuotedLambda(10))
  println(ReproMacros.valDefsNextToQuotedValsInsideQuotedLambda(10))
  println(ReproMacros.quotedLambdaInsideLambdaBuilder(10))
  println(ReproMacros.lambdaBuilderInsideQuotedLambda(10))
  println(ReproMacros.matchCaseChainInsideQuotedLambda(10))
}
