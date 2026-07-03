package hearth.kindlings.integrationtests

import cats.data.{
  Chain,
  Const,
  NonEmptyChain,
  NonEmptyLazyList,
  NonEmptyList,
  NonEmptyMap,
  NonEmptySeq,
  NonEmptySet,
  NonEmptyVector,
  Validated,
  ValidatedNel
}

case class WithNEL(values: NonEmptyList[Int])
case class WithNEV(values: NonEmptyVector[Int])
case class WithNESeq(values: NonEmptySeq[Int])
case class WithNELL(values: NonEmptyLazyList[Int])
case class WithNEC(values: NonEmptyChain[Int])
case class WithChain(values: Chain[Int])
case class WithNEM(values: NonEmptyMap[String, Int])
case class WithNES(values: NonEmptySet[Int])
case class WithConst(value: Const[String, Int])
case class WithValidated(result: Validated[String, Int])
case class WithValidatedNel(result: ValidatedNel[String, Int])
