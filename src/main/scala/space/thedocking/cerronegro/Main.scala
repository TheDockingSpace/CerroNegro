package space.thedocking.cerronegro

object Main extends App {
  import cats.Show
  import cats.implicits._
  import argonaut._
  import Argonaut._

  val fragment1 = JsonFragment(
    "Bla",
    """
      |{
      |   "userid": "<someid>",
      |   "relatives": [{"name": "<each relativeName>", "alive": true}]
      |}
      |""".stripMargin
  )

  val fragment2 = JsonFragment(
    "someid",
    "\"123\""
  )

  val ctx = GenerationContext(fragment1, fragment2).toVO

  implicit def ParserJsonFragmentCodecJson: CodecJson[ParsedJsonFragment] =
    casecodec3(ParsedJsonFragment.apply, ParsedJsonFragment.unapply)(
      "fragmentName",
      "str",
      "json")

  implicit def FailedJsonFragmentCodecJson: CodecJson[FailedJsonFragment] =
    casecodec3(FailedJsonFragment.apply, FailedJsonFragment.unapply)(
      "fragmentName",
      "str",
      "failure")

  implicit def MissingDependencyCodecJson: CodecJson[MissingDependency] =
    casecodec1(MissingDependency.apply, MissingDependency.unapply)(
      "missingDependencyName")

  implicit def GenerationContextVOCodecJson: CodecJson[GenerationContextVO] =
    casecodec4(GenerationContextVO.apply, GenerationContextVO.unapply)(
      "parsedFragments",
      "failedFragments",
      "missingDependencies",
      "fragmentDependencies"
//      "rootFragments"
    )

  implicit val showCtx: Show[GenerationContextVO] =
    Show.show(ctx => ctx.asJson.spaces4)

  println(ctx.show)
}
