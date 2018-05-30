package space.thedocking.cerronegro

import argonaut.Argonaut._
import argonaut.DecodeJsonCats.NonEmptyListDecodeJson
import argonaut.EncodeJsonCats.NonEmptyListEncodeJson
import argonaut._
import cats.Show

package object implicits {

  implicit def ParsedJsonFragmentCodecJson: CodecJson[ParsedJsonFragment] =
    casecodec3(ParsedJsonFragment.apply, ParsedJsonFragment.unapply)(
      "fragmentName",
      "str",
      "json")

  implicit def EachJsonFunctionCodecJson: CodecJson[EachJsonFunction] =
    casecodec1(EachJsonFunction.apply, EachJsonFunction.unapply)(
      "jsonFunctionArguments")

  implicit def FailedJsonFragmentCodecJson: CodecJson[FailedJsonFragment] =
    casecodec3(FailedJsonFragment.apply, FailedJsonFragment.unapply)(
      "fragmentName",
      "str",
      "failure")

  implicit def MissingDependencyCodecJson: CodecJson[MissingDependency] =
    casecodec3(MissingDependency.apply, MissingDependency.unapply)(
      "location",
      "dependencyName",
      "dependencyExpression")

  implicit def FailedFragmentDependencyCodecJson
    : CodecJson[FailedFragmentDependency] =
    casecodec3(FailedFragmentDependency.apply,
               FailedFragmentDependency.unapply)("location",
                                                 "dependencyExpression",
                                                 "jsonFragment")

  implicit def FunctionDependencyCodecJson: CodecJson[FunctionDependency] =
    casecodec3(FunctionDependency.apply, FunctionDependency.unapply)(
      "location",
      "dependencyExpression",
      "jsonFunction")

  implicit def ParsedFragmentDependencyCodecJson
    : CodecJson[ParsedFragmentDependency] =
    casecodec3(ParsedFragmentDependency.apply,
               ParsedFragmentDependency.unapply)("location",
                                                 "dependencyExpression",
                                                 "jsonFragment")

  implicit def GenerationContextVOCodecJson: CodecJson[GenerationContextVO] =
    casecodec4(GenerationContextVO.apply, GenerationContextVO.unapply)(
      "parsedFragments",
      "failedFragments",
      "missingDependencies",
      "fragmentDependencies"
      //      "rootFragments"
    )

  private def tagged[A](tag: String,
                        c: HCursor,
                        decoder: DecodeJson[A]): DecodeResult[A] =
    (c --\ tag).hcursor.fold(
      DecodeResult.fail[A]("Invalid tagged type", c.history))(decoder.decode)

  implicit def ParsedJsonFunctionEncodeJson: EncodeJson[ParsedJsonFunction] =
    EncodeJson(_ match {
      case EachJsonFunction(args) => Json("each" := Json("arguments" := args))
      case EchoJsonFunction(args) => Json("echo" := Json("arguments" := args))
    })

  implicit def ParsedJsonFunctionDecodeJson: DecodeJson[ParsedJsonFunction] =
    DecodeJson(
      c =>
        tagged("each",
               c,
               implicitly[DecodeJson[List[String]]].map(EachJsonFunction)) |||
          tagged("echo",
                 c,
                 implicitly[DecodeJson[List[String]]].map(EchoJsonFunction)))

  implicit val JsonDependencyCodecJson: CodecJson[JsonDependency] =
    CodecJson[JsonDependency](
      {
        case d: FailedFragmentDependency => FailedFragmentDependencyCodecJson(d)
        case d: FunctionDependency       => FunctionDependencyCodecJson(d)
        case d: MissingDependency        => MissingDependencyCodecJson(d)
        case d: ParsedFragmentDependency => ParsedFragmentDependencyCodecJson(d)
      },
      (FailedFragmentDependencyCodecJson
        ||| [FunctionDependency, JsonDependency] FunctionDependencyCodecJson
        ||| [MissingDependency, JsonDependency] MissingDependencyCodecJson
        ||| [ParsedFragmentDependency, JsonDependency] ParsedFragmentDependencyCodecJson)
        .map(d => d: JsonDependency)(_)
    )

  implicit val showCtx: Show[GenerationContextVO] =
    Show.show(ctx => ctx.asJson.spaces2)

}
