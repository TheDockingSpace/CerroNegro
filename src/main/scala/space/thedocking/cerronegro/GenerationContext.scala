package space.thedocking.cerronegro

import java.util.UUID

import argonaut.Argonaut._
import argonaut.DecodeJsonCats.NonEmptyListDecodeJson
import argonaut.EncodeJsonCats.NonEmptyListEncodeJson
import argonaut._
import cats.Show
import cats.data.NonEmptyList
import cats.implicits._
import space.thedocking.cerronegro.ArgonautUtils.{jsonArrayToMap, jsonObjectToMap}

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.matching.Regex

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

  implicit def FailedFragmentDependencyCodecJson: CodecJson[FailedFragmentDependency] =
    casecodec3(FailedFragmentDependency.apply, FailedFragmentDependency.unapply)(
      "location",
      "dependencyExpression",
      "jsonFragment")

  implicit def FunctionDependencyCodecJson: CodecJson[FunctionDependency] =
    casecodec3(FunctionDependency.apply, FunctionDependency.unapply)(
      "location",
      "dependencyExpression",
      "jsonFunction")

  implicit def ParsedFragmentDependencyCodecJson: CodecJson[ParsedFragmentDependency] =
    casecodec3(ParsedFragmentDependency.apply, ParsedFragmentDependency.unapply)(
      "location",
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

  private def tagged[A](tag: String, c: HCursor, decoder: DecodeJson[A]): DecodeResult[A] =
  (c --\ tag).hcursor.fold(DecodeResult.fail[A]("Invalid tagged type", c.history))(decoder.decode)

  implicit def ParsedJsonFunctionEncodeJson: EncodeJson[ParsedJsonFunction] =
    EncodeJson(_ match {
      case EachJsonFunction(args) => Json("each" := Json("arguments" := args))
      case EchoJsonFunction(args) => Json("echo" := Json("arguments" := args))
    })

  implicit def ParsedJsonFunctionDecodeJson: DecodeJson[ParsedJsonFunction] =
    DecodeJson(c =>
        tagged("each", c, implicitly[DecodeJson[List[String]]].map(EachJsonFunction)) |||
          tagged("echo", c, implicitly[DecodeJson[List[String]]].map(EchoJsonFunction)))

  implicit val showCtx: Show[GenerationContextVO] =
    Show.show(ctx => ctx.asJson.spaces2)

}

sealed trait ProcessFailure {
  val failure: String
}

sealed trait GeneratorStackElement {
  val name: String
}

sealed trait ProcessSuccess extends GeneratorStackElement {
  val json: Json
}

sealed trait JsonFragment {
  val name: String
  def str: String
}

case class ParsedJsonFragment(override val name: String,
                              override val str: String,
                              override val json: Json)
    extends JsonFragment
    with ProcessSuccess

case class FailedJsonFragment(override val name: String,
                              override val str: String,
                              override val failure: String)
    extends JsonFragment
    with ProcessFailure

case object JsonFragment {
  def apply(name: String, fragment: String): JsonFragment = {
    fragment.parse match {
      case Left(failure) => FailedJsonFragment(name, fragment, failure)
      case Right(json)   => ParsedJsonFragment(name, fragment, json)
    }
  }
}

sealed trait GeneratedJson {
  val jsonDependency: JsonDependency
  def str: String
}

case class GenerationSuccess(override val jsonDependency: JsonDependency,
                             override val json: Json)
    extends GeneratedJson
    with ProcessSuccess
    with GeneratorStackElement {
  override lazy val name: String = jsonDependency.name
  override lazy val str: String = json.spaces4
}

case class GenerationFailure(override val jsonDependency: JsonDependency,
                             override val str: String,
                             override val failure: String)
    extends GeneratedJson
    with ProcessFailure

case object GenerationContext {

  val ExpressionRegex: Regex = """<[ ]*(\w+[ ]*)+>""".r

  val jsonFunctionParsers: Map[Symbol, JsonFunctionParser] = Map(
    EachJsonFunctionParser.functionName -> EachJsonFunctionParser,
    EchoJsonFunctionParser.functionName -> EchoJsonFunctionParser)

  def apply(fragments: Set[JsonFragment],
            jsonFunctionParsers: Map[Symbol, JsonFunctionParser] =
              jsonFunctionParsers): GenerationContext =
    LazyGenerationContext(UUID.randomUUID.toString,fragments.map(f => f.name -> f).toMap,
                          jsonFunctionParsers)

  def apply(fragments: JsonFragment*): GenerationContext =
    apply(fragments.toSet)

}

case class GenerationContextVO(
    parsedFragments: Map[String, ParsedJsonFragment],
    failedFragments: Map[String, FailedJsonFragment],
    missingDependencies: Map[String, Set[String]],
    fragmentDependencies: Map[String, Set[String]]
//    rootFragments: NonEmptyList[String]
)

sealed trait JsonDependency extends GeneratorStackElement {
  val dependencyExpression: String
  val location: NonEmptyList[String]
}

sealed trait FailedDependency extends JsonDependency

sealed trait ValidDependency extends JsonDependency

case class MissingDependency(override val location: NonEmptyList[String],
                             override val name: String,
                             override val dependencyExpression: String) extends FailedDependency

case class FailedFragmentDependency(override val location: NonEmptyList[String],
                                    override val dependencyExpression: String,
                                    jsonFragment: FailedJsonFragment)
  extends FailedDependency {
  override val name: String = jsonFragment.name
}

case class ParsedFragmentDependency(override val location: NonEmptyList[String],
                              override val dependencyExpression: String,
                              jsonFragment: ParsedJsonFragment)
    extends ValidDependency {
  override val name: String = jsonFragment.name
}

case class FunctionDependency(override val location: NonEmptyList[String],
                              override val dependencyExpression: String,
                              jsonFunction: ParsedJsonFunction)
    extends ValidDependency {
  override val name: String = jsonFunction.functionName.name
}

trait GenerationContext extends GeneratorStackElement {
  def rootFragments: Set[ParsedJsonFragment]
  def missingDependencies: Map[ParsedJsonFragment, Set[FailedDependency]]
  def fragmentDependencies: Map[ParsedJsonFragment, Set[ValidDependency]]
  def dependenciesByExpression: Map[String, ValidDependency]
  def missingDependenciesByName: Map[String, FailedDependency]
  def toVO: GenerationContextVO
  def generate(implicit generator: Generator): GenerationResult =
    generator.processGenerationContext(this)
}

case class LazyGenerationContext(
                                override val name: String,
    fragmentsByName: Map[String, JsonFragment],
    jsonFunctionParsers: Map[Symbol, JsonFunctionParser])
    extends GenerationContext {
  import GenerationContext._
  import io.scalaland.chimney.dsl._
  override def toVO: GenerationContextVO = {
    this
      .into[GenerationContextVO]
      .withFieldComputed(_.parsedFragments, _.parsedFragments)
      .withFieldComputed(_.failedFragments, _.failedFragments)
      .withFieldComputed(
        _.missingDependencies,
        _.missingDependencies.map {
          case (fragment, missing) =>
            fragment.name -> missing.map(_.name)
        }
      )
      .withFieldComputed(_.fragmentDependencies, _.fragmentDependencies.map {
        case (fragment, dependencies) =>
          fragment.name -> dependencies.map(_.name)
      })
//      .withFieldComputed(_.rootFragments, _.rootFragments)
      .transform
  }

  val (parsedFragments: Map[String, ParsedJsonFragment],
       failedFragments: Map[String, FailedJsonFragment]) = {
    val grouped = fragmentsByName.values.groupBy(_.getClass)

    def classFragmentsByName[T <: JsonFragment](
        implicit classTag: ClassTag[T]): Map[String, T] =
      grouped
        .getOrElse(classTag.runtimeClass.asInstanceOf[Class[T]], Nil)
        .map(f => f.name -> f.asInstanceOf[T])
        .toMap[String, T]
    (classFragmentsByName[ParsedJsonFragment],
     classFragmentsByName[FailedJsonFragment])
  }

  private lazy val missingAndFound = {
    val (missing, found) =
      parsedFragments.values.map(findElementDependencies).unzip
    (missing.fold(Map.empty)(_ |+| _), found.fold(Map.empty)(_ |+| _))

  }

    override lazy val missingDependencies: Map[ParsedJsonFragment, Set[FailedDependency]] = missingAndFound._1
    override lazy val fragmentDependencies: Map[ParsedJsonFragment, Set[ValidDependency]] = missingAndFound._2

  override lazy val dependenciesByExpression: Map[String, ValidDependency] = {
    fragmentDependencies.values.flatten.toSet.map { d: ValidDependency =>
      d.dependencyExpression -> d
    }.toMap
  }

  override lazy val missingDependenciesByName: Map[String, FailedDependency] = {
    missingDependencies.values.flatten.toSet.map { d: FailedDependency =>
      d.name -> d
    }.toMap
  }

  lazy val nonRootFragments: Set[ParsedJsonFragment] =
    fragmentDependencies.values.flatten.flatMap { d: ValidDependency =>
      val fragments: Set[ParsedJsonFragment] = d match {
        case ParsedFragmentDependency(_, _, f: ParsedJsonFragment) => Set(f)
        case FunctionDependency(_, _, jsonFunction) =>
          jsonFunction.jsonFunctionDependencyNames.flatMap(dependencyName =>
            parsedFragments.get(dependencyName))
        case _ => Set.empty
      }
      fragments
    }.toSet

  override lazy val rootFragments
    : Set[ParsedJsonFragment] = parsedFragments.values.toSet -- nonRootFragments

  def findElementDependencies(fragment: ParsedJsonFragment)
    : (Map[ParsedJsonFragment, Set[FailedDependency]],
       Map[ParsedJsonFragment, Set[ValidDependency]]) = {
    val mixedDependencies =
      findElementDependencies(fragment, fragment.json :: Nil)
    val missing = mixedDependencies
      .mapValues {
        _.filter(_.isLeft).map(_.left.get)
      }
      .filterNot(_._2.isEmpty)
    val found = mixedDependencies
      .mapValues {
        _.filter(_.isRight).map(_.right.get)
      }
      .filterNot(_._2.isEmpty)
    (missing, found)
  }

  def checkFragments(expressionString: String, location: NonEmptyList[String])
    : Option[Set[Either[FailedDependency, ParsedFragmentDependency]]] = {
    val dependencies: Option[Set[Either[FailedDependency, ParsedFragmentDependency]]] =
      fragmentsByName
        .get(expressionString)
        .map{
          case f: ParsedJsonFragment => Set(ParsedFragmentDependency(location, expressionString, f).asRight)
          case m: FailedJsonFragment => Set(FailedFragmentDependency(location, expressionString, m).asLeft)
        }
    dependencies
  }

  def checkFunctions(expressionString: String, location: NonEmptyList[String])
    : Option[Set[Either[FailedDependency, ValidDependency]]] = {
    val jsonFunction: Option[ParsedJsonFunction] =
      expressionString.split(' ').toList match {
        case function :: arguments =>
          jsonFunctionParsers
            .get(Symbol(function))
            .map(_.eval(arguments))
        case Nil => None //this should never happen
      }
    val functionDependency
      : Option[Set[Either[FailedDependency, ValidDependency]]] =
      jsonFunction.map(f => Set(Right(FunctionDependency(location, expressionString, f))))
    val functionDependencies
      : Option[Set[Either[FailedDependency, ValidDependency]]] =
      jsonFunction.flatMap { jsonFunction: ParsedJsonFunction =>
        val functionDependencies =
          jsonFunction.jsonFunctionDependencyNames.flatMap { expressionString =>
            checkMatchedExpressions(expressionString, location)
          }
        if (functionDependencies.isEmpty) None else functionDependencies.some
      }
    functionDependency |+| functionDependencies
  }

  def checkExpressions(stringValue: String, location: NonEmptyList[String])
    : Set[Either[FailedDependency, ValidDependency]] = {
    val matcher = ExpressionRegex.pattern.matcher(stringValue)
    if (matcher.matches()) {
      checkMatchedExpressions(matcher.toMatchResult
                                .group(1),
                              location)
    } else {
      Set.empty
    }
  }

  def checkMatchedExpressions(expressionString: String,
                              location: NonEmptyList[String])
    : Set[Either[FailedDependency, ValidDependency]] = {
    val dependencies =
      checkFragments(expressionString, location) match {
      case None =>
        checkFunctions(expressionString, location)
      case s: Some[_] => s.asInstanceOf[Option[Set[Either[FailedDependency, ValidDependency]]]]
    }
    dependencies.getOrElse(Set(Left(MissingDependency(location, expressionString, expressionString))))
  }

  def levelDependencies(location: NonEmptyList[String],
                        otherLocations: List[NonEmptyList[String]],
                        rest: List[Json],
                        values: Map[String, Json])
    : (List[Json],
       List[NonEmptyList[String]],
       Set[Either[FailedDependency, ValidDependency]]) = {
    val updatedRest = rest ++ values.values
    val indexes =
      values.keys.flatMap(key => NonEmptyList.fromList(location.toList :+ key))
    val updatedLocations = otherLocations ++ indexes
    val dependencies = Set.empty[Either[FailedDependency, ValidDependency]]
    (updatedRest, updatedLocations, dependencies)
  }

  @tailrec final def findElementDependencies(
      fragment: ParsedJsonFragment,
      elements: List[Json],
      locations: List[NonEmptyList[String]] = Nil,
      dependencies: Map[ParsedJsonFragment,
                        Set[Either[FailedDependency, ValidDependency]]] =
        Map.empty): Map[ParsedJsonFragment,
                        Set[Either[FailedDependency, ValidDependency]]] = {
    val (nextElements: List[Json],
         nextLocations: List[NonEmptyList[String]],
         fragmentDependencies: Map[
           ParsedJsonFragment,
           Set[Either[FailedDependency, ValidDependency]]]) = {
      val (nextElements, nextLocations, fragmentDependencies): (List[Json],
                                                                List[
                                                                  NonEmptyList[
                                                                    String]],
                                                                Set[Either[
                                                                  FailedDependency,
                                                                  ValidDependency]]) = {
        val location = locations.headOption.getOrElse(NonEmptyList.of("_root_"))
        val otherLocations: List[NonEmptyList[String]] =
          if (locations.isEmpty) Nil else locations.tail
        elements match {
          case json :: rest if json.isString =>
            (rest,
             otherLocations,
             dependencies
               .getOrElse(fragment, Set.empty) ++ checkExpressions(
               json.stringOrEmpty,
               location))
          case json :: rest if json.isArray =>
            levelDependencies(location,
                              otherLocations,
                              rest,
                              jsonArrayToMap(json))
          case json :: rest if json.isObject =>
            levelDependencies(location,
                              otherLocations,
                              rest,
                              jsonObjectToMap(json))
          case Nil =>
            (Nil, Nil, Set.empty)
          case _ :: rest =>
            (rest, otherLocations, Set.empty)
        }
      }
      (nextElements,
       nextLocations,
       if (fragmentDependencies.isEmpty) dependencies
       else Map(fragment -> fragmentDependencies) |+| dependencies)
    }
    if (nextElements.isEmpty) {
      fragmentDependencies
    } else {
      findElementDependencies(fragment,
                              nextElements,
                              nextLocations,
                              fragmentDependencies)
    }
  }

}
