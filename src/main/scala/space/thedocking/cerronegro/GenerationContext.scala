package space.thedocking.cerronegro

import argonaut._
import Argonaut._
import cats.data.NonEmptyList
import cats.instances.all._
import cats.syntax.all._
import space.thedocking.cerronegro.ArgonautUtils.{
  jsonArrayToMap,
  jsonObjectToMap
}

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.matching.Regex

sealed trait ProcessFailure {
  val failure: String
}

sealed trait GeneratorStackElement

sealed trait ProcessSuccess extends GeneratorStackElement {
  val json: Json
}

sealed trait JsonFragment {
  val fragmentName: String
  def str: String
}

case class ParsedJsonFragment(override val fragmentName: String,
                              override val str: String,
                              override val json: Json)
    extends JsonFragment
    with ProcessSuccess

case class FailedJsonFragment(override val fragmentName: String,
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
    EachJsonFunction.functionName -> EachJsonFunction)

  def apply(fragments: Set[JsonFragment],
            jsonFunctionParsers: Map[Symbol, JsonFunctionParser] =
              jsonFunctionParsers): GenerationContext =
    LazyGenerationContext(fragments.map(f => f.fragmentName -> f).toMap,
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
  val dependencyName: String
  val dependencyExpression: String
  val location: NonEmptyList[String]
}

case class MissingDependency(override val location: NonEmptyList[String],
                             override val dependencyName: String,
                             override val dependencyExpression: String) extends JsonDependency

case class FragmentDependency(override val location: NonEmptyList[String],
                              override val dependencyExpression: String,
                              jsonFragment: JsonFragment)
    extends JsonDependency {
  override val dependencyName: String = jsonFragment.fragmentName
}

case class FunctionDependency(override val location: NonEmptyList[String],
                              override val dependencyExpression: String,
                              jsonFunction: ParsedJsonFunction)
    extends JsonDependency {
  override val dependencyName: String = jsonFunction.functionName.name
}

trait GenerationContext extends GeneratorStackElement {
  def rootFragments: Set[ParsedJsonFragment]
  def missingDependencies: Map[ParsedJsonFragment, Set[MissingDependency]]
  def fragmentDependencies: Map[ParsedJsonFragment, Set[JsonDependency]]
  def dependenciesByExpression: Map[String, JsonDependency]
  def missingDependenciesByName: Map[String, MissingDependency]
  def toVO: GenerationContextVO
  def generate(implicit generator: Generator): GenerationResult =
    generator.processGenerationContext(this)
}

case class LazyGenerationContext(
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
            fragment.fragmentName -> missing.map(_.dependencyName)
        }
      )
      .withFieldComputed(_.fragmentDependencies, _.fragmentDependencies.map {
        case (fragment, dependencies) =>
          fragment.fragmentName -> dependencies.map(_.dependencyName)
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
        .map(f => f.fragmentName -> f.asInstanceOf[T])
        .toMap[String, T]
    (classFragmentsByName[ParsedJsonFragment],
     classFragmentsByName[FailedJsonFragment])
  }

  lazy val (
    missingDependencies: Map[ParsedJsonFragment, Set[MissingDependency]],
    fragmentDependencies: Map[ParsedJsonFragment, Set[JsonDependency]]) = {
    val (missing, found) =
      parsedFragments.values.map(findElementDependencies).unzip
    (missing.fold(Map.empty)(_ |+| _), found.fold(Map.empty)(_ |+| _))
  }

  override lazy val dependenciesByExpression: Map[String, JsonDependency] = {
    fragmentDependencies.values.flatten.toSet.map { d: JsonDependency =>
      d.dependencyExpression -> d
    }.toMap
  }

  override lazy val missingDependenciesByName: Map[String, MissingDependency] = {
    missingDependencies.values.flatten.toSet.map { d: MissingDependency =>
      d.dependencyName -> d
    }.toMap
  }

  lazy val nonRootFragments: Set[ParsedJsonFragment] =
    fragmentDependencies.values.flatten.flatMap { d: JsonDependency =>
      val fragments: Set[ParsedJsonFragment] = d match {
        case FragmentDependency(_, _, f: ParsedJsonFragment) => Set(f)
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
    : (Map[ParsedJsonFragment, Set[MissingDependency]],
       Map[ParsedJsonFragment, Set[JsonDependency]]) = {
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
    : Option[Set[Either[MissingDependency, JsonDependency]]] = {
    val dependencies: Option[Set[Either[MissingDependency, JsonDependency]]] =
      fragmentsByName
        .get(expressionString)
        .map(f => Set(Right(FragmentDependency(location, expressionString, f))))
    dependencies
  }

  def checkFunctions(expressionString: String, location: NonEmptyList[String])
    : Option[Set[Either[MissingDependency, JsonDependency]]] = {
    val jsonFunction: Option[ParsedJsonFunction] =
      expressionString.split(' ').toList match {
        case function :: arguments =>
          jsonFunctionParsers
            .get(Symbol(function))
            .map(_.eval(arguments))
        case Nil => None //this should never happen
      }
    val functionDependency
      : Option[Set[Either[MissingDependency, JsonDependency]]] =
      jsonFunction.map(f => Set(Right(FunctionDependency(location, expressionString, f))))
    val functionDependencies
      : Option[Set[Either[MissingDependency, JsonDependency]]] =
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
    : Set[Either[MissingDependency, JsonDependency]] = {
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
    : Set[Either[MissingDependency, JsonDependency]] = {
    val dependencies = checkFragments(expressionString, location) match {
      case None =>
        checkFunctions(expressionString, location)
      case s: Some[Set[Either[MissingDependency, JsonDependency]]] =>
        s
    }
    dependencies.getOrElse(Set(Left(MissingDependency(location, expressionString, expressionString))))
  }

  def levelDependencies(location: NonEmptyList[String],
                        otherLocations: List[NonEmptyList[String]],
                        rest: List[Json],
                        values: Map[String, Json])
    : (List[Json],
       List[NonEmptyList[String]],
       Set[Either[MissingDependency, JsonDependency]]) = {
    val updatedRest = rest ++ values.values
    val indexes =
      values.keys.flatMap(key => NonEmptyList.fromList(location.toList :+ key))
    val updatedLocations = otherLocations ++ indexes
    val dependencies = Set.empty[Either[MissingDependency, JsonDependency]]
    (updatedRest, updatedLocations, dependencies)
  }

  @tailrec final def findElementDependencies(
      fragment: ParsedJsonFragment,
      elements: List[Json],
      locations: List[NonEmptyList[String]] = Nil,
      dependencies: Map[ParsedJsonFragment,
                        Set[Either[MissingDependency, JsonDependency]]] =
        Map.empty): Map[ParsedJsonFragment,
                        Set[Either[MissingDependency, JsonDependency]]] = {
    val (nextElements: List[Json],
         nextLocations: List[NonEmptyList[String]],
         fragmentDependencies: Map[
           ParsedJsonFragment,
           Set[Either[MissingDependency, JsonDependency]]]) = {
      val (nextElements, nextLocations, fragmentDependencies): (List[Json],
                                                                List[
                                                                  NonEmptyList[
                                                                    String]],
                                                                Set[Either[
                                                                  MissingDependency,
                                                                  JsonDependency]]) = {
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
