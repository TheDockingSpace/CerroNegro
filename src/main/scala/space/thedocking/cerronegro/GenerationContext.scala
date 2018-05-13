package space.thedocking.cerronegro

import argonaut._
import Argonaut._
import cats.instances.all._
import cats.syntax.all._

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.matching.Regex

sealed trait ProcessFailure {
  val failure: String
}

sealed trait ProcessSuccess {
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
  val name: String
  def str: String
}

case class GenerationSuccess(override val name: String, override val json: Json)
    extends GeneratedJson
    with ProcessSuccess {
  override lazy val str: String = json.spaces4
}

case class GenerationFailure(override val name: String,
                             override val str: String,
                             json: Json,
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
//    rootFragments: List[String]
)

case class MissingDependency(missingDependencyName: String)

sealed trait JsonDependency {
  val dependencyName: String
  val location: List[String]
}

case class FragmentDependency(override val location: List[String],
                              jsonFragment: JsonFragment)
    extends JsonDependency {
  override val dependencyName: String = jsonFragment.fragmentName
}

case class FunctionDependency(override val location: List[String],
                              jsonFunction: ParsedJsonFunction)
    extends JsonDependency {
  override val dependencyName: String = jsonFunction.functionName.name
}

trait GenerationContext {
  def rootFragments: Set[ParsedJsonFragment]
  def toVO: GenerationContextVO
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
            fragment.fragmentName -> missing.map(_.missingDependencyName)
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
        implicit classTag: ClassTag[T]) =
      grouped
        .getOrElse(classTag.runtimeClass.asInstanceOf[Class[T]], Nil)
        .map(f => f.fragmentName -> f)
        .toMap
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

  lazy val nonRootFragments: Set[ParsedJsonFragment] =
    fragmentDependencies.values.flatten.flatMap { d: JsonDependency =>
      val fragments: Set[ParsedJsonFragment] = d match {
        case FragmentDependency(_, f: ParsedJsonFragment) => Set(f)
        case FunctionDependency(_, jsonFunction) =>
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

  def checkFragments(expressionString: String, location: List[String])
    : Option[Set[Either[MissingDependency, JsonDependency]]] = {
    val dependencies: Option[Set[Either[MissingDependency, JsonDependency]]] =
      fragmentsByName
        .get(expressionString)
        .map(f => Set(Right(FragmentDependency(location, f))))
    dependencies
  }

  def checkFunctions(expressionString: String, location: List[String])
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
      jsonFunction.map(f => Set(Right(FunctionDependency(location, f))))
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

  def checkExpressions(stringValue: String, location: List[String])
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

  def checkMatchedExpressions(expressionString: String, location: List[String])
    : Set[Either[MissingDependency, JsonDependency]] = {
    val dependencies = checkFragments(expressionString, location) match {
      case None =>
        checkFunctions(expressionString, location)
      case s: Some[Set[Either[MissingDependency, JsonDependency]]] =>
        s
    }
    dependencies.getOrElse(Set(Left(MissingDependency(expressionString))))
  }

  @tailrec final def findElementDependencies(
      fragment: ParsedJsonFragment,
      elements: List[Json],
      locations: List[List[String]] = Nil,
      dependencies: Map[ParsedJsonFragment,
                        Set[Either[MissingDependency, JsonDependency]]] =
        Map.empty): Map[
    ParsedJsonFragment,
    Set[Either[MissingDependency, JsonDependency]]] = {
    val (nextElements: List[Json],
         nextLocations: List[List[String]],
         fragmentDependencies: Map[
           ParsedJsonFragment,
           Set[Either[MissingDependency, JsonDependency]]]) = {
      val (nextElements, nextLocations, fragmentDependencies): (List[Json],
                                                                List[
                                                                  List[String]],
                                                                Set[Either[
                                                                  MissingDependency,
                                                                  JsonDependency]]) = {
        val location = locations.headOption.getOrElse(Nil)
        val locationRest = if (locations.isEmpty) Nil else locations.tail
        elements match {
          case json :: rest if json.isString =>
            (rest,
              locationRest,
             dependencies
               .getOrElse(fragment, Set.empty) ++ checkExpressions(
               json.stringOrEmpty,
               location))
          case json :: rest if json.isArray =>
            val jsonArray = json.arrayOrEmpty
            (rest ++ jsonArray, locationRest ++ ((0 until jsonArray.size).map(location :+ _.toString)),  Set.empty)
          case json :: rest if json.isObject =>
            val jsonObjectValues = json.objectValuesOrEmpty
            (rest ++ jsonObjectValues, locationRest ++ ((0 until jsonObjectValues.size).map(location :+ _.toString)), Set.empty)
          case Nil =>
            (Nil, Nil, Set.empty)
          case _ :: rest =>
            (rest, locationRest, Set.empty)
        }}
      (nextElements, nextLocations,
       if (fragmentDependencies.isEmpty) dependencies
       else Map(fragment -> fragmentDependencies) |+| dependencies)
    }
    if (nextElements.isEmpty) {
      fragmentDependencies
    } else {
      findElementDependencies(fragment, nextElements, nextLocations, fragmentDependencies)
    }
  }

}
