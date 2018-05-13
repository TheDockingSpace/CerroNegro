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

case class GenerationResult(generatedByName: Map[String, GeneratedJson],
                            failed: Map[String, GenerationFailure])

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
    fragmentDependencies: Map[String, Set[String]],
)

case class MissingDependency(missingDependencyName: String)

trait JsonDependency {
  val dependencyName: String
}

case class FragmentDependency(jsonFragment: JsonFragment)
    extends JsonDependency {
  override val dependencyName: String = jsonFragment.fragmentName
}

case class FunctionDependency(jsonFunction: ParsedJsonFunction)
    extends JsonDependency {
  override val dependencyName: String = jsonFunction.functionName.name
}

trait GenerationContext {
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

  def checkFragments(expressionString: String)
    : Option[Set[Either[MissingDependency, JsonDependency]]] = {
    val dependencies: Option[Set[Either[MissingDependency, JsonDependency]]] =
      fragmentsByName
        .get(expressionString)
        .map(f => Set(Right(FragmentDependency(f))))
    dependencies
  }

  def checkFunctions(expressionString: String)
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
      jsonFunction.map(f => Set(Right(FunctionDependency(f))))
    val functionDependencies
      : Option[Set[Either[MissingDependency, JsonDependency]]] =
      jsonFunction.flatMap { jsonFunction: ParsedJsonFunction =>
        val functionDependencies =
          jsonFunction.jsonFunctionDependencyNames.flatMap { expressionString =>
            checkMatchedExpressions(expressionString)
          }
        if (functionDependencies.isEmpty) None else functionDependencies.some
      }
    functionDependency |+| functionDependencies
  }

  def checkExpressions(
      stringValue: String): Set[Either[MissingDependency, JsonDependency]] = {
    val matcher = ExpressionRegex.pattern.matcher(stringValue)
    if (matcher.matches()) {
      checkMatchedExpressions(
        matcher.toMatchResult
          .group(1))
    } else {
      Set.empty
    }
  }

  def checkMatchedExpressions(expressionString: String)
    : Set[Either[MissingDependency, JsonDependency]] = {
    val dependencies = checkFragments(expressionString) match {
      case None =>
        checkFunctions(expressionString)
      case s: Some[Set[Either[MissingDependency, JsonDependency]]] =>
        s
    }
    dependencies.getOrElse(Set(Left(MissingDependency(expressionString))))
  }

  @tailrec final def findElementDependencies(
      fragment: ParsedJsonFragment,
      elements: List[Json],
      dependencies: Map[ParsedJsonFragment,
                        Set[Either[MissingDependency, JsonDependency]]] =
        Map.empty): Map[ParsedJsonFragment,
                        Set[Either[MissingDependency, JsonDependency]]] = {
    val (nextElements: List[Json],
         fragmentDependencies: Map[
           ParsedJsonFragment,
           Set[Either[MissingDependency, JsonDependency]]]) = {
      val (nextElements, fragmentDependencies): (List[Json],
                                                 Set[Either[MissingDependency,
                                                            JsonDependency]]) =
        elements match {
          case json :: rest if json.isString =>
            (rest,
             dependencies
               .getOrElse(fragment, Set.empty) ++ checkExpressions(
               json.stringOrEmpty))
          case json :: rest if json.isArray =>
            (rest ++ json.arrayOrEmpty, Set.empty)
          case json :: rest if json.isObject =>
            (rest ++ json.objectValuesOrEmpty, Set.empty)
          case Nil =>
            (Nil, Set.empty)
          case _ :: rest =>
            (rest, Set.empty)
        }
      (nextElements,
       if (fragmentDependencies.isEmpty) dependencies
       else Map(fragment -> fragmentDependencies) |+| dependencies)
    }
    if (nextElements.isEmpty) {
      fragmentDependencies
    } else {
      findElementDependencies(fragment, nextElements, fragmentDependencies)
    }
  }

}
