package space.thedocking.cerronegro

import argonaut.Json
import cats.Monoid
import cats.data.NonEmptyList
import cats.instances.all._
import cats.syntax.all._

import scala.annotation.tailrec

case class GenerationResult(generatedByName: Map[String, GeneratedJson] = Map(),
                            failedByName: Map[String, GenerationFailure] = Map())

object GenerationResult{
  val empty = GenerationResult()

  def apply(outcome: Either[GenerationFailure, GenerationSuccess]): GenerationResult = outcome match {
    case Left(f) => GenerationResult(failedByName = Map(f.jsonDependency.dependencyName -> f))
    case Right(g) => GenerationResult(generatedByName = Map(g.jsonDependency.dependencyName -> g))
  }
}

trait Generator {
  def process(implicit context: GenerationContext): GenerationResult

  implicit val generationResultSemigroup: Monoid[GenerationResult] = new Monoid[GenerationResult] {
    override def empty: GenerationResult = GenerationResult.empty
    override def combine(x: GenerationResult, y: GenerationResult): GenerationResult =
      GenerationResult(
        x.generatedByName ++ y.generatedByName,
        x.failedByName ++ y.failedByName
      )
  }

}

object JsonGenerator extends Generator {

  @tailrec def process(jsonLevel: Json, elementPath: NonEmptyList[String], rest: List[String])
             (implicit context: GenerationContext): Either[GenerationFailure, GenerationSuccess] = {
    val (levelElement, levelRest) = (rest.head, rest.tail)
    val element = jsonLevel match {
      case _ if jsonLevel.isArray => jsonLevel.arrayOrEmpty(levelElement.toInt)
      case _ if jsonLevel.isObject => jsonLevel.objectOrEmpty(levelElement).getOrElse(throw new RuntimeException(s"Field $levelElement not found"))
      case _ => throw new RuntimeException("Cannot get elements from json value " + jsonLevel)
    }
    if (levelRest.isEmpty) {
      ???
    } else {
      process(element, elementPath, levelRest)
    }
  }

  def process(dependency: JsonDependency)
             (implicit context: GenerationContext): Either[GenerationFailure, GenerationSuccess] = ???

  def process(json: Json,
                jsonDependency: JsonDependency,
                location: NonEmptyList[String])
               (implicit context: GenerationContext): Either[GenerationFailure, GenerationSuccess] = {
      Right {
        val r: (Product, List[String]) = location.toList match {
          case elementName :: rest => (process(json, NonEmptyList.of("_root_", elementName), rest), rest)
          case leaf => (process(jsonDependency)(context), Nil)
        }
        GenerationSuccess(jsonDependency, json)
      }
  }

  def process(fragment: ParsedJsonFragment)(implicit context: GenerationContext) : GenerationResult = {
    val generationResults = for {
      dependency <- context.fragmentDependencies.getOrElse(fragment, Nil)
      fragmentGeneration <- process(fragment.json, dependency, dependency.location).some
    } yield GenerationResult(fragmentGeneration)
    Monoid.combineAll(generationResults)
  }

  override def process(implicit context: GenerationContext): GenerationResult = {
    val generationResults = for {
      fragment <- context.rootFragments
    } yield process(fragment)
    Monoid.combineAll(generationResults)
  }
}
