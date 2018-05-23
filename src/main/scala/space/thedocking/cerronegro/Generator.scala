package space.thedocking.cerronegro

import argonaut.Json
import cats.Monoid
import cats.data.NonEmptyList
import cats.syntax.all._
import slogging.LazyLogging
import space.thedocking.cerronegro.GenerationContext.ExpressionRegex

import scala.annotation.tailrec

case class GenerationResult(
    generatedByName: Map[String, GeneratedJson] = Map(),
    failedByName: Map[String, GenerationFailure] = Map())

object GenerationResult {
  val empty = GenerationResult()

  def apply(
      outcome: Either[GenerationFailure, GenerationSuccess]): GenerationResult =
    outcome match {
      case Left(f) =>
        GenerationResult(
          failedByName = Map(f.jsonDependency.dependencyName -> f))
      case Right(g) =>
        GenerationResult(
          generatedByName = Map(g.jsonDependency.dependencyName -> g))
    }
}

trait Generator {
  def processGenerationContext(
      implicit context: GenerationContext,
      generationStack: NonEmptyList[GeneratorStackElement]): GenerationResult

  def processGenerationContext(
      implicit context: GenerationContext): GenerationResult =
    processGenerationContext(context, NonEmptyList.of(context))

  implicit val generationResultSemigroup: Monoid[GenerationResult] =
    new Monoid[GenerationResult] {
      override def empty: GenerationResult = GenerationResult.empty
      override def combine(x: GenerationResult,
                           y: GenerationResult): GenerationResult =
        GenerationResult(
          x.generatedByName ++ y.generatedByName,
          x.failedByName ++ y.failedByName
        )
    }

}

object JsonGenerator extends Generator with LazyLogging {

  private def push(element: GeneratorStackElement,
                   generationStack: NonEmptyList[GeneratorStackElement]) = {
    if (generationStack.exists(_ == element)) {
      throw new RuntimeException("Circular dependencies are not supported")
    }
    generationStack.::(element)
  }

  @tailrec def processJsonLevel(
      jsonLevel: Json,
      elementPath: NonEmptyList[String],
      rest: List[String],
      generationStack: NonEmptyList[GeneratorStackElement])(
      implicit context: GenerationContext)
    : Either[GenerationFailure, GenerationSuccess] = {
    val (levelElement, levelRest) = (rest.head, rest.tail)
    val element = jsonLevel match {
      case _ if jsonLevel.isArray => jsonLevel.arrayOrEmpty(levelElement.toInt)
      case _ if jsonLevel.isObject =>
        jsonLevel
          .objectOrEmpty(levelElement)
          .getOrElse(
            throw new RuntimeException(s"Field $levelElement not found"))
      case _ =>
        throw new RuntimeException(
          "Cannot get elements from json value " + jsonLevel)
    }
    val elementStr = element.stringOr(
      throw new RuntimeException("A dependency name was expected here"))

    val matcher = ExpressionRegex.pattern.matcher(elementStr)
    val someExpression: Option[String] = if (matcher.matches()) {
      matcher.toMatchResult
        .group(1)
        .some
    } else {
      none
    }

    if (levelRest.isEmpty) {

      someExpression match {
        case Some(expression) =>
          if (context.missingDependenciesByName.contains(expression)) {
            GenerationFailure(context.missingDependenciesByName(expression),
                              elementStr,
                              s"Cannot resolve dependency $expression").asLeft
          } else {
            val jsonDependency: JsonDependency = context
              .dependenciesByExpression(expression)
            processJsonDependency(jsonDependency,
                                  push(jsonDependency, generationStack))
          }
        case None =>
          logger.debug(elementStr)
          ???
      }
    } else {
      processJsonLevel(element, elementPath, levelRest, generationStack)
    }
  }

  def processJsonDependency(
      dependency: JsonDependency,
      generationStack: NonEmptyList[GeneratorStackElement])(
      implicit context: GenerationContext)
    : Either[GenerationFailure, GenerationSuccess] = {
    logger.debug(dependency.toString)
    ???
  }

  def processParsedJson(json: Json,
                        jsonDependency: JsonDependency,
                        location: NonEmptyList[String],
                        generationStack: NonEmptyList[GeneratorStackElement])(
      implicit context: GenerationContext)
    : Either[GenerationFailure, GenerationSuccess] = {
    Right {
      val r: (Product, List[String]) = location.toList match {
        case elementName :: rest =>
          (processJsonLevel(json,
                            NonEmptyList.of("_root_", elementName),
                            rest,
                            generationStack),
           rest)
        case Nil =>
          ???
      }
      GenerationSuccess(jsonDependency, json)
    }
  }

  def processParsedJsonFragment(
      fragment: ParsedJsonFragment,
      generationStack: NonEmptyList[GeneratorStackElement])(
      implicit context: GenerationContext): GenerationResult = {
    val generationResults = for {
      dependency <- context.fragmentDependencies.getOrElse(fragment, Nil)
      fragmentGeneration <- processParsedJson(fragment.json,
                                              dependency,
                                              dependency.location,
                                              generationStack).some
    } yield GenerationResult(fragmentGeneration)
    Monoid.combineAll(generationResults)
  }

  override def processGenerationContext(
      implicit context: GenerationContext,
      generationStack: NonEmptyList[GeneratorStackElement])
    : GenerationResult = {
    val generationResults = for {
      fragment <- context.rootFragments
    } yield processParsedJsonFragment(fragment, push(fragment, generationStack))
    Monoid.combineAll(generationResults)
  }
}
