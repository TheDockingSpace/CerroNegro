package space.thedocking.cerronegro

import argonaut._
import Argonaut._
import cats.instances.all._
import cats.syntax.semigroup._

import scala.annotation.tailrec
import scala.reflect.ClassTag

sealed trait ProcessFailure {
  val failure: String
}

sealed trait ProcessSuccess {
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

case class MissingJsonFragment(val name: String)

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

trait Generator {
  def generate: GenerationResult
}

case object GenerationContext {
  val DependencyRegex = """<(\w+)>""".r
  def apply(fragments: JsonFragment*): LazyGenerationContext =
    LazyGenerationContext(fragments.map(f => f.name -> f).toMap)
}

case class GenerationContextVO(
    val parsedFragments: Map[String, ParsedJsonFragment],
    val failedFragments: Map[String, FailedJsonFragment],
    val missingDependencies: Map[String, Set[String]],
    val fragmentDependencies: Map[String, Set[String]],
)

case class LazyGenerationContext(
    val fragmentsByName: Map[String, JsonFragment]) {
  import GenerationContext._
  import io.scalaland.chimney.dsl._
  def toVO: GenerationContextVO = {
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
      .transform
  }

  val (parsedFragments: Map[String, ParsedJsonFragment],
       failedFragments: Map[String, FailedJsonFragment]) = {
    val grouped = fragmentsByName.values.groupBy(_.getClass)

    def classFragmentsByName[T <: JsonFragment](
        implicit classTag: ClassTag[T]) =
      grouped
        .getOrElse(classTag.runtimeClass.asInstanceOf[Class[T]], Nil)
        .map(f => (f.name -> f))
        .toMap
    (classFragmentsByName[ParsedJsonFragment],
     classFragmentsByName[FailedJsonFragment])
  }

  lazy val (
    missingDependencies: Map[ParsedJsonFragment, Set[MissingJsonFragment]],
    fragmentDependencies: Map[ParsedJsonFragment, Set[ParsedJsonFragment]]) = {
    val (missing, found) =
      parsedFragments.values.map(findFieldDependencies).unzip
    (missing.fold(Map.empty)(_ |+| _), found.fold(Map.empty)(_ |+| _))
  }

  def findFieldDependencies(fragment: ParsedJsonFragment)
    : (Map[ParsedJsonFragment, Set[MissingJsonFragment]],
       Map[ParsedJsonFragment, Set[JsonFragment]]) = {
    val mixedDependencies =
      findFieldDependencies(fragment, fragment.json :: Nil)
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

  def jsonDependencies(
      stringValue: String): Set[Either[MissingJsonFragment, JsonFragment]] = {
    val matcher = DependencyRegex.pattern.matcher(stringValue)
    val dependencies: Set[Either[MissingJsonFragment, JsonFragment]] =
      if (matcher.matches()) {
        val fragmentName = matcher.toMatchResult
          .group(1)
        Set(
          fragmentsByName
            .get(fragmentName)
            .map(Right(_))
            .getOrElse(Left(MissingJsonFragment(fragmentName))))
      } else {
        Set.empty
      }
    dependencies
  }
  @tailrec final def findFieldDependencies(
      fragment: ParsedJsonFragment,
      fields: List[Json],
      dependencies: Map[ParsedJsonFragment,
                        Set[Either[MissingJsonFragment, JsonFragment]]] =
        Map.empty): Map[ParsedJsonFragment,
                        Set[Either[MissingJsonFragment, JsonFragment]]] = {
    val (nextFields: List[Json],
         fragmentDependencies: Map[
           ParsedJsonFragment,
           Set[Either[MissingJsonFragment, JsonFragment]]]) = {
      val (nextFields, fragmentDependencies): (List[Json],
                                               Set[Either[MissingJsonFragment,
                                                          JsonFragment]]) =
        fields match {
          case json :: rest if json.isString =>
            (rest,
             dependencies.getOrElse(fragment, Set.empty) ++ jsonDependencies(
               json.stringOrEmpty))
          case json :: rest if json.isObject =>
            (rest ++ json.objectValuesOrEmpty, Set.empty)
          case Nil =>
            (Nil, Set.empty)
          case _ :: rest =>
            (rest, Set.empty)
        }
      (nextFields,
       if (fragmentDependencies.isEmpty) dependencies
       else Map(fragment -> fragmentDependencies) |+| (dependencies))
    }
    if (nextFields.isEmpty) {
      fragmentDependencies
    } else {
      findFieldDependencies(fragment, nextFields, fragmentDependencies)
    }
  }

}

object Main extends App {
  import cats.Show
  import cats.implicits._
  import argonaut._
  import Argonaut._

  val fragment = JsonFragment(
    "Bla",
    """
      |{
      |   "userid": "<someid>"
      |}
      |""".stripMargin
  )

  val ctx = GenerationContext(fragment).toVO

  implicit def ParserJsonFragmentCodecJson: CodecJson[ParsedJsonFragment] =
    casecodec3(ParsedJsonFragment.apply, ParsedJsonFragment.unapply)("name",
                                                                     "str",
                                                                     "json")

  implicit def FailedJsonFragmentCodecJson: CodecJson[FailedJsonFragment] =
    casecodec3(FailedJsonFragment.apply, FailedJsonFragment.unapply)("name",
                                                                     "str",
                                                                     "failure")

  implicit def MissingJsonFragmentCodecJson: CodecJson[MissingJsonFragment] =
    casecodec1(MissingJsonFragment.apply, MissingJsonFragment.unapply)("name")

  implicit def GenerationContextVOCodecJson
    : CodecJson[GenerationContextVO] =
    casecodec4(GenerationContextVO.apply,
      GenerationContextVO.unapply)("parsedFragments",
                                                  "failedFragments",
                                                  "missingDependencies",
                                                  "fragmentDependencies")

  implicit val showCtx: Show[GenerationContextVO] =
    Show.show(ctx => ctx.asJson.spaces4)

  println(ctx.show)
}
