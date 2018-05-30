package space.thedocking.cerronegro

import cats.implicits._
import slogging.{LogLevel, LoggerConfig, PrintLoggerFactory}
import space.thedocking.cerronegro.implicits._

object Main extends App {

  LoggerConfig.factory = PrintLoggerFactory()
  LoggerConfig.level = LogLevel.DEBUG

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

  val ctx = GenerationContext(fragment1, fragment2)
  val ctxVO = ctx.toVO

  println(ctx.rootFragments)
  println(ctxVO.show)

  implicit val generator: Generator = JsonGenerator

  val generated = ctx.generate

  println(generated)
}
