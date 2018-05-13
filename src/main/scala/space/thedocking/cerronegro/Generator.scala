package space.thedocking.cerronegro

case class GenerationResult(generatedByName: Map[String, GeneratedJson],
                            failed: Map[String, GenerationFailure])

trait Generator {
  def process(context: GenerationContext): GenerationResult
}

class JsonGenerator extends Generator {
  override def process(context: GenerationContext): GenerationResult = {
    context.rootFragments.map { f =>

    }
    ???
  }
}
