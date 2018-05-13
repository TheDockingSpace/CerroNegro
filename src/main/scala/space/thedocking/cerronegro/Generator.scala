package space.thedocking.cerronegro

trait Generator {
  def process(context: GenerationContext): GenerationResult
}
