package space.thedocking.cerronegro

trait JsonFunctionParser {
  val functionName: Symbol
  def eval(jsonFunctionArguments: List[String]): ParsedJsonFunction
}

trait ParsedJsonFunction {
  val functionName: Symbol
  val jsonFunctionArguments: List[String]
  val jsonFunctionDependencyNames: Set[String]
  val isValid: Boolean
}

case object EachJsonFunctionParser extends JsonFunctionParser {
  override val functionName: Symbol = 'each
  override def eval(arguments: List[String]): ParsedJsonFunction =
    EachJsonFunction(arguments)
}

final case class EachJsonFunction(override val jsonFunctionArguments: List[String])
    extends ParsedJsonFunction {
  override val functionName: Symbol = EachJsonFunctionParser.functionName
  override val jsonFunctionDependencyNames: Set[String] = Set(
    jsonFunctionArguments(0))
  override val isValid: Boolean = jsonFunctionArguments.size == 1
}

case object EchoJsonFunctionParser extends JsonFunctionParser {
  override val functionName: Symbol = 'echo
  override def eval(arguments: List[String]): ParsedJsonFunction =
    EchoJsonFunction(arguments)
}

final case class EchoJsonFunction(override val jsonFunctionArguments: List[String])
  extends ParsedJsonFunction {
  override val functionName: Symbol = EachJsonFunctionParser.functionName
  override val jsonFunctionDependencyNames: Set[String] = Set.empty
  override val isValid: Boolean = jsonFunctionArguments.size > 0
}
