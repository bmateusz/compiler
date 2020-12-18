package compiler.elements

import compiler.Errors.Redefinition
import compiler.Expression.EvaluationMode
import compiler.Result
import compiler.Tokens.{Identifier, Token}

trait Element {
  val name: Identifier

  def evaluate(block: Block, rest: List[Token], em: EvaluationMode): Result[Element] =
    block.get(name) match {
      case Some(value) =>
        Result(Redefinition(name.value), rest)
      case None =>
        Result(this, rest)
    }
}
