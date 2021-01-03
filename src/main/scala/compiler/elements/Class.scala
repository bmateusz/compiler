package compiler.elements

import compiler.Errors.{ExpectedIdentifier, Redefinition, UnexpectedReturnType}
import compiler.Expression.EvaluationMode
import compiler.Result
import compiler.Tokens.{Identifier, Indentation, Token}

case class Class(name: Identifier,
                 parameters: Parameters,
                 innerBlock: Block) extends Element {
  override def evaluate(block: Block, rest: List[Token], em: EvaluationMode): Result[Element] =
    block.get(name) match {
      case Some(value) =>
        Result(Redefinition(name.value), rest)
      case None =>
        Result(copy(innerBlock = innerBlock.setParent(block)), rest)
    }
}

object Class {

  def parse(tokens: List[Token], indentation: Indentation): Result[Class] =
    tokens match {
      case (identifier: Identifier) :: xs =>
        Parameters
          .parse(xs)
          .flatMap {
            case ((_, Some(typ)), rest) =>
              Result(UnexpectedReturnType(typ), rest)
            case ((parameters, None), rest) =>
              Block.parse(rest, Block.empty, Some(indentation.right), exprs = false).flatMap { (resultBlock, resultRest) =>
                Result(
                  Class(identifier, parameters, resultBlock),
                  resultRest
                )
              }
          }
      case other :: xs =>
        Result(ExpectedIdentifier(Some(other)), xs)
      case Nil =>
        Result(ExpectedIdentifier(None))
    }

}
