package compiler.elements

import compiler.Errors.{DefinitionWithoutBody, ExpectedIdentifier, Redefinition, UnexpectedToken}
import compiler.Expression.EvaluationMode
import compiler.Tokens.{Equals, EvaluatedToken, Identifier, Indentation, Token}
import compiler.Types.Type
import compiler.{Expression, Result}

case class Definition(name: Identifier,
                      parameters: Parameters,
                      returnType: Option[Type],
                      innerBlock: Option[Block]) extends Element {
  override def evaluate(block: Block, rest: List[Token], em: EvaluationMode): Result[Element] =
    block.get(name) match {
      case Some(value) =>
        Result(Redefinition(name.value), rest)
      case None =>
        Result(copy(innerBlock = innerBlock.map(_.setParent(block))), rest)
    }

  def call(values: List[EvaluatedToken]): Result[Block] =
    innerBlock match {
      case Some(definitionBlock) =>
        parameters
          .values
          .zip(values)
          .foldLeft(Result(Block.empty)) { case (currBlock, (parameter, value)) =>
            currBlock.flatMapValue(_.add(Assignment(parameter.identifier, None, Expression(List(value))), Nil))
          }
          .flatMapValue(parameterBlock => parameterBlock.evaluate(definitionBlock))
      case None =>
        Result(DefinitionWithoutBody(name.value))
    }
}

object Definition {

  def parse(tokens: List[Token], indentation: Indentation): Result[Definition] =
    tokens match {
      case (identifier: Identifier) :: xs =>
        Parameters
          .parse(xs)
          .flatMap {
            case ((parameters, returnType), Equals :: rest) =>
              Block.parse(rest, Block.empty, Some(indentation.right), exprs = true).flatMap { (resultBlock, resultRest) =>
                Result(
                  Definition(identifier, parameters, returnType, Some(resultBlock)),
                  resultRest
                )
              }
            case ((parameters, returnType), rest) =>
              Result(
                Definition(identifier, parameters, returnType, None),
                rest
              )
          }
      case other :: xs =>
        Result(UnexpectedToken(other), xs)
      case Nil =>
        Result(ExpectedIdentifier(None))
    }

}
