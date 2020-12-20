package compiler.elements

import compiler.Errors.{DefinitionWithoutBody, ExpectedIdentifier, UnexpectedToken}
import compiler.Tokens.{Equals, EvaluatedToken, Identifier, Token}
import compiler.Types.Type
import compiler.{Expression, Result}

case class Definition(name: Identifier,
                      parameters: Parameters,
                      returnType: Option[Type],
                      block: Option[Block]) extends Element {
  def call(values: List[List[EvaluatedToken]], parent: Block): Result[Block] =
    block match {
      case Some(definitionBlock) =>
        parameters
          .values
          .zip(values)
          .foldLeft(Result(parent)) { case (currBlock, (parameter, value)) =>
            currBlock.flatMapValue(_.add(Assignment(parameter.identifier, Expression(value)), List.empty))
          }
          .flatMapValue(parameterBlock => parameterBlock.evaluate(definitionBlock))
      case None =>
        Result(DefinitionWithoutBody(name.value))
    }
}

object Definition {

  def parse(tokens: List[Token]): Result[Definition] =
    tokens match {
      case (identifier: Identifier) :: xs =>
        Parameters
          .parse(xs)
          .flatMap {
            case ((parameters, returnType), Equals :: rest) =>
              Block.parse(rest, Block.empty, List.empty).flatMap { (resultBlock, resultRest) =>
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
