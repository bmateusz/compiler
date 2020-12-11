package compiler.elements

import compiler.Errors.{ExpectedIdentifier, UnexpectedToken}
import compiler.Tokens.{Equals, Identifier, Token}
import compiler.Result
import compiler.Types.Type

case class Definition(name: Identifier,
                      parameters: Parameters,
                      returnType: Option[Type],
                      block: Option[Block]) extends Element

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
