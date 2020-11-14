package compiler

import compiler.Errors.{ExpectedIdentifier, UnexpectedToken}
import compiler.Tokens.{Equals, Identifier, Token}

case class Definition(name: Identifier,
                      parameters: Parameters,
                      block: Option[Block]) extends Element

object Definition {

  def parse(tokens: List[Token]): Result[Definition] =
    tokens match {
      case (identifier: Identifier) :: xs =>
        Parameters
          .parse(xs)
          .map {
            case (parameters, Equals :: rest) =>
              Block.parse(rest, Block.empty, List.empty).map { (resultBlock, resultRest) =>
                Result(
                  Definition(identifier, parameters, Some(resultBlock)),
                  resultRest
                )
              }
            case (parameters, rest) =>
              Result(
                Definition(identifier, parameters, None),
                rest
              )
          }
      case other :: xs =>
        Result(UnexpectedToken(other), xs)
      case Nil =>
        Result(ExpectedIdentifier(None))
    }

}
