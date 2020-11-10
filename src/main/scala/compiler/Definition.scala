package compiler

import compiler.Errors.UnexpectedToken
import compiler.Tokens.{Identifier, Token}

case class Definition(name: Identifier,
                      parameters: Parameters)

object Definition {

  def parse(tokens: List[Token], block: Block): Result[Definition] =
    tokens match {
      case (identifier: Identifier) :: xs =>
        Parameters
          .parse(xs)
          .map { (parameters, rest) =>
            Result(
              Definition(identifier, parameters),
              rest
            )
          }
      case other :: xs =>
        Result(UnexpectedToken(other), xs)
    }

}
