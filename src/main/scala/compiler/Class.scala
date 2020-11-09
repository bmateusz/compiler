package compiler

import compiler.Errors.UnexpectedToken
import compiler.Tokens.{Identifier, Token}

case class Class(name: Identifier,
                 parameterList: ParameterList)

object Class {

  def parse(tokens: List[Token], block: Block): Result[Class] =
    tokens match {
      case (identifier: Identifier) :: xs =>
        ParameterList
          .parseParameterList(xs)
          .map { (parameterList, rest) =>
            Result(
              Class(identifier, parameterList),
              rest
            )
          }
      case other :: xs =>
        Result(UnexpectedToken(other), xs)
    }

}
