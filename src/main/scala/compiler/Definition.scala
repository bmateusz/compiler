package compiler

import compiler.Errors.UnexpectedToken
import compiler.Tokens._

case class Definition(name: Identifier,
                      value: ParameterList)

object Definition {

  def parse(tokens: List[Token], block: Block): Result[Definition] =
    tokens match {
      case (identifier: Identifier) :: xs =>
        ParameterList
          .parseParameterList(xs)
          .map { (parameterList, rest) =>
            Result(
              Definition(identifier, parameterList),
              rest
            )
          }
      case other :: xs =>
        Result(UnexpectedToken(other), xs)
    }

}
