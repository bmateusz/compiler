package compiler

import compiler.Errors.{ExpectedIdentifier, UnexpectedReturnType, UnexpectedToken}
import compiler.Tokens.{Identifier, Token}

case class Class(name: Identifier,
                 parameters: Parameters)

object Class {

  def parse(tokens: List[Token], block: Block): Result[Class] =
    tokens match {
      case (identifier: Identifier) :: xs =>
        Parameters
          .parse(xs)
          .map {
            case (Parameters(_, Some(typ)), rest) =>
              Result(UnexpectedReturnType(typ), rest)
            case (parameters, rest) =>
              Result(
                Class(identifier, parameters),
                rest
              )
          }
      case other :: xs =>
        Result(UnexpectedToken(other), xs)
      case Nil =>
        Result(ExpectedIdentifier(None))
    }

}
