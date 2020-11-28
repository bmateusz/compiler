package compiler.elements

import compiler.Errors.{ExpectedIdentifier, UnexpectedReturnType}
import compiler.Tokens.{Identifier, Token}
import compiler.Result

case class Class(name: Identifier,
                 parameters: Parameters) extends Element

object Class {

  def parse(tokens: List[Token]): Result[Class] =
    tokens match {
      case (identifier: Identifier) :: xs =>
        Parameters
          .parse(xs)
          .flatMap {
            case (Parameters(_, Some(typ)), rest) =>
              Result(UnexpectedReturnType(typ), rest)
            case (parameters, rest) =>
              Result(
                Class(identifier, parameters),
                rest
              )
          }
      case other :: xs =>
        Result(ExpectedIdentifier(Some(other)), xs)
      case Nil =>
        Result(ExpectedIdentifier(None))
    }

}
