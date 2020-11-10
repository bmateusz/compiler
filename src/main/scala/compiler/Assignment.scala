package compiler

import compiler.Tokens.{Identifier, Indentation, Token}

import scala.annotation.tailrec

case class Assignment(name: Identifier,
                      expression: Expression)

object Assignment {

  @tailrec
  def parse(identifier: Identifier, tokens: List[Token], block: Block): Result[Assignment] =
    tokens match {
      case Indentation(_) :: xs =>
        parse(identifier, xs, block)
      case xs =>
        Expression
          .parse(xs, List.empty, List.empty, None)
          .map { (expression, rest) =>
            Result(Assignment(identifier, expression), rest)
          }
    }

}
