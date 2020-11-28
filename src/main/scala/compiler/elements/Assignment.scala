package compiler.elements

import compiler.Tokens.{Identifier, Indentation, Token}
import compiler.{Expression, Result}

import scala.annotation.tailrec

case class Assignment(name: Identifier,
                      expression: Expression) extends Element

object Assignment {

  @tailrec
  def parse(identifier: Identifier, tokens: List[Token]): Result[Assignment] =
    tokens match {
      case Indentation(_) :: xs =>
        parse(identifier, xs)
      case xs =>
        Expression
          .parse(xs, List.empty, List.empty, None)
          .flatMap { (expression, rest) =>
            Result(Assignment(identifier, expression), rest)
          }
    }

}
