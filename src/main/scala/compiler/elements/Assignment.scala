package compiler.elements

import compiler.Errors.AssignmentError
import compiler.Tokens.{EvaluatedToken, EvaluationError, Identifier, Indentation, Token, ValueToken}
import compiler.{Expression, Result}

import scala.annotation.tailrec

case class Assignment(name: Identifier,
                      expression: Expression) extends Element {
  override def evaluate(block: Block, rest: List[Token]): Result[Assignment] =
    expression.evaluate(block) match {
      case EvaluationError(token) :: _ =>
        Result(AssignmentError(token), rest)
      case xs =>
        Result(Assignment(name, Expression(xs)), rest)
    }

  def constantOrIdentifier(): List[EvaluatedToken] =
    expression.tokens match {
      case Nil => Nil
      case (v: ValueToken) :: Nil => v :: Nil
      case _ => name :: Nil
    }
}

object Assignment {

  @tailrec
  def parse(identifier: Identifier, tokens: List[Token]): Result[Assignment] =
    tokens match {
      case Indentation(_) :: xs =>
        parse(identifier, xs)
      case xs =>
        Expression
          .parse(xs)
          .flatMap { (expression, rest) =>
            Result(Assignment(identifier, expression), rest)
          }
    }

}
