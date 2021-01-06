package compiler.elements

import compiler.Errors.{AssignmentError, TypeError}
import compiler.Expression.EvaluationMode
import compiler.Tokens.{EvaluatedToken, EvaluationError, Identifier, Indentation, Token, ValueToken}
import compiler.Types.Type
import compiler.{Expression, Result, Types}

import scala.annotation.tailrec

case class Assignment(name: Identifier,
                      typ: Option[Type],
                      expression: Expression) extends Element {
  override def evaluate(block: Block, rest: List[Token], em: EvaluationMode): Result[Assignment] =
    expression.evaluate(block, em) match {
      case EvaluationError(token) =>
        Result(AssignmentError(token), rest)
      case evaluatedToken =>
        val calculatedType = Types.fromEvaluatedToken(evaluatedToken)
        typ match {
          case Some(providedType) =>
            if (calculatedType == providedType)
              Result(Assignment(name, typ, Expression(List(evaluatedToken))), rest)
            else
              Result(TypeError(calculatedType, providedType), rest)
          case None =>
            Result(Assignment(name, Some(calculatedType), Expression(List(evaluatedToken))), rest)
        }
    }

  def constantOrIdentifier(): List[EvaluatedToken] =
    expression.tokens match {
      case Nil => Nil
      case (v: ValueToken) :: Nil => v :: Nil
      case _ => name :: Nil
    }

  def singleTokenOrIdentifier(): EvaluatedToken =
    expression.tokens match {
      case (v: EvaluatedToken) :: Nil => v
      case _ => name
    }
}

object Assignment {

  @tailrec
  def parse(identifier: Identifier, typ: Option[Identifier], tokens: List[Token]): Result[Assignment] =
    tokens match {
      case Indentation(_) :: xs =>
        parse(identifier, typ, xs)
      case xs =>
        Expression
          .parse(xs)
          .flatMap { (expression, rest) =>
            Result(Assignment(identifier, typ.map(t => Types.parse(t.value)), expression), rest)
          }
    }

}
