package compiler

import compiler.Errors.{UnexpectedToken, UnmatchedLeftParenthesis, UnmatchedRightParenthesis}
import compiler.Tokens._

import scala.annotation.tailrec

case class Expression(tokens: List[Token]) extends Element {
  override val name: Identifier = Identifier("_expression")

  def evaluate(block: Block = Block.empty): List[EvaluatedToken] = tokens.foldLeft(List.empty[EvaluatedToken]) {
    case (DivisionByZero :: Nil, _) => List(DivisionByZero)
    case (acc, identifier: Identifier) => block.get(identifier) match {
      case Some(asg: Assignment) =>
        asg.expression.tokens ++ acc
      case None =>
        acc
    }
    case (acc, token: Integer) => token :: acc
    case (acc, token: Floating) => token :: acc
    case (Integer(x) :: xs, Operator(Negate)) => Integer(-x) :: xs
    case (Floating(x) :: xs, Operator(Negate)) => Floating(-x) :: xs
    case (Integer(x) :: Integer(y) :: ys, Operator(Add)) => Integer(y + x) :: ys
    case (Integer(x) :: Integer(y) :: ys, Operator(Subtract)) => Integer(y - x) :: ys
    case (Integer(x) :: Integer(y) :: ys, Operator(Multiply)) => Integer(y * x) :: ys
    case (Integer(x) :: Integer(y) :: ys, Operator(Divide)) if x != 0 => Integer(y / x) :: ys
    case (Integer(x) :: Integer(y) :: ys, Operator(Divide)) => List(DivisionByZero)
    case (Floating(x) :: Integer(y) :: ys, Operator(Add)) => Floating(y + x) :: ys
    case (Floating(x) :: Integer(y) :: ys, Operator(Subtract)) => Floating(y - x) :: ys
    case (Floating(x) :: Integer(y) :: ys, Operator(Multiply)) => Floating(y * x) :: ys
    case (Floating(x) :: Integer(y) :: ys, Operator(Divide)) => Floating(y / x) :: ys
    case (Integer(x) :: Floating(y) :: ys, Operator(Add)) => Floating(y + x) :: ys
    case (Integer(x) :: Floating(y) :: ys, Operator(Subtract)) => Floating(y - x) :: ys
    case (Integer(x) :: Floating(y) :: ys, Operator(Multiply)) => Floating(y * x) :: ys
    case (Integer(x) :: Floating(y) :: ys, Operator(Divide)) => Floating(y / x) :: ys
    case (Floating(x) :: Floating(y) :: ys, Operator(Add)) => Floating(y + x) :: ys
    case (Floating(x) :: Floating(y) :: ys, Operator(Subtract)) => Floating(y - x) :: ys
    case (Floating(x) :: Floating(y) :: ys, Operator(Multiply)) => Floating(y * x) :: ys
    case (Floating(x) :: Floating(y) :: ys, Operator(Divide)) => Floating(y / x) :: ys
  }
}

object Expression {

  /**
   * Shunting-yard algorithm by Edsger W. Dijkstra
   */
  @tailrec
  def parse(tokens: List[Token],
            outputStack: List[Token],
            operatorStack: List[Token],
            lastToken: Option[Token]): Result[Expression] =
    tokens match {
      case (identifier: Identifier) :: xs =>
        parse(xs, outputStack :+ identifier, operatorStack, Some(identifier))
      case (token: ValueToken) :: xs =>
        parse(xs, outputStack :+ token, operatorStack, Some(token))
      case (token: Operator) :: xs =>
        val isUnaryOperator = lastToken.forall {
          case Operator(_) | LeftParenthesis => true
          case _ => false
        }
        val op = token.op match {
          case Subtract if isUnaryOperator => Negate
          case same => same
        }
        val (left, right) = operatorStack.span {
          case Operator(currOp) if currOp.hasGreaterPrecedenceThan(op) => true
          case _ => false
        }
        parse(xs, outputStack ++ left, Operator(op) +: right, Some(token))
      case (token@LeftParenthesis) :: xs =>
        parse(xs, outputStack, token +: operatorStack, Some(token))
      case (token@RightParenthesis) :: xs =>
        operatorStack.span {
          case LeftParenthesis => false
          case _ => true
        } match {
          case (left, Nil) =>
            Result(UnmatchedRightParenthesis())
          case (left, right) =>
            parse(xs, outputStack ++ left, right.tail, Some(token))
        }
      case Indentation(length) :: xs if lastToken.isEmpty =>
        parse(xs, outputStack, operatorStack, lastToken)
      case Indentation(length) :: xs =>
        finishExpression(xs, outputStack, operatorStack)
      case other :: xs =>
        Result(UnexpectedToken(other), xs)
      case Nil =>
        finishExpression(Nil, outputStack, operatorStack)
    }

  private def finishExpression(rest: List[Token],
                               outputStack: List[Token],
                               operatorStack: List[Token]): Result[Expression] = {
    if (operatorStack.contains(LeftParenthesis)) {
      Result(UnmatchedLeftParenthesis())
    } else {
      Result(Expression(outputStack ++ operatorStack), rest)
    }
  }
}
