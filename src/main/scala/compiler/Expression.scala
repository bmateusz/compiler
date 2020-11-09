package compiler

import compiler.Errors.UnmatchedLeftParenthesis
import compiler.Tokens._

import scala.annotation.tailrec

case class Expression(tokens: List[ParsedToken]) {
  def evaluate: List[EvaluatedToken] = tokens.foldLeft(List.empty[EvaluatedToken]) {
    case (acc, token@Integer(_)) => token :: acc
    case (Integer(x) :: xs, Operator(Negate)) => Integer(-x) :: xs
    case (Integer(x) :: Integer(y) :: ys, Operator(Add)) => Integer(y + x) :: ys
    case (Integer(x) :: Integer(y) :: ys, Operator(Subtract)) => Integer(y - x) :: ys
    case (Integer(x) :: Integer(y) :: ys, Operator(Multiply)) => Integer(y * x) :: ys
    case (Integer(x) :: Integer(y) :: ys, Operator(Divide)) if x != 0 => Integer(y / x) :: ys
    case (Integer(x) :: Integer(y) :: ys, Operator(Divide)) => return List(DivisionByZero)
  }
}

object Expression {

  /**
   * Shunting-yard algorithm by Edsger W. Dijkstra
   */
  @tailrec
  def parse(tokens: List[Token],
            outputStack: List[ParsedToken],
            operatorStack: List[ParsedToken],
            lastToken: Option[Token]): Result[Expression] =
    tokens match {
      case x :: xs =>
        x match {
          case token@Integer(value) =>
            parse(xs, outputStack :+ token, operatorStack, Some(x))
          case token@Operator(_) =>
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
            parse(xs, outputStack ++ left, Operator(op) +: right, Some(x))
          case token@LeftParenthesis =>
            parse(xs, outputStack, token +: operatorStack, Some(x))
          case RightParenthesis =>
            val (left, right) = operatorStack.span {
              case LeftParenthesis => false
              case _ => true
            }
            parse(xs, outputStack ++ left, right.tail, Some(x))
          case Indentation(length) if lastToken.isEmpty =>
            parse(xs, outputStack, operatorStack, lastToken)
          case Indentation(length) =>
            finishExpression(xs, outputStack, operatorStack)
          case Def => ???
          case Class => ???
          case NotImplemented => ???
          case If => ???
          case Else => ???
          case Colon => ???
          case Equals => ???
          case Comma => ???
          case Dot => ???
          case token@Identifier(value) => ???
          case StringLiteral(value) => ???
        }
      case Nil =>
        finishExpression(Nil, outputStack, operatorStack)
    }

  private def finishExpression(rest: List[Token],
                               outputStack: List[ParsedToken],
                               operatorStack: List[ParsedToken]): Result[Expression] = {
    if (operatorStack.contains(LeftParenthesis)) {
      Result(UnmatchedLeftParenthesis())
    } else {
      Result(Expression(outputStack ++ operatorStack), rest)
    }
  }
}
