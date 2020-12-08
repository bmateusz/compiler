package compiler

import compiler.Errors.{UnexpectedToken, UnmatchedLeftParenthesis, UnmatchedRightParenthesis}
import compiler.Tokens._
import compiler.elements.{Assignment, Block, Class, Definition}
import compiler.Tokens.TokenListExtension

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

case class Expression(tokens: List[EvaluatedToken]) {

  def evaluate(block: Block = Block.empty): List[EvaluatedToken] =
    evaluate(tokens, block)

  def evaluate(tokens: List[EvaluatedToken], block: Block): List[EvaluatedToken] = tokens.foldLeft(List.empty[EvaluatedToken]) {
    case ((ee: EvaluationError) :: Nil, _) => List(ee)
    case ((field: Identifier) :: (identifier: Identifier) :: xs, Operator(Dot)) =>
      dot(block, identifier, field, xs)
    case ((field: Identifier) :: (cli: ClassInstance) :: xs, Operator(Dot)) =>
      dot(block, cli, field, xs)
    case (acc, pc: ParsedCall) =>
      parsedCall(block, pc, acc)
    case (acc, identifier: Identifier) =>
      block.get(identifier) match {
        case Some(asg: Assignment) =>
          asg.constantOrIdentifier ++ acc
        case Some(other) =>
          List(EvaluationError(UnexpectedIdentifier(other.name)))
        case None =>
          identifier :: acc
      }
    case (acc, cli: ClassInstance) => cli :: acc
    case (acc, token: Integer) => token :: acc
    case (acc, token: Floating) => token :: acc
    case (acc, token: StringLiteral) => token :: acc
    case (Integer(x) :: xs, Operator(Negate)) => Integer(-x) :: xs
    case (Floating(x) :: xs, Operator(Negate)) => Floating(-x) :: xs
    case (Integer(x) :: Integer(y) :: ys, Operator(Add)) => Integer(y + x) :: ys
    case (Integer(x) :: Integer(y) :: ys, Operator(Subtract)) => Integer(y - x) :: ys
    case (Integer(x) :: Integer(y) :: ys, Operator(Multiply)) => Integer(y * x) :: ys
    case (Integer(x) :: Integer(y) :: ys, Operator(Divide)) if x != 0 => Integer(y / x) :: ys
    case (Integer(_) :: Integer(_) :: _, Operator(Divide)) => List(EvaluationError(DivisionByZero))
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
    case (StringLiteral(x) :: StringLiteral(y) :: ys, Operator(Add)) => StringLiteral(y + x) :: ys
    case (acc, other) => List(EvaluationError(UnexpectedEvaluation(acc, other)))
  }

  private def dot(block: Block, cli: ClassInstance, field: Identifier, xs: List[EvaluatedToken]): List[EvaluatedToken] =
    block.get(cli.identifier) match {
      case Some(cls: Class) =>
        cls.parameters.findParameter(field) match {
          case Some((parameter, n)) =>
            cli.values(n) ++ xs
          case None =>
            List(EvaluationError(UnexpectedIdentifier(cli.identifier)))
        }
      case None =>
        List(EvaluationError(UnexpectedIdentifier(cli.identifier)))
    }

  @tailrec
  private def dot(block: Block, identifier: Identifier, field: Identifier, xs: List[EvaluatedToken]): List[EvaluatedToken] =
    block.get(identifier) match {
      case Some(asg: Assignment) =>
        asg.expression.tokens match {
          case (v: ValueToken) :: Nil =>
            v :: xs
          case (cli: ClassInstance) :: Nil =>
            dot(block, cli, field, xs)
          case (identifier: Identifier) :: Nil =>
            dot(block, identifier, field, xs)
          case other =>
            List(EvaluationError(UnexpectedIdentifier(field)))
        }
      case Some(other) =>
        List(EvaluationError(UnexpectedIdentifier(field)))
      case None =>
        List(EvaluationError(UnexpectedIdentifier(identifier)))
    }

  def parsedCall(block: Block, pc: ParsedCall, acc: List[EvaluatedToken]): List[EvaluatedToken] =
    block.get(pc.identifier) match {
      case Some(cls: Class) =>
        pc.expression
          .tokens
          .splitByComma()
          .map(evaluate(_, block))
          .pipe { tokens =>
            ClassInstance(pc.identifier, tokens) :: acc
          }
      case Some(df: Definition) =>
        pc.expression
          .tokens
          .splitByComma()
          .map(evaluate(_, block))
          .pipe { tokens =>
            CallDefinition(pc.identifier, tokens) :: acc
          }
      case Some(other) =>
        List(EvaluationError(UnexpectedIdentifier(other.name)))
      case None =>
        pc.identifier :: acc
    }
}

object Expression {

  def parse(tokens: List[Token]): Result[Expression] =
    parse(tokens, List.empty, List.empty, None)

  /**
   * Shunting-yard algorithm by Edsger W. Dijkstra
   */
  @tailrec
  private def parse(tokens: List[Token],
                    outputStack: List[ParsedToken],
                    operatorStack: List[Token],
                    lastToken: Option[Token]): Result[Expression] =
    tokens match {
      case (identifier: Identifier) :: xs =>
        parse(xs, outputStack :+ identifier, operatorStack, Some(identifier))
      case (token: ValueToken) :: xs =>
        parse(xs, outputStack :+ token, operatorStack, Some(token))
      case Comma :: xs =>
        parse(xs, outputStack :+ Comma, operatorStack, Some(Comma))
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
        lastToken match {
          case Some(identifier: Identifier) =>
            xs.spanMatchingRightParenthesis() match {
              case (_, Nil) =>
                Result(UnmatchedRightParenthesis())
              case (left, right) =>
                parse(left) match {
                  case Result(Right(innerExpression: Expression), _: List[Token]) =>
                    parse(right, outputStack.init :+ ParsedCall(identifier, innerExpression), token +: operatorStack, Some(RightParenthesis))
                  case err =>
                    err
                }
            }
          case _ =>
            parse(xs, outputStack, token +: operatorStack, Some(token))
        }
      case (token@RightParenthesis) :: xs =>
        operatorStack.span {
          case LeftParenthesis => false
          case _ => true
        } match {
          case (_, Nil) =>
            Result(UnmatchedRightParenthesis())
          case (left, right) =>
            parse(xs, outputStack ++ left, right.tail, Some(token))
        }
      case Indentation(_) :: xs if lastToken.isEmpty =>
        parse(xs, outputStack, operatorStack, lastToken)
      case Indentation(_) :: xs =>
        finishExpression(xs, outputStack, operatorStack)
      case other :: xs =>
        Result(UnexpectedToken(other), xs)
      case Nil =>
        finishExpression(Nil, outputStack, operatorStack)
    }

  private def finishExpression(rest: List[Token],
                               outputStack: List[ParsedToken],
                               operatorStack: List[Token]): Result[Expression] =
    if (operatorStack.contains(LeftParenthesis)) {
      Result(UnmatchedLeftParenthesis())
    } else {
      Result(Expression(outputStack ++ operatorStack), rest)
    }

}
