package compiler

import compiler.Errors.{UnexpectedToken, UnmatchedLeftParenthesis, UnmatchedRightParenthesis}
import compiler.Expression.{EvaluationMode, FullEvaluation, SimpleEvaluation}
import compiler.Result.ResultEither
import compiler.Tokens._
import compiler.elements.{Assignment, Block, Class, Definition, Parameters}

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

case class Expression(tokens: List[EvaluatedToken]) {

  def evaluate(block: Block = Block.empty, em: EvaluationMode = SimpleEvaluation): EvaluatedToken =
    evaluate(tokens, block, em)

  def evaluate(tokens: List[EvaluatedToken], block: Block, em: EvaluationMode): EvaluatedToken = tokens.foldLeft(Nil: List[EvaluatedToken]) {
    case ((ee: EvaluationError) :: Nil, _) => List(ee)
    case ((field: Identifier) :: (identifier: Identifier) :: xs, Operator(Dot)) =>
      dot(block, identifier, field, xs)
    case ((field: Identifier) :: (cli: ClassInstance) :: xs, Operator(Dot)) =>
      dot(block, cli, field, xs)
    case ((pc: ParsedCall) :: (identifier: Identifier) :: xs, Operator(Dot)) =>
      dot(block, identifier, pc, xs, em)
    case (acc, pc: ParsedCall) =>
      parsedCall(block, pc, None, acc, em)
    case (acc, identifier: Identifier) =>
      block.get(identifier) match {
        case Some(asg: Assignment) =>
          asg.constantOrIdentifier ++ acc
        case Some(df: Definition) =>
          checkParameterList(df.parameters, Nil) match {
            case Some(evaluationError) =>
              List(evaluationError)
            case None =>
              postEvaluation(CallDefinition(df, Nil), block, acc, em)
          }
        case Some(other) =>
          List(EvaluationError(UnexpectedIdentifier(other.name)))
        case None =>
          identifier :: acc
      }
    case (acc, cli: ClassInstance) => cli :: acc
    case (acc, token: ValueToken) => token :: acc
    case ((x: ValueToken) :: xs, Operator(Negate)) => unaryOperator(x, xs)
    case ((x: ValueToken) :: (y: ValueToken) :: ys, Operator(op)) => operator(x, y, ys, op)
    case (acc, other) => List(EvaluationError(UnexpectedEvaluation(acc, other)))
  }.pipe { evaluatedTokens =>
    em match {
      case FullEvaluation =>
        fullEvaluate(evaluatedTokens, block)
      case SimpleEvaluation =>
        evaluatedTokens
    }
  }.pipe {
    case Nil => Pass
    case token :: Nil => token
    case more => EvaluationError(TooManyEvaluatedTokens(more))
  }

  def unaryOperator(x: ValueToken, xs: List[EvaluatedToken]): List[EvaluatedToken] =
    x match {
      case Integer(integer) => Integer(-integer) :: xs
      case Floating(double) => Floating(-double) :: xs
      case value: StringLiteral => List(EvaluationError(UnaryOperatorError(Negate, value)))
    }

  def operator(x: ValueToken, y: ValueToken, ys: List[EvaluatedToken], op: Operators): List[EvaluatedToken] =
    (x, y, op) match {
      case (Integer(x), Integer(y), Add) => Integer(y + x) :: ys
      case (Integer(x), Integer(y), Subtract) => Integer(y - x) :: ys
      case (Integer(x), Integer(y), Multiply) => Integer(y * x) :: ys
      case (Integer(x), Integer(y), Divide) if x != 0 => Integer(y / x) :: ys
      case (Integer(_), Integer(_), Divide) => List(EvaluationError(DivisionByZero))
      case (Floating(x), Integer(y), Add) => Floating(y + x) :: ys
      case (Floating(x), Integer(y), Subtract) => Floating(y - x) :: ys
      case (Floating(x), Integer(y), Multiply) => Floating(y * x) :: ys
      case (Floating(x), Integer(y), Divide) => Floating(y / x) :: ys
      case (Integer(x), Floating(y), Add) => Floating(y + x) :: ys
      case (Integer(x), Floating(y), Subtract) => Floating(y - x) :: ys
      case (Integer(x), Floating(y), Multiply) => Floating(y * x) :: ys
      case (Integer(x), Floating(y), Divide) => Floating(y / x) :: ys
      case (Floating(x), Floating(y), Add) => Floating(y + x) :: ys
      case (Floating(x), Floating(y), Subtract) => Floating(y - x) :: ys
      case (Floating(x), Floating(y), Multiply) => Floating(y * x) :: ys
      case (Floating(x), Floating(y), Divide) => Floating(y / x) :: ys
      case (StringLiteral(x), StringLiteral(y), Add) => StringLiteral(y + x) :: ys
      case (a, b, op) => List(EvaluationError(OperatorError(op, b, a)))
    }

  private def postEvaluation(elem: EvaluatedToken, block: Block, acc: List[EvaluatedToken], em: EvaluationMode): List[EvaluatedToken] =
    em match {
      case FullEvaluation =>
        fullEvaluate(List(elem), block) ++ acc
      case SimpleEvaluation =>
        elem :: acc
    }

  private def fullEvaluate(evaluatedTokens: List[EvaluatedToken], block: Block): List[EvaluatedToken] = evaluatedTokens.flatMap {
    case identifier: Identifier =>
      block.get(identifier) match {
        case Some(asg: Assignment) =>
          List(asg.singleTokenOrIdentifier())
        case _ =>
          List(identifier)
      }
    case cd@CallDefinition(definition, values) =>
      definition.call(values).value match {
        case Left(_) =>
          List(cd)
        case Right(definitionBlock) =>
          definitionBlock.expression match {
            case Some(expr) =>
              val result = expr.evaluate(definitionBlock.setParent(block), FullEvaluation)
              val typ = Types.fromEvaluatedToken(result)
              definition.returnType match {
                case Some(returnType) if returnType != typ =>
                  List(EvaluationError(DefinitionReturnTypeMismatch(definition.returnType.get, typ)))
                case _ =>
                  List(result)
              }
            case None =>
              List(cd)
          }
      }
    case ed@EvaluatedDot(cli, cd@CallDefinition(definition, values)) =>
      classInstanceToBlock(cli) match {
        case Left(error) =>
          List(ed)
        case Right(cliBlock) =>
          fullEvaluate(List(cd), cliBlock)
      }
    case other =>
      List(other)
  }

  private def dot(block: Block, cli: ClassInstance, field: Identifier, xs: List[EvaluatedToken]): List[EvaluatedToken] =
    cli.cls.parameters.findParameter(field) match {
      case Some((parameter, n)) =>
        cli.values(n) +: xs
      case None =>
        cli.cls.innerBlock.get(field) match {
          case Some(df: Definition) =>
            EvaluatedDot(cli, CallDefinition(df, Nil)) +: xs
          case Some(other) =>
            List(EvaluationError(UnexpectedIdentifierAfterDot(other)))
          case None =>
            List(EvaluationError(UnexpectedIdentifier(cli.cls.name)))
        }
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
        List(EvaluationError(UnexpectedIdentifierAfterDot(other)))
      case None =>
        List(EvaluationError(UnexpectedIdentifier(identifier)))
    }

  private def dot(block: Block, identifier: Identifier, pc: ParsedCall, xs: List[EvaluatedToken], em: EvaluationMode): List[EvaluatedToken] =
    block.get(identifier) match {
      case Some(asg: Assignment) =>
        asg.expression.tokens match {
          case (cli: ClassInstance) :: Nil =>
            parsedCall(cli.cls.innerBlock, pc, Some(cli), xs, em)
          case other =>
            List(EvaluationError(UnexpectedIdentifier(pc.identifier)))
        }
      case Some(other) =>
        List(EvaluationError(UnexpectedIdentifierAfterDot(other)))
      case None =>
        List(EvaluationError(UnexpectedIdentifier(identifier)))
    }

  def parsedCall(block: Block, pc: ParsedCall, parent: Option[ClassInstance], acc: List[EvaluatedToken], em: EvaluationMode): List[EvaluatedToken] =
    block.get(pc.identifier) match {
      case Some(cls: Class) =>
        pc.expression
          .tokens
          .splitByComma()
          .map(evaluate(_, block, em))
          .filter(_ != Pass)
          .pipe { tokens =>
            checkParameterList(cls.parameters, tokens) match {
              case Some(evaluationError) =>
                List(evaluationError)
              case None =>
                postEvaluation(ClassInstance(cls, tokens), block, acc, em)
            }
          }
      case Some(df: Definition) =>
        pc.expression
          .tokens
          .splitByComma()
          .map(evaluate(_, block, em))
          .filter(_ != Pass)
          .pipe { tokens =>
            checkParameterList(df.parameters, tokens) match {
              case Some(evaluationError) =>
                List(evaluationError)
              case None =>
                val cd = CallDefinition(df, tokens)
                val elem = parent match {
                  case Some(value) =>
                    EvaluatedDot(value, cd)
                  case None =>
                    cd
                }
                postEvaluation(elem, block, acc, em)
            }
          }
      case Some(other) =>
        List(EvaluationError(UnexpectedIdentifier(other.name)))
      case None =>
        pc +: acc
    }

  def checkParameterList(parameters: Parameters, tokens: List[EvaluatedToken]): Option[EvaluationError] =
    if (parameters.values.length == tokens.length)
      parameters.values.zip(tokens.map(Types.fromEvaluatedToken)).find {
        case (parameter, value) =>
          parameter.typ != value
      }.map {
        case (parameter, value) => EvaluationError(ParameterTypeError(parameter.typ, value))
      }
    else
      Some(EvaluationError(ParameterTypeMismatchError(parameters.values, tokens)))

  def classInstanceToBlock(cli: ClassInstance): ResultEither[Block] = {
    cli.cls.parameters.values.zip(cli.values).foldLeft(Result(Block.empty)) {
      case (acc, (param, value)) =>
        acc.flatMapValue(
          _.add(Assignment(param.identifier, Some(param.typ), Expression(List(value))), Nil)
        )
    }.finishedParsingTokens()
  }
}

object Expression {

  sealed trait EvaluationMode

  case object FullEvaluation extends EvaluationMode

  case object SimpleEvaluation extends EvaluationMode

  def parse(tokens: List[Token]): Result[Expression] =
    parse(tokens, Nil, Nil, None)

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
      case Indentation(_) :: _ =>
        finishExpression(tokens, outputStack, operatorStack)
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
