package compiler

import compiler.Errors.{UnexpectedToken, UnmatchedLeftParenthesis, UnmatchedRightParenthesis}
import compiler.Expression.{EvaluationMode, FullEvaluation, SimpleEvaluation}
import compiler.Result.ResultEither
import compiler.Tokens._
import compiler.elements.{Assignment, Block, Class, Definition, Enum, Parameters}

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

case class Expression(tokens: List[EvaluatedToken]) {

  private val debug = false

  private def trace(string: => String): Unit = if (debug) println(string) else ()

  def evaluate(block: Block = Block.empty, em: EvaluationMode = SimpleEvaluation): EvaluatedToken =
    evaluate(tokens, block, em)

  def evaluate(tokens: List[EvaluatedToken], block: Block, em: EvaluationMode): EvaluatedToken = tokens.foldLeft(Nil: List[EvaluatedToken]) {
    case ((ee: EvaluationError) :: _, _) =>
      List(ee)
    case ((child: EvaluatedToken) :: (parent: EvaluatedToken) :: xs, Operator(Dot)) =>
      dot(block, parent, child, xs, em)
    case (acc, identifier: Identifier) =>
      identifier :: acc
    case (acc, pc: ParsedCall) =>
      pc :: acc
    case (acc, cli: ClassInstance) =>
      cli :: acc
    case (acc, token: ValueToken) =>
      token :: acc
    case ((x: EvaluatedToken) :: xs, Operator(op@Negate)) =>
      EvaluatedUnaryOperator(op, x) :: xs
    case ((x: EvaluatedToken) :: (y: EvaluatedToken) :: ys, Operator(op)) =>
      EvaluatedOperator(x, y, op) :: ys
    case (acc, other) =>
      List(EvaluationError(UnexpectedEvaluation(acc, other)))
  }.pipe {
    case Nil => Pass
    case token :: Nil => em match {
      case FullEvaluation =>
        fullEvaluate(token, block)
      case SimpleEvaluation =>
        simpleEvaluate(token, block)
    }
    case more => EvaluationError(TooManyEvaluatedTokens(more))
  }

  def evaluateIdentifier(block: Block, identifier: Identifier, acc: List[EvaluatedToken], em: EvaluationMode): List[EvaluatedToken] =
    block.get(identifier) match {
      case Some(asg: Assignment) =>
        EvaluatedAssignment(asg) :: acc
      case Some(df: Definition) =>
        checkParameterList(df.parameters, Nil) match {
          case Some(evaluationError) =>
            List(evaluationError)
          case None =>
            postEvaluation(CallDefinition(df, Nil, None), block, acc, em)
        }
      case Some(cls: Class) =>
        ClassStatic(cls) :: acc
      case Some(enm: Enum) =>
        EnumStatic(enm) :: acc
      case Some(other) =>
        List(EvaluationError(UnexpectedIdentifier(other.name)))
      case None =>
        List(EvaluationError(UnexpectedIdentifierInBlock(identifier)))
    }

  def unaryOperator(x: ValueToken, op: Operators): EvaluatedToken =
    op match {
      case Tokens.Negate =>
        x match {
          case Integer(integer) => Integer(-integer)
          case Floating(double) => Floating(-double)
          case value: StringLiteral => EvaluationError(UnaryOperatorError(Negate, value))
        }
      case _ =>
        EvaluationError(UnaryOperatorError(op, x))
    }

  def operator(x: ValueToken, y: ValueToken, op: Operators): EvaluatedToken =
    (x, y, op) match {
      case (Integer(x), Integer(y), Add) => Integer(y + x)
      case (Integer(x), Integer(y), Subtract) => Integer(y - x)
      case (Integer(x), Integer(y), Multiply) => Integer(y * x)
      case (Integer(x), Integer(y), Divide) if x != 0 => Integer(y / x)
      case (Integer(_), Integer(_), Divide) => EvaluationError(DivisionByZero)
      case (Floating(x), Integer(y), Add) => Floating(y + x)
      case (Floating(x), Integer(y), Subtract) => Floating(y - x)
      case (Floating(x), Integer(y), Multiply) => Floating(y * x)
      case (Floating(x), Integer(y), Divide) => Floating(y / x)
      case (Integer(x), Floating(y), Add) => Floating(y + x)
      case (Integer(x), Floating(y), Subtract) => Floating(y - x)
      case (Integer(x), Floating(y), Multiply) => Floating(y * x)
      case (Integer(x), Floating(y), Divide) => Floating(y / x)
      case (Floating(x), Floating(y), Add) => Floating(y + x)
      case (Floating(x), Floating(y), Subtract) => Floating(y - x)
      case (Floating(x), Floating(y), Multiply) => Floating(y * x)
      case (Floating(x), Floating(y), Divide) => Floating(y / x)
      case (StringLiteral(x), StringLiteral(y), Add) => StringLiteral(y + x)
      case (a, b, op) => EvaluationError(OperatorError(op, b, a))
    }

  private def postEvaluation(elem: EvaluatedToken, block: Block, acc: List[EvaluatedToken], em: EvaluationMode): List[EvaluatedToken] =
    em match {
      case FullEvaluation =>
        fullEvaluate(elem, block) :: acc
      case SimpleEvaluation =>
        simpleEvaluate(elem, block) :: acc
    }

  private def simpleEvaluateIdentifier(block: Block, identifier: Identifier): EvaluatedToken =
    evaluateIdentifier(block, identifier, Nil, SimpleEvaluation) match {
      case Nil => Pass
      case head :: Nil => head
      case more => EvaluationError(TooManyEvaluatedTokens(more))
    }

  private def simpleEvaluate(evaluatedToken: EvaluatedToken, block: Block): EvaluatedToken = {
    trace(s"simple evaluate $evaluatedToken, $block")
    evaluatedToken match {
      case identifier: Identifier =>
        simpleEvaluateIdentifier(block, identifier)
      case pc: ParsedCall =>
        parsedCall(block, pc, None, Nil, SimpleEvaluation).head
      case euo@EvaluatedUnaryOperator(op, a) =>
        simpleEvaluate(a, block) match {
          case x: ValueToken =>
            unaryOperator(x, op)
          case err: EvaluationError =>
            err
          case _ =>
            euo
        }
      case eo@EvaluatedOperator(a, b, op) =>
        (simpleEvaluate(a, block), simpleEvaluate(b, block)) match {
          case (x: ValueToken, y: ValueToken) =>
            operator(x, y, op)
          case (err: EvaluationError, _) =>
            err
          case (_, err: EvaluationError) =>
            err
          case _ =>
            eo
        }
      case _ =>
        evaluatedToken
    }
  }

  private def fullEvaluate(evaluatedToken: EvaluatedToken, block: Block): EvaluatedToken = {
    trace(s"full evaluate $evaluatedToken, $block")
    evaluatedToken match {
    case identifier: Identifier =>
      block.get(identifier) match {
        case Some(asg: Assignment) =>
          asg.singleTokenOrIdentifier()
        case _ =>
          evaluateIdentifier(block, identifier, Nil, FullEvaluation).head
      }
    case cd@CallDefinition(definition, values, ec) =>
      definition.call(values).value match {
        case Left(_) =>
          cd
        case Right(definitionBlock) =>
          definitionBlock.expression match {
            case Some(expr) =>
              val contextBlock = definitionBlock
                .setParent(
                  ec
                    .flatMap(classInstanceToBlock(_).toOption)
                    .map(block.setParent)
                    .getOrElse(block)
                )
              val result = expr.evaluate(contextBlock, FullEvaluation)
              val typ = Types.fromEvaluatedToken(result)
              definition.returnType match {
                case Some(returnType) if returnType != typ =>
                  EvaluationError(DefinitionReturnTypeMismatch(definition.name, definition.returnType.get, typ))
                case _ =>
                  result
              }
            case None =>
              cd
          }
      }
    case ed@EvaluatedDot(cli, child) =>
      classInstanceToBlock(cli) match {
        case Left(_) =>
          ed
        case Right(cliBlock) =>
          fullEvaluate(child, cliBlock)
      }
    case EvaluatedUnaryOperator(op: Operators, a: EvaluatedToken) =>
      fullEvaluate(a, block) match {
        case x: ValueToken =>
          unaryOperator(x, op)
        case x: EvaluatedToken =>
          EvaluatedUnaryOperator(op, x)
      }
    case EvaluatedOperator(a: EvaluatedToken, b: EvaluatedToken, op: Operators) =>
      (fullEvaluate(a, block), fullEvaluate(b, block)) match {
        case (x: ValueToken, y: ValueToken) =>
          operator(x, y, op)
        case (err: EvaluationError, _) =>
          err
        case (_, err: EvaluationError) =>
          err
        case (x: EvaluatedToken, y: EvaluatedToken) =>
          EvaluatedOperator(x, y, op)
      }
    case pc: ParsedCall =>
      parsedCall(block, pc, None, Nil, FullEvaluation).head
    case other =>
      other
  }
  }

  private def dot(block: Block, parent: EvaluatedToken, child: EvaluatedToken, xs: List[EvaluatedToken], em: EvaluationMode): List[EvaluatedToken] =
    parent match {
      case parentIdentifier: Identifier =>
        simpleEvaluateIdentifier(block, parentIdentifier)
          .pipe(dot(block, _, child, xs, em))
      case ec: EvaluatedClass =>
        child match {
          case childIdentifier: Identifier =>
            dot(ec, childIdentifier, xs)
          case pc: ParsedCall =>
            parsedCall(block, pc, Some(ec), xs, em)
          case _ =>
            List(Pass)
        }
      case enumStatic: EnumStatic =>
        child match {
          case childIdentifier: Identifier =>
            enumStatic.enm.get(childIdentifier) match {
              case Some(enumValue: Identifier) =>
                EnumInstance(enumStatic.enm, enumValue) :: xs
              case None =>
                EvaluationError(UnexpectedEnumValueAfterDot(enumStatic.enm, childIdentifier)) :: xs
            }
          case _ =>
            List(Pass)
        }
      case EvaluatedAssignment(asg) =>
        child match {
          case childIdentifier: Identifier =>
            asg.expression.tokens match {
              case (v: ValueToken) :: Nil =>
                dot(block, v, childIdentifier, xs, em)
              case (cli: ClassInstance) :: Nil =>
                dot(cli, childIdentifier, xs)
              case (identifier: Identifier) :: Nil =>
                dot(block, identifier, childIdentifier, xs, em)
              case _ =>
                List(EvaluationError(UnexpectedIdentifierAfterDot(asg)))
            }
          case pc: ParsedCall =>
            asg.expression.tokens match {
              case (cli: ClassInstance) :: Nil =>
                parsedCall(cli.cls.innerBlock, pc, Some(cli), xs, em)
              case _ =>
                List(EvaluationError(UnexpectedIdentifier(pc.identifier)))
            }
          case _ =>
            List(Pass)
        }
      case pc: ParsedCall =>
        parsedCall(block, pc, None, xs, em) match {
          case oneEvaluatedParent :: Nil =>
            dot(block, oneEvaluatedParent, child, xs, em)
          case _ =>
            List(Pass)
        }
      case ee: EvaluationError =>
        List(ee)
      case Pass =>
        List(Pass)
      case _ =>
        List(Pass)
    }

  private def dot(ec: EvaluatedClass, field: Identifier, xs: List[EvaluatedToken]): List[EvaluatedToken] =
    ec.cls.parameters.findParameter(field) match {
      case Some((parameter, n)) =>
        ec match {
          case ClassInstance(_, values) =>
            values(n) :: xs
          case ClassStatic(cls) =>
            List(EvaluationError(NotStatic(cls.name, parameter.identifier)))
        }
      case None =>
        ec.cls.innerBlock.get(field) match {
          case Some(df: Definition) =>
            EvaluatedDot(ec, CallDefinition(df, Nil, Some(ec))) :: xs
          case Some(asg: Assignment) =>
            EvaluatedDot(ec, asg.singleTokenOrIdentifier()) :: xs
          case Some(other) =>
            List(EvaluationError(UnexpectedIdentifierAfterDot(other)))
          case None =>
            List(EvaluationError(UnexpectedIdentifierAfterDot(ec.cls)))
        }
    }

  def parsedCall(block: Block, pc: ParsedCall, parentClass: Option[EvaluatedClass], acc: List[EvaluatedToken], em: EvaluationMode): List[EvaluatedToken] =
    parentClass.flatMap(_.cls.innerBlock.get(pc.identifier)).orElse(block.get(pc.identifier)) match {
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
                val cd = CallDefinition(df, tokens, parentClass)
                val elem = parentClass match {
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
        pc :: acc
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

  def classInstanceToBlock(ec: EvaluatedClass): ResultEither[Block] = {
    ec match {
      case ClassInstance(cls, values) =>
        cls.parameters.values.zip(values).foldLeft(Result(ec.cls.innerBlock)) {
          case (acc, (param, value)) =>
            acc.flatMapValue(
              _.add(Assignment(param.identifier, Some(param.typ), Expression(List(value))), Nil)
            )
        }.finishedParsingTokens()
      case ClassStatic(_) =>
        Right(Block.empty)
    }
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
        parse(xs, outputStack ++ left, Operator(op) :: right, Some(token))
      case (token@LeftParenthesis) :: xs =>
        lastToken match {
          case Some(identifier: Identifier) =>
            xs.spanMatchingRightParenthesis() match {
              case (_, Nil) =>
                Result(UnmatchedRightParenthesis())
              case (left, right) =>
                parse(left) match {
                  case Result(Right(innerExpression: Expression), _: List[Token]) =>
                    parse(right, outputStack.init :+ ParsedCall(identifier, innerExpression), token :: operatorStack, Some(RightParenthesis))
                  case err =>
                    err
                }
            }
          case _ =>
            parse(xs, outputStack, token :: operatorStack, Some(token))
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
      case (_: Comment) :: xs =>
        parse(xs, outputStack, operatorStack, lastToken)
      case (multilineStringPart: MultilineStringPart) :: xs =>
        parse(xs, outputStack, multilineStringPart :: operatorStack, lastToken)
      case (multilineString: MultilineString) :: xs =>
        operatorStack.span {
          case _: MultilineStringPart => true
          case _ => false
        } match {
          case (left, right) =>
            val stringLiteral = StringLiteral((multilineString :: left).reverse.map(_.value).mkString("\n"))
            parse(xs, outputStack :+ stringLiteral, right, lastToken)
        }
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
