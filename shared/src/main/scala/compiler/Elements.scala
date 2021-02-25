package compiler

import compiler.Elements.Parameters.Parameter
import compiler.Errors._
import compiler.Expression.{EvaluationMode, FullEvaluation}
import compiler.Tokens.{Colon, Comma, Comment, Def, EnumInstance, Equals, EvaluatedToken, EvaluationError, Identifier, Indentation, LeftParenthesis, RightParenthesis, Token, TokenListExtension, ValueToken}
import compiler.Types.{Type, UnknownType}

import scala.annotation.tailrec

object Elements {

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

  case class Block(elements: List[Element],
                   expression: Option[Expression],
                   parent: Option[Block] = None) {
    def add(element: Element, rest: List[Token]): Result[Block] =
      if (elements.exists(_.name.value == element.name.value))
        Result(Redefinition(element.name.value), rest)
      else
        Result(
          copy(
            elements = elements :+ element
          ),
          rest
        )

    def setExpression(expression: Expression): Block =
      copy(expression = Some(expression))

    def setParent(block: Block): Block =
      copy(parent = Some(block))

    lazy val identifierMap: Map[String, Element] =
      elements
        .flatMap {
          case enm: Enum =>
            enm.allElements
          case other =>
            List(other.name.value -> other)
        }
        .toMap


    def get(identifier: Identifier): Option[Element] =
      identifierMap.get(identifier.value)
        .orElse(parent.flatMap(_.get(identifier)))

    def evaluate(parent: Block = Block.empty, rest: List[Token] = Nil, em: EvaluationMode = FullEvaluation): Result[Block] =
      elements.foldLeft(Result(parent, rest)) {
        case (Result(Right(block), rest), curr) =>
          curr
            .evaluate(block, rest, em)
            .flatMap {
              case (newElement, rest) =>
                block.add(newElement, rest)
            }
        case (left@Result(_, _), _) =>
          left
      }
  }

  object Block {

    val empty: Block =
      Block(Nil, None)

    def parse(result: Result[Block], indentation: Option[Indentation], exprs: Boolean): Result[Block] =
      result.flatMap((block, rest) => parse(rest, block, indentation, exprs))

    @tailrec
    def parse(tokens: List[Token], block: Block, indentation: Option[Indentation], exprs: Boolean): Result[Block] =
      tokens match {
        case (current: Indentation) :: xs =>
          indentation match {
            case Some(Indentation(previousLength)) =>
              if (current.length >= previousLength)
                parse(xs, block, Some(current), exprs)
              else
                finishBlock(block, tokens)
            case None =>
              parse(xs, block, Some(current), exprs)
          }
        case Def :: xs =>
          Definition
            .parse(xs, top(indentation))
            .flatMap { (definition, rest) =>
              parse(block.add(definition, rest), indentation, exprs)
            }
        case Tokens.Class :: xs =>
          compiler.Elements.Class
            .parse(xs, top(indentation))
            .flatMap { (cls, rest) =>
              parse(block.add(cls, rest), indentation, exprs)
            }
        case Tokens.Enum :: xs =>
          compiler.Elements.Enum
            .parse(xs)
            .flatMap { (enm, rest) =>
              parse(block.add(enm, rest), indentation, exprs)
            }
        case (identifier: Identifier) :: Colon :: (typ: Identifier) :: Equals :: xs =>
          Assignment
            .parse(identifier, Some(typ), xs)
            .flatMap { (assignment, rest) =>
              parse(block.add(assignment, rest), indentation, exprs)
            }
        case (identifier: Identifier) :: Equals :: xs =>
          Assignment
            .parse(identifier, None, xs)
            .flatMap { (assignment, rest) =>
              parse(block.add(assignment, rest), indentation, exprs)
            }
        case (_: Comment) :: xs =>
          parse(xs, block, indentation, exprs)
        case Nil =>
          finishBlock(block, Nil)
        case others =>
          if (exprs)
            Expression
              .parse(others)
              .flatMap { (expr, rest) =>
                finishBlock(block.setExpression(expr), rest)
              }
          else
            finishBlock(block, others)
      }

    private def finishBlock(block: Block, others: List[Token]) =
      Result(block, others)

    private def top(indentations: Option[Indentation]) =
      indentations.getOrElse(Indentation(0))

  }

  case class Class(name: Identifier,
                   parameters: Parameters,
                   innerBlock: Block) extends Element {
    override def evaluate(block: Block, rest: List[Token], em: EvaluationMode): Result[Element] =
      block.get(name) match {
        case Some(_) =>
          Result(Redefinition(name.value), rest)
        case None =>
          Result(copy(innerBlock = innerBlock.setParent(block)), rest)
      }
  }

  object Class {

    def parse(tokens: List[Token], indentation: Indentation): Result[Class] =
      tokens match {
        case (identifier: Identifier) :: xs =>
          Parameters
            .parse(xs)
            .flatMap {
              case ((_, Some(typ)), rest) =>
                Result(UnexpectedReturnType(typ), rest)
              case ((parameters, None), rest) =>
                Block.parse(rest, Block.empty, Some(indentation.right), exprs = false).flatMap { (resultBlock, resultRest) =>
                  Result(
                    Class(identifier, parameters, resultBlock),
                    resultRest
                  )
                }
            }
        case other :: xs =>
          Result(ExpectedIdentifier(Some(other)), xs)
        case Nil =>
          Result(ExpectedIdentifier(None))
      }

  }

  case class Definition(name: Identifier,
                        parameters: Parameters,
                        returnType: Option[Type],
                        innerBlock: Option[Block]) extends Element {
    override def evaluate(block: Block, rest: List[Token], em: EvaluationMode): Result[Element] =
      block.get(name) match {
        case Some(_) =>
          Result(Redefinition(name.value), rest)
        case None =>
          Result(copy(innerBlock = innerBlock.map(_.setParent(block))), rest)
      }

    def call(values: List[EvaluatedToken]): Result[Block] =
      innerBlock match {
        case Some(definitionBlock) =>
          parameters
            .values
            .zip(values)
            .foldLeft(Result(Block.empty)) { case (currBlock, (parameter, value)) =>
              currBlock.flatMapValue(_.add(Assignment(parameter.identifier, None, Expression(List(value))), Nil))
            }
            .flatMapValue(parameterBlock => parameterBlock.evaluate(definitionBlock))
        case None =>
          Result(DefinitionWithoutBody(name.value))
      }
  }

  object Definition {

    def parse(tokens: List[Token], indentation: Indentation): Result[Definition] =
      tokens match {
        case (identifier: Identifier) :: xs =>
          Parameters
            .parse(xs)
            .flatMap {
              case ((parameters, returnType), Equals :: rest) =>
                Block.parse(rest, Block.empty, Some(indentation.right), exprs = true).flatMap { (resultBlock, resultRest) =>
                  Result(
                    Definition(identifier, parameters, returnType, Some(resultBlock)),
                    resultRest
                  )
                }
              case ((parameters, returnType), rest) =>
                Result(
                  Definition(identifier, parameters, returnType, None),
                  rest
                )
            }
        case other :: xs =>
          Result(UnexpectedToken(other), xs)
        case Nil =>
          Result(ExpectedIdentifier(None))
      }

  }

  sealed trait Element {
    def name: Identifier

    def evaluate(block: Block, rest: List[Token], em: EvaluationMode): Result[Element] =
      block.get(name) match {
        case Some(_) =>
          Result(Redefinition(name.value), rest)
        case None =>
          Result(this, rest)
      }
  }


  case class Enum(name: Identifier,
                  values: List[Identifier]) extends Element {
    def add(identifier: Identifier): Enum =
      copy(values = values :+ identifier)

    def get(field: Identifier): Option[Identifier] =
      values.find(_.value == field.value)

    def notUniqueIdentifiers(): List[String] =
      Identifier.notUniqueIdentifiers(values)

    def allElements: List[(String, Element)] =
      (name.value -> this) ::
        values
          .map(v => v.value -> Assignment(v, Some(UnknownType(name.value)), Expression(List(EnumInstance(this, v)))))
  }

  object Enum {

    def parse(tokens: List[Token]): Result[Enum] =
      tokens match {
        case (name: Identifier) :: xs =>
          parse(name, xs)
        case other =>
          Result(ExpectedIdentifier(other.headOption))
      }

    def parse(name: Identifier, tokens: List[Token]): Result[Enum] =
      tokens match {
        case LeftParenthesis :: xs =>
          val (left, right) = xs.spanMatchingRightParenthesis()
          right match {
            case RightParenthesis :: rest =>
              Result(
                parseEnums(left, Enum(name, Nil)),
                rest
              ).flatMap { (result, rest) =>
                result.notUniqueIdentifiers() match {
                  case Nil =>
                    Result(result, rest)
                  case errors =>
                    Result(NotUniqueEnumValues(result.name.value, errors), rest)
                }
              }
            case _ =>
              Result(ExpectedRightParenthesis(None))
          }
        case other =>
          Result(ExpectedLeftParenthesis(other.headOption))
      }

    @tailrec
    def parseEnums(tokens: List[Token], enum: Enum): Either[List[CompilerError], Enum] =
      tokens match {
        case Indentation(_) :: xs =>
          parseEnums(xs, enum)
        case (identifier: Identifier) :: Indentation(_) :: rest =>
          parseEnums(rest, enum.add(identifier))
        case (identifier: Identifier) :: Comma :: rest =>
          parseEnums(rest, enum.add(identifier))
        case (identifier: Identifier) :: Nil =>
          Right(enum.add(identifier))
        case Nil if enum.values.nonEmpty =>
          Right(enum)
        case Nil =>
          Left(List(EmptyEnum(enum.name.value)))
        case tokens =>
          Left(List(ExpectedIdentifier(tokens.headOption)))
      }
  }

  case class Parameters(values: List[Parameter]) {
    def addParameter(name: String, typ: String): Parameters =
      copy(values = values :+ Parameter(Identifier(name), Types.parse(typ)))

    def notUniqueIdentifiers(): List[String] =
      Identifier.notUniqueIdentifiers(values.map(_.identifier))

    def findParameter(identifier: Identifier): Option[(Parameter, Int)] =
      values.zipWithIndex.find(_._1.identifier == identifier)
  }

  object Parameters {

    case class Parameter(identifier: Identifier, typ: Type)

    val empty: Parameters = Parameters(Nil)

    @tailrec
    def parse(tokens: List[Token]): Result[(Parameters, Option[Type])] =
      tokens match {
        case Indentation(_) :: LeftParenthesis :: xs =>
          parse(LeftParenthesis :: xs)
        case Indentation(_) :: Colon :: xs =>
          parse(Colon :: xs)
        case LeftParenthesis :: xs =>
          val (left, right) = xs.spanMatchingRightParenthesis()
          right match {
            case RightParenthesis :: Colon :: Identifier(returnType) :: rest =>
              Result(
                parseParameters(left, empty).map((_, Some(Types.parse(returnType)))),
                rest
              )
            case RightParenthesis :: rest =>
              Result(parseParameters(left, empty).map((_, None)), rest)
            case _ =>
              Result(ExpectedRightParenthesis(None))
          }
        case Colon :: Identifier(returnType) :: rest =>
          Result((Parameters.empty, Some(Types.parse(returnType))), rest)
        case rest =>
          Result((Parameters.empty, None), rest)
      }

    @tailrec
    def parseParameters(tokens: List[Token], pl: Parameters): Either[List[CompilerError], Parameters] =
      tokens match {
        case Indentation(_) :: xs =>
          parseParameters(xs, pl)
        case Identifier(name) :: Colon :: Identifier(typ) :: Indentation(_) :: rest =>
          parseParameters(rest, pl.addParameter(name, typ))
        case Identifier(name) :: Colon :: Identifier(typ) :: Comma :: rest =>
          parseParameters(rest, pl.addParameter(name, typ))
        case Identifier(name) :: Colon :: Identifier(typ) :: Nil =>
          checkParameters(pl.addParameter(name, typ))
        case Nil =>
          checkParameters(pl)
        case Identifier(_) :: Colon :: tokens =>
          Left(List(ExpectedType(tokens.headOption)))
        case Identifier(_) :: tokens =>
          Left(List(ExpectedColon(tokens.headOption)))
        case tokens =>
          Left(List(ExpectedIdentifier(tokens.headOption)))
      }

    def checkParameters(parameters: Parameters): Either[List[CompilerError], Parameters] =
      parameters.notUniqueIdentifiers() match {
        case Nil => Right(parameters)
        case xs => Left(List(NotUniqueParameters(xs)))
      }

  }

}
