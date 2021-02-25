package compiler

import compiler.Errors._
import compiler.Expression.EvaluationMode
import compiler.Tokens.{Comma, EnumInstance, Equals, EvaluatedToken, EvaluationError, Identifier, Indentation, LeftParenthesis, RightParenthesis, Token, TokenListExtension, ValueToken}
import compiler.Types.{Type, UnknownType}

import scala.annotation.tailrec

object Elements {

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

}
