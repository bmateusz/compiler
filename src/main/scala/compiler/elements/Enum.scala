package compiler.elements

import compiler.Errors.{CompilerError, EmptyEnum, ExpectedIdentifier, ExpectedLeftParenthesis, ExpectedRightParenthesis, NotUniqueEnumValues}
import compiler.Result
import compiler.Tokens.{Comma, Identifier, Indentation, LeftParenthesis, RightParenthesis, Token, TokenListExtension}

import scala.annotation.tailrec

case class Enum(name: Identifier,
                values: List[Identifier]) extends Element {
  def add(identifier: Identifier): Enum =
    copy(values = values :+ identifier)

  def notUniqueIdentifiers(): List[String] =
    Identifier.notUniqueIdentifiers(values)
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
              parseEnums(left, Enum(name, List.empty)),
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
