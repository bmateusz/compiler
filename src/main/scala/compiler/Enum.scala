package compiler

import compiler.Errors.{CompilerError, EmptyEnum, ExpectedEnums, ExpectedIdentifier, ExpectedParameters, ExpectedRightParenthesis, NotUniqueEnumValues}
import compiler.Tokens.{Comma, Identifier, Indentation, LeftParenthesis, RightParenthesis, Token}

import scala.annotation.tailrec

case class Enum(name: Identifier,
                values: List[Identifier]) extends Element {
  def add(identifier: Identifier): Enum =
    copy(values = values :+ identifier)

  def notUniqueIdentifiers(): List[String] =
    values
      .groupMapReduce(_.value)(_ => 1)(_ + _)
      .filter(_._2 > 1)
      .keys
      .toList
      .sorted
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
        val (left, right) = xs.span(_ != RightParenthesis)
        right match {
          case RightParenthesis :: rest =>
            Result.eitherSingleError(
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
        Result(ExpectedEnums(other.headOption))
    }

  @tailrec
  def parseEnums(tokens: List[Token], enum: Enum): Either[CompilerError, Enum] =
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
        Left(EmptyEnum(enum.name.value))
      case tokens =>
        Left(ExpectedIdentifier(tokens.headOption))
    }
}
