package compiler

import compiler.Errors.{CompilerError, ExpectedIdentifier, ExpectedParameters, ExpectedRightParenthesis}
import compiler.Tokens.{Comma, Identifier, Indentation, LeftParenthesis, RightParenthesis, Token}

import scala.annotation.tailrec

case class Enum(name: Identifier,
                values: List[Identifier]) extends Element

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
              parseEnums(left, List.empty).map(Enum(name, _)),
              rest
            )
          case _ =>
            Result(ExpectedRightParenthesis(None))
        }
      case other =>
        Result(ExpectedParameters(other.headOption))
    }

  @tailrec
  def parseEnums(tokens: List[Token], enums: List[Identifier]): Either[CompilerError, List[Identifier]] =
    tokens match {
      case Indentation(_) :: xs =>
        parseEnums(xs, enums)
      case (identifier: Identifier) :: Indentation(_) :: rest =>
        parseEnums(rest, enums :+ identifier)
      case (identifier: Identifier) :: Comma :: rest =>
        parseEnums(rest, enums :+ identifier)
      case (identifier: Identifier) :: Nil =>
        Right(enums :+ identifier)
      case Nil =>
        Right(enums)
      case tokens =>
        Left(ExpectedIdentifier(tokens.headOption))
    }
}
