package compiler

import compiler.Errors.InvalidToken
import compiler.Tokens.{Indentation, Token, WideToken}

import java.lang.Character.isWhitespace
import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

class Line(val tokens: List[Token],
           val number: Int) {
  override def toString: String = tokens.mkString(" ")
}

object Line {

  def parse(string: String, number: Int): Result[Line] =
    string
      .span(isWhitespace)
      .pipe { case (whitespaces, rest) =>
        tokenize(rest) match {
          case Right(tokens) if tokens.isEmpty =>
            Result(new Line(Nil, number))
          case Right(tokens) =>
            Result(new Line(Indentation(whitespaces.length) +: tokens, number))
          case Left(rest) =>
            Result(InvalidToken(number, string, rest))
        }
      }

  @tailrec
  def tokenize(line: String, tokens: List[Token] = Nil): Either[String, List[Token]] =
    if (line.isEmpty) {
      Right(tokens)
    } else {
      Tokens.parse(line) match {
        case Some(wideToken: WideToken) =>
          tokenize(line.drop(wideToken.length).dropWhile(isWhitespace), tokens :+ wideToken.wrapped)
        case Some(identifier) =>
          tokenize(line.drop(identifier.length).dropWhile(isWhitespace), tokens :+ identifier)
        case None =>
          Left(line)
      }
    }

}
