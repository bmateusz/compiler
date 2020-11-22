package compiler

import java.lang.Character.isWhitespace

import compiler.Errors.InvalidToken
import compiler.Tokens.{Indentation, Token}

import scala.annotation.tailrec

class Line(val tokens: List[Token],
           val number: Int) {
  override def toString: String = tokens.mkString(" ")
}

object Line {

  def parse(string: String, number: Int): Result[Line] = {
    val (whitespaces, rest) = string.span(isWhitespace)
    tokenize(rest) match {
      case Right(tokens) =>
        Result(new Line(Indentation(whitespaces.length) +: tokens, number))
      case Left(rest) =>
        Result(InvalidToken.apply(number, string, rest))
    }
  }

  @tailrec
  def tokenize(string: String, tokens: List[Token] = List.empty): Either[String, List[Token]] = {
    val line = string.dropWhile(isWhitespace)
    if (line.isEmpty) {
      Right(tokens)
    } else {
      Tokens.parse(line) match {
        case Some(identifier) =>
          tokenize(line.drop(identifier.length), tokens :+ identifier)
        case None =>
          Left(string)
      }
    }
  }

}
