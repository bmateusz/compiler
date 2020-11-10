package compiler

import java.lang.Character.isWhitespace

import compiler.Errors.InvalidToken
import compiler.Tokens.{Indentation, Token}

import scala.annotation.tailrec

class Line(val number: Int,
           val tokens: List[Token]) {
  override def toString: String = tokens.mkString(" ")
}

object Line {

  def parse(number: Int, string: String): Result[Line] = {
    val (whitespaces, rest) = string.span(isWhitespace)
    tokenize(rest) match {
      case Right(tokens) =>
        Result(new Line(number, Indentation(whitespaces.length) +: tokens))
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
