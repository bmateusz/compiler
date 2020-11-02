package compiler

import java.lang.Character.isWhitespace

import compiler.SimpleTokens.allSimpleTokens

import scala.annotation.tailrec
import scala.util.matching.{Regex, UnanchoredRegex}

class Line(val number: Int,
           val tokens: List[Token]) {
  override def toString: String = tokens.mkString(" ")
}

object Line {
  def apply(number: Int, string: String): Either[CompilerError, Line] = {
    val (whitespaces, rest) = string.span(isWhitespace)
    tokenize(rest) match {
      case Right(tokens) =>
        Right(new Line(number, Indentation(whitespaces.length) +: tokens))
      case Left(rest) =>
        Left(new InvalidToken(number, string.length - rest.length, rest.take(50)))
    }
  }

  @tailrec
  def tokenize(string: String, tokens: List[Token] = List.empty): Either[String, List[Token]] = {
    val line = string.dropWhile(isWhitespace)
    if (line.isEmpty) {
      Right(tokens)
    } else {
      parse(line) match {
        case Some(identifier) =>
          tokenize(line.drop(identifier.length), tokens :+ identifier)
        case None =>
          Left(string)
      }
    }
  }

  def parse(line: String): Option[Token] =
    findSimpleToken(line)
      .orElse(findStringLiteral(line))
      .orElse(findNumericLiteral(line))
      .orElse(findLiteral(line))

  def findSimpleToken(line: String): Option[Token] = allSimpleTokens.find(token => line.startsWith(token.value))

  val stringRegex: UnanchoredRegex = "^\"(.+)\"".r.unanchored

  def findStringLiteral(line: String): Option[Token] =
    line match {
      case stringRegex(str) => Some(StringLiteral(str))
      case _ => None
    }

  val floatRegex: UnanchoredRegex = "^(\\d+)(\\.\\d+)?".r.unanchored

  def findNumericLiteral(line: String): Option[Token] =
    line match {
      case floatRegex(a, null) => Some(Integer(a.toInt))
      case floatRegex(a, b) => Some(Floating(s"$a.$b".toDouble))
      case _ => None
    }

  def findLiteral(line: String): Option[Token] =
    line.takeWhile(char => char.isLetterOrDigit) match {
      case "" => None
      case matched => Some(Identifier(matched))
    }

}
