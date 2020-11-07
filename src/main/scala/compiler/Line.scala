package compiler

import java.lang.Character.isWhitespace

import compiler.Errors.{CompilerError, InvalidToken}
import compiler.Tokens.SimpleTokens.simpleTokens
import compiler.Tokens._

import scala.annotation.tailrec
import scala.util.matching.UnanchoredRegex

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
        Left(InvalidToken.apply(number, string, rest))
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
      .orElse(findRegexToken(line))

  def findSimpleToken(line: String): Option[Token] = simpleTokens.find(token => line.startsWith(token.value))

  val stringRegex: UnanchoredRegex = """^"((\\.|[^\\"])*)"""".r.unanchored
  val floatRegex: UnanchoredRegex = "^(\\d+)(\\.\\d+)?".r.unanchored
  val literalRegex: UnanchoredRegex = "^([a-zA-Z]([0-9a-zA-Z_-]*[0-9a-zA-Z])?)".r.unanchored

  def findRegexToken(line: String): Option[Token] =
    line match {
      case stringRegex(str, _) => Some(StringLiteral(str))
      case floatRegex(a, null) => Some(Integer(a.toInt))
      case floatRegex(a, b) => Some(Floating(s"$a.$b".toDouble))
      case literalRegex(literal, _) => Some(Identifier(literal))
      case _ => None
    }

}
