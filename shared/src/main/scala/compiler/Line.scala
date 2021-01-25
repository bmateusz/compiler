package compiler

import compiler.Errors.InvalidToken
import compiler.Tokens.{CommentEnd, CommentInline, CommentLine, CommentStart, CommentStartLiteral, Indentation, MultilineString, MultilineStringPart, SingleComment, SingleCommentLiteral, Token, TripleQuote, WideToken}

import java.lang.Character.isWhitespace
import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

class Line(val tokens: List[Token],
           val number: Int) {
  override def toString: String = tokens.mkString(" ")

  def endsWithCommentStart: Boolean = tokens.lastOption.exists {
    case _: CommentStart => true
    case _ => false
  }

  def startsWithCommentEnd: Boolean = tokens.headOption.exists {
    case _: CommentEnd => true
    case _ => false
  }

  def endsWithMultilineString: Boolean = tokens.lastOption.exists {
    case _: MultilineStringPart => true
    case _ => false
  }

  def startsWithMultilineString: Boolean = tokens.headOption.exists {
    case _: MultilineStringPart => true
    case _ => false
  }
}

object Line {

  val empty = new Line(Nil, 0)

  def parse(string: String, number: Int): Result[Line] =
    string
      .span(isWhitespace)
      .pipe { case (whitespaces, rest) =>
        tokenize(rest) match {
          case Right(tokens) if tokens.isEmpty =>
            Result(new Line(Nil, number))
          case Right(tokens) =>
            Result(new Line(Indentation(whitespaces.length) :: tokens, number))
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
        case Some(SingleCommentLiteral) =>
          Right(tokens :+ SingleComment(line.drop(2)))
        case Some(TripleQuote) =>
          findTripleQuoteEnd(line.drop(3)) match {
            case None =>
              Right(tokens :+ MultilineStringPart(line))
            case Some(n) =>
              val (begin, end) = line.splitAt(n)
              tokenize(end.dropWhile(isWhitespace), tokens :+ MultilineString(begin.drop(3).dropRight(3)))
          }
        case Some(CommentStartLiteral) =>
          findCommentEnd(line) match {
            case None =>
              Right(tokens :+ CommentStart(line))
            case Some(n) =>
              val (begin, end) = line.splitAt(n)
              tokenize(end.dropWhile(isWhitespace), tokens :+ CommentInline(begin.drop(2).dropRight(2)))
          }
        case Some(wideToken: WideToken) =>
          tokenize(line.drop(wideToken.length).dropWhile(isWhitespace), tokens :+ wideToken.wrapped)
        case Some(token) =>
          tokenize(line.drop(token.length).dropWhile(isWhitespace), tokens :+ token)
        case None =>
          Left(line)
      }
    }

  def parseInMultilineComment(line: String, number: Int): Result[Line] =
    findCommentEnd(line) match {
      case None =>
        Result(new Line(CommentLine(line) :: Nil, number))
      case Some(n) =>
        val (begin, end) = line.splitAt(n)
        parse(end, number)
          .map(result => new Line(CommentEnd(begin) :: result.tokens, result.number))
    }

  def findCommentEnd(string: String): Option[Int] =
    string.indexOf("*/") match {
      case -1 => None
      case n => Some(n + 2)
    }

  def parseInMultilineString(line: String, number: Int): Result[Line] =
    findTripleQuoteEnd(line) match {
      case None =>
        Result(new Line(MultilineStringPart(line) :: Nil, number))
      case Some(n) =>
        val (begin, end) = line.splitAt(n)
        parse(end, number)
          .map(result => new Line(MultilineString(begin) :: result.tokens, result.number))
    }

  def findTripleQuoteEnd(string: String): Option[Int] =
    string.indexOf(TripleQuote.value) match {
      case -1 => None
      case n => Some(n + TripleQuote.value.length)
    }

}
