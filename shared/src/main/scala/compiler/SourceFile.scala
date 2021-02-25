package compiler

import compiler.Elements.Block
import compiler.Errors.{CommentNotClosed, CompilerError}
import compiler.Result.ResultEither
import compiler.Tokens.Token

import scala.io.BufferedSource
import scala.util.chaining.scalaUtilChainingOps

class SourceFile(val tokens: List[Token]) {
  override def toString: String =
    tokens.map(_.value).mkString(" ")

  def compile(block: Block = Block.empty): ResultEither[Block] =
    Block
      .parse(tokens, block, None, exprs = true)
      .finishedParsingTokens()
}

object SourceFile {
  def parse(source: BufferedSource): Either[List[CompilerError], SourceFile] =
    parse(source.getLines())

  def parse(string: String): Either[List[CompilerError], SourceFile] =
    parse(string.linesIterator)

  sealed trait LineParseMode

  case object Normal extends LineParseMode

  case object MultilineComment extends LineParseMode

  case object MultilineString extends LineParseMode

  def parse(lines: Iterator[String]): Either[List[CompilerError], SourceFile] =
    lines
      .zipWithIndex
      .foldLeft((List.empty[Result[Line]], Normal: LineParseMode)) {
        case ((acc, Normal), (string, num)) =>
          val parsedLine = Line.parse(string, num)
          (
            parsedLine :: acc,
            if (endsWithCommentStart(parsedLine))
              MultilineComment
            else if (endsWithMultilineStringPart(parsedLine))
              MultilineString
            else
              Normal
          )
        case ((acc, MultilineComment), (string, num)) =>
          val parsedLine = Line.parseInMultilineComment(string, num)
          (parsedLine :: acc, if (endMultilineCommentParsing(parsedLine)) Normal else MultilineComment)
        case ((acc, MultilineString), (string, num)) =>
          val parsedLine = Line.parseInMultilineString(string, num)
          (parsedLine :: acc, if (endMultilineStringParsing(parsedLine)) Normal else MultilineString)
      }
      .pipe {
        case (x :: xs, MultilineComment) if !endMultilineCommentParsing(x) =>
          Result[Line](CommentNotClosed()) :: xs
        case (result, _) =>
          result
      }
      .reverse
      .mapEither(
        lefts => lefts.flatten,
        rights => new SourceFile(flattenLines(rights))
      )

  private def endsWithCommentStart(parsedLine: Result[Line]) =
    parsedLine.map(_.endsWithCommentStart).getOrElse(false)

  private def endMultilineCommentParsing(parsedLine: Result[Line]) =
    parsedLine.map(p => p.startsWithCommentEnd && !p.endsWithCommentStart).getOrElse(true)

  private def endsWithMultilineStringPart(parsedLine: Result[Line]) =
    parsedLine.map(_.endsWithMultilineString).getOrElse(false)

  private def endMultilineStringParsing(parsedLine: Result[Line]) =
    parsedLine.map(p => p.startsWithMultilineString && !p.endsWithMultilineString).getOrElse(true)

  private def flattenLines(rights: List[Line]): List[Token] =
    rights.flatMap(_.tokens)
}
