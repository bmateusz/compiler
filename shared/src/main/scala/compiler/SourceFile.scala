package compiler

import compiler.Errors.{CommentNotClosed, CompilerError}
import compiler.Result.ResultEither
import compiler.Tokens.Token
import compiler.elements.Block

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

  def parse(lines: Iterator[String]): Either[List[CompilerError], SourceFile] =
    lines
      .zipWithIndex
      .foldLeft((List.empty[Result[Line]], 0)) {
        case ((acc, 0), (string, num)) =>
          val parsedLine = Line.parse(string, num)
          (parsedLine :: acc, if (endsWithCommentStart(parsedLine)) 1 else 0)
        case ((acc, 1), (string, num)) =>
          val parsedLine = Line.parseInMultilineComment(string, num)
          (parsedLine :: acc, if (endMultilineCommentParsing(parsedLine)) 0 else 1)
      }
      .pipe {
        case (x :: xs, 1) if !endMultilineCommentParsing(x) =>
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

  private def flattenLines(rights: List[Line]): List[Token] =
    rights.flatMap(_.tokens)
}
