package compiler

import compiler.Errors.CompilerError
import compiler.Result.ResultEither
import compiler.Tokens.Token
import compiler.elements.Block

import scala.io.BufferedSource

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
      .map { case (string, num) => Line.parse(string, num) }
      .toList
      .mapEither(
        lefts => lefts.flatten,
        rights => new SourceFile(flattenLines(rights))
      )

  def flattenLines(rights: List[Line]): List[Token] =
    rights.flatMap(_.tokens)
}
