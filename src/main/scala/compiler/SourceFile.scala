package compiler

import scala.io.BufferedSource

class SourceFile(val tokens: List[Token]) {
  override def toString: String = tokens.map(_.value).mkString(" ")

  def nonEmptyTokens: List[Token] = tokens.filter {
    case Indentation(_) => false
    case _ => true
  }

  def compile = {
    println(Block.parse(tokens, Block.empty))
  }
}

object SourceFile {
  def parse(source: BufferedSource): Either[List[CompilerError], SourceFile] = parse(source.getLines())

  def parse(string: String): Either[List[CompilerError], SourceFile] = parse(string.linesIterator)

  def parse(lines: Iterator[String]): Either[List[CompilerError], SourceFile] = {
    val parsedLines = lines
      .zipWithIndex
      .map { case (string, num) => Line(num, string) }
      .toList

    val (lefts, rights) = parsedLines.partitionMap(identity)

    if (lefts.isEmpty) {
      Right(new SourceFile(flattenLines(rights)))
    } else {
      Left(lefts)
    }
  }

  def flattenLines(rights: List[Line]): List[Token] = {
    rights.flatMap(_.tokens)
  }
}
