package repl

import compiler.Errors.CompilerError
import compiler.Tokens.EvaluatedToken
import compiler.{Block, Expression, SourceFile}

import scala.annotation.tailrec

trait Repl {

  def read(): String

  def println(block: Block): Unit

  def printlnEvaluation(tokens: List[EvaluatedToken]): Unit

  def printlnError(errors: List[CompilerError]): Unit

  @tailrec
  final def repl(): Unit = {
    val str = read()
    if (str.nonEmpty) {
      SourceFile.parse(str) match {
        case Right(source) =>
          source.compile.value match {
            case Right(success) =>
              println(success)
            case Left(compileError) =>
              Expression.parse(source.tokens, List.empty, List.empty, None).value match {
                case Right(success) =>
                  printlnEvaluation(success.evaluate)
                case Left(_) =>
                  printlnError(compileError)
              }
          }
        case Left(compileError) =>
          printlnError(compileError)
      }
      repl()
    }
  }

}
