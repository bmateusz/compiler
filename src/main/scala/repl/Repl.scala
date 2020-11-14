package repl

import compiler.Errors.CompilerError
import compiler.Tokens.EvaluatedToken
import compiler.{Element, Expression, SourceFile}

import scala.annotation.tailrec

trait Repl {

  def read(): String

  def println(elements: List[Element]): Unit

  def printlnEvaluation(tokens: List[EvaluatedToken]): Unit

  def printlnError(errors: List[CompilerError]): Unit

  @tailrec
  final def repl(): Unit = {
    val str = read()
    if (str.nonEmpty) {
      SourceFile.parse(str) match {
        case Right(source) =>
          source.compile.value match {
            case Right(block) =>
              block.elements match {
                case (expr: Expression) :: Nil =>
                  printlnEvaluation(expr.evaluate)
                case other =>
                  println(other)
              }
            case Left(compileError) =>
              printlnError(compileError)
          }
        case Left(compileError) =>
          printlnError(compileError)
      }
      repl()
    }
  }

}
