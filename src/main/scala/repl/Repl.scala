package repl

import compiler.Errors.CompilerError
import compiler.Tokens.EvaluatedToken
import compiler.elements.{Block, Element}
import compiler.{Expression, SourceFile}

import scala.annotation.tailrec

trait Repl {

  def read(): String

  def println(elements: List[Element]): Unit

  def printlnEvaluation(tokens: List[EvaluatedToken]): Unit

  def printlnError(errors: List[CompilerError]): Unit

  @tailrec
  final def repl(block: Block = Block.empty): Unit = {
    val str = read()
    if (str.nonEmpty) {
      SourceFile.parse(str) match {
        case Right(source) =>
          source.compile(block).value match {
            case Right(newBlock) =>
              newBlock.expressions.lastOption match {
                case Some(expr: Expression) =>
                  printlnEvaluation(expr.evaluate(newBlock))
                  repl(newBlock)
                case _ =>
                  println(newBlock.sortedElements)
                  repl(newBlock)
              }
            case Left(compileError) =>
              printlnError(compileError)
              repl(block)
          }
        case Left(compileError) =>
          printlnError(compileError)
          repl(block)
      }
    }
  }

}
