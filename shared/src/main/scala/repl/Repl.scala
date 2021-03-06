package repl

import compiler.Elements.Element
import compiler.Errors.CompilerError
import compiler.Expression.FullEvaluation
import compiler.Tokens.EvaluatedToken
import compiler.{Block, Expression, SourceFile}

import scala.annotation.tailrec

trait Repl {

  def read(): String

  def println(elements: List[Element]): Unit

  def printlnEvaluation(tokens: EvaluatedToken): Unit

  def printlnError(errors: List[CompilerError]): Unit

  @tailrec
  final def repl(block: Block = Block.empty): Unit = {
    val str = read()
    if (str.nonEmpty) {
      SourceFile.parse(str) match {
        case Right(source) =>
          source.compile(block) match {
            case Right(newBlock) =>
              newBlock.expression match {
                case Some(expr: Expression) =>
                  printlnEvaluation(expr.evaluate(newBlock, FullEvaluation))
                  repl(block)
                case _ =>
                  newBlock.evaluate().finishedParsingTokens() match {
                    case Left(evaluationError) =>
                      printlnError(evaluationError)
                      repl(block)
                    case Right(evaluated) =>
                      println(evaluated.elements)
                      repl(evaluated)
                  }
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
