package repl

import compiler.Errors.CompilerError
import compiler.Expression.FullEvaluation
import compiler.SourceFile
import compiler.Tokens.EvaluatedToken
import compiler.elements.{Block, Element}

trait Evaluator {

  def setOutput(elements: List[Element], token: Option[EvaluatedToken]): Unit

  def setOutputError(errors: List[CompilerError]): Unit

  def evaluate(string: String): Unit =
    SourceFile.parse(string) match {
      case Right(source) =>
        source.compile(Block.empty) match {
          case Right(newBlock) =>
            newBlock.evaluate().finishedParsingTokens() match {
              case Left(evaluationError) =>
                setOutputError(evaluationError)
              case Right(evaluatedBlock) =>
                setOutput(
                  evaluatedBlock.sortedElements,
                  newBlock.expression.map(_.evaluate(evaluatedBlock, FullEvaluation))
                )
            }
          case Left(compileError) =>
            setOutputError(compileError)
        }
      case Left(compileError) =>
        setOutputError(compileError)
    }

}
