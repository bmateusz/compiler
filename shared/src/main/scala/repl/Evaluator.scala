package repl

import compiler.Errors.CompilerError
import compiler.Expression.{EvaluationMode, FullEvaluation}
import compiler.SourceFile
import compiler.Tokens.EvaluatedToken
import compiler.elements.{Block, Element}

trait Evaluator {

  def setOutput(elements: List[Element], token: Option[EvaluatedToken], source: SourceFile): Unit

  def setOutputError(errors: List[CompilerError], source: Option[SourceFile]): Unit

  def evaluate(string: String, evaluationMode: EvaluationMode = FullEvaluation): Unit =
    SourceFile.parse(string) match {
      case Right(source) =>
        source.compile(Block.empty) match {
          case Right(newBlock) =>
            newBlock.evaluate(em = evaluationMode).finishedParsingTokens() match {
              case Left(evaluationError) =>
                setOutputError(evaluationError, Some(source))
              case Right(evaluatedBlock) =>
                setOutput(
                  evaluatedBlock.elements,
                  newBlock.expression.map(_.evaluate(evaluatedBlock, evaluationMode)),
                  source
                )
            }
          case Left(compileError) =>
            setOutputError(compileError, Some(source))
        }
      case Left(compileError) =>
        setOutputError(compileError, None)
    }

}
