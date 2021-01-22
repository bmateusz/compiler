package repl

import compiler.Errors.CompilerError
import compiler.Expression.{EvaluationMode, FullEvaluation}
import compiler.SourceFile
import compiler.Tokens.EvaluatedToken
import compiler.elements.{Block, Element}

object Evaluator {

  sealed trait EvaluationResult {
    def token: Option[EvaluatedToken]
  }

  case class EvaluationSuccess(elements: List[Element],
                               token: Option[EvaluatedToken],
                               source: SourceFile) extends EvaluationResult

  case class EvaluationFailure(errors: List[CompilerError],
                               source: Option[SourceFile]) extends EvaluationResult {
    val token: Option[EvaluatedToken] = None
  }

  def evaluate(string: String, evaluationMode: EvaluationMode = FullEvaluation): EvaluationResult =
    SourceFile.parse(string) match {
      case Right(source) =>
        source.compile(Block.empty) match {
          case Right(newBlock) =>
            newBlock.evaluate(em = evaluationMode).finishedParsingTokens() match {
              case Left(evaluationError) =>
                EvaluationFailure(evaluationError, Some(source))
              case Right(evaluatedBlock) =>
                EvaluationSuccess(
                  evaluatedBlock.elements,
                  newBlock.expression.map(_.evaluate(evaluatedBlock, evaluationMode)),
                  source
                )
            }
          case Left(compileError) =>
            EvaluationFailure(compileError, Some(source))
        }
      case Left(compileError) =>
        EvaluationFailure(compileError, None)
    }

}
