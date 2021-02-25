package compiler

import compiler.Elements.Block
import compiler.Errors.CompilerError
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.chaining.scalaUtilChainingOps

trait CompilerSpecs extends AnyFlatSpec with EitherValues {

  implicit class ResultEitherOps[A](result: Result[A]) {
    def right: Either.RightProjection[List[CompilerError], A] = result.value.right

    def left: Either.LeftProjection[List[CompilerError], A] = result.value.left
  }

  def parseLineSuccess(string: String): Line =
    Line.parse(string, 0).right.value

  def parseSuccess(string: String): SourceFile =
    SourceFile.parse(string).right.value

  def parseError(string: String): List[CompilerError] =
    SourceFile.parse(string).left.value

  def compileSuccess(string: String): Block =
    parseSuccess(string).compile().right.value

  def compileError(string: String): List[CompilerError] =
    parseSuccess(string).compile().left.value

  def parseExpression(string: String): Result[Expression] =
    parseSuccess(string)
      .pipe(sf => Expression.parse(sf.tokens))

  def parseExpressionSuccess(string: String): Expression =
    parseExpression(string).right.value

  def parseExpressionError(string: String): List[CompilerError] =
    parseExpression(string).left.value

  def evaluateBlock(block: Block): Block =
    block.evaluate().value.right.value

  def evaluateBlockError(block: Block): List[CompilerError] =
    block.evaluate().value.left.value

}
