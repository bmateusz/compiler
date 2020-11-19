package compiler

import compiler.Errors.CompilerError
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.chaining.scalaUtilChainingOps

trait CompilerSpecs extends AnyFlatSpec with EitherValues {

  implicit class ResultEitherOps[A](result: Result[A]) {
    def right: Either.RightProjection[List[CompilerError], A] = result.value.right

    def left: Either.LeftProjection[List[CompilerError], A] = result.value.left
  }

  def parseSuccess(string: String): SourceFile =
    SourceFile.parse(string).right.value

  def parseError(string: String): List[CompilerError] =
    SourceFile.parse(string).left.value

  def compileSuccess(string: String): Block =
    parseSuccess(string).compile().value.right.value

  def compileError(string: String): List[CompilerError] =
    parseSuccess(string).compile().value.left.value

  def parseExpression(string: String): Result[Expression] =
    parseSuccess(string)
      .pipe(sf => Expression.parse(sf.tokens, List.empty, List.empty, None))

  def parseExpressionSuccess(string: String): Expression =
    parseExpression(string).right.value

  def parseExpressionError(string: String): List[CompilerError] =
    parseExpression(string).left.value

}
