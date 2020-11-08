package compiler

import compiler.Errors.CompilerError
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.chaining.scalaUtilChainingOps

trait CompilerSpecs extends AnyFlatSpec with EitherValues {

  def parseSuccess(string: String): SourceFile = SourceFile.parse(string).right.value

  def parseError(string: String): List[CompilerError] = SourceFile.parse(string).left.value

  def parseExpression(string: String): Either[CompilerError, Expression] =
    parseSuccess(string)
      .pipe(sf => Expression.parse(sf.tokens, List.empty, List.empty, None))

  def parseExpressionSuccess(string: String): Expression =
    parseExpression(string).right.value

  def parseExpressionError(string: String): CompilerError =
    parseExpression(string).left.value

}