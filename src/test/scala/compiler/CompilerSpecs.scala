package compiler

import compiler.Errors.CompilerError
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.chaining.scalaUtilChainingOps

trait CompilerSpecs extends AnyFlatSpec with EitherValues {

  def parseExpression(string: String): Expression =
    SourceFile.parse(string).right.value.nonEmptyTokens
      .pipe(Expression.parse(_, List.empty, List.empty, None))

  def parseSuccess(string: String): SourceFile = SourceFile.parse(string).right.value

  def parseError(string: String): List[CompilerError] = SourceFile.parse(string).left.value

}
