package compiler

import arbitrary.ArbitraryExpression._
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.util.chaining.scalaUtilChainingOps

class ExpressionTest extends AnyFlatSpec with EitherValues with ScalaCheckPropertyChecks {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSize = 1, sizeRange = 10000, minSuccessful = 10000)

  private def parseExpression(string: String): Expression =
    SourceFile.parse(string).right.value.nonEmptyTokens
      .pipe(Expression.parse(_, List.empty, List.empty, None))

  it should "evaluate random expressions with parentheses" in {
    forAll { tree: Tree =>
      val prodResult = parseExpression(tree.toString).evaluate
      val testResult = tree.evaluate match {
        case Right(value) => List(Integer(value))
        case Left("Division by zero") => List(DivisionByZero)
        case Left(error) => fail(error)
      }

      assert(prodResult === testResult, s"Expected $testResult, got $prodResult for ${tree.toString}")
    }
  }

  it should "evaluate a simple addition" in {
    val expr = parseExpression("1 + 1")
    assert(expr.tokens === List(Integer(1), Integer(1), Operator(Add)))
    assert(expr.evaluate === List(Integer(2)))
  }

  it should "evaluate a negative number" in {
    val expr = parseExpression("2 + -5")
    assert(expr.tokens === List(Integer(2), Integer(5), Operator(Negate), Operator(Add)))
    assert(expr.evaluate === List(Integer(-3)))
  }

  it should "evaluate a simple addition and subtraction" in {
    val expr = parseExpression("1 - 2 + 3")
    assert(expr.tokens === List(Integer(1), Integer(2), Operator(Subtract), Integer(3), Operator(Add)))
    assert(expr.evaluate === List(Integer(2)))
  }

  it should "evaluate a parenthesis" in {
    val expr = parseExpression("3 + 4 * (2 - 1)")
    assert(expr.tokens === List(Integer(3), Integer(4), Integer(2), Integer(1), Operator(Subtract), Operator(Multiply), Operator(Add)))
    assert(expr.evaluate === List(Integer(7)))
  }

  it should "evaluate long expression" in {
    val expr = parseExpression("20 / 2 - (4+3) * 2")
    assert(expr.tokens === List(Integer(20), Integer(2), Operator(Divide), Integer(4), Integer(3), Operator(Add), Integer(2), Operator(Multiply), Operator(Subtract)))
    assert(expr.evaluate === List(Integer(-4)))
  }

  it should "evaluate from left to right" in {
    val expr = parseExpression("0 / 1 * 0")
    assert(expr.tokens === List(Integer(0), Integer(1), Operator(Divide), Integer(0), Operator(Multiply)))
    assert(expr.evaluate === List(Integer(0)))
  }

  it should "evaluate a division by zero" in {
    val expr = parseExpression("0 / (1 * 0)")
    assert(expr.tokens === List(Integer(0), Integer(1), Integer(0), Operator(Multiply), Operator(Divide)))
    assert(expr.evaluate === List(DivisionByZero))
  }

}
