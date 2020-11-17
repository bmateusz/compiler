package compiler

import arbitrary.ArbitraryExpression._
import compiler.Errors.{UnmatchedLeftParenthesis, UnmatchedRightParenthesis}
import compiler.Tokens._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ExpressionTest extends CompilerSpecs with ScalaCheckPropertyChecks {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSize = 1, sizeRange = 1000, minSuccessful = 1000)

  it should "evaluate random integer expressions with parentheses" in {
    forAll { tree: IntTree =>
      val prodResult = parseExpressionSuccess(tree.toString).evaluate()
      val testResult = tree.evaluate match {
        case Right(value) => List(Integer(value))
        case Left("Division by zero") => List(DivisionByZero)
        case Left(error) => fail(error)
      }

      assert(prodResult === testResult, s"Expected $testResult, got $prodResult for ${tree.toString}")
    }
  }

  it should "evaluate random float expressions with parentheses" in {
    forAll { tree: FloatTree =>
      val prodResult = parseExpressionSuccess(tree.toString).evaluate()
      val testResult = tree.evaluate match {
        case Right(value) => List(Floating(value))
        case Left("Division by zero") => List(DivisionByZero)
        case Left(error) => fail(error)
      }

      assert(prodResult === testResult, s"Expected $testResult, got $prodResult for ${tree.toString}")
    }
  }

  it should "evaluate unary minus" in {
    val expr = parseExpressionSuccess("-1")
    assert(expr.tokens === List(Integer(1), Operator(Negate)))
    assert(expr.evaluate() === List(Integer(-1)))
  }

  it should "evaluate a simple addition" in {
    val expr = parseExpressionSuccess("1 + 1")
    assert(expr.tokens === List(Integer(1), Integer(1), Operator(Add)))
    assert(expr.evaluate() === List(Integer(2)))
  }

  it should "evaluate a negative number" in {
    val expr = parseExpressionSuccess("2 + -5")
    assert(expr.tokens === List(Integer(2), Integer(5), Operator(Negate), Operator(Add)))
    assert(expr.evaluate() === List(Integer(-3)))
  }

  it should "evaluate a simple addition and subtraction" in {
    val expr = parseExpressionSuccess("1 - 2 + 3")
    assert(expr.tokens === List(Integer(1), Integer(2), Operator(Subtract), Integer(3), Operator(Add)))
    assert(expr.evaluate() === List(Integer(2)))
  }

  it should "evaluate a parenthesis" in {
    val expr = parseExpressionSuccess("3 + 4 * (2 - 1)")
    assert(expr.tokens === List(Integer(3), Integer(4), Integer(2), Integer(1), Operator(Subtract), Operator(Multiply), Operator(Add)))
    assert(expr.evaluate() === List(Integer(7)))
  }

  it should "evaluate long expression" in {
    val expr = parseExpressionSuccess("20 / 2 - (4+3) * 2")
    assert(expr.tokens === List(Integer(20), Integer(2), Operator(Divide), Integer(4), Integer(3), Operator(Add), Integer(2), Operator(Multiply), Operator(Subtract)))
    assert(expr.evaluate() === List(Integer(-4)))
  }

  it should "evaluate from left to right" in {
    val expr = parseExpressionSuccess("0 / 1 * 0")
    assert(expr.tokens === List(Integer(0), Integer(1), Operator(Divide), Integer(0), Operator(Multiply)))
    assert(expr.evaluate() === List(Integer(0)))
  }

  it should "evaluate exponential float" in {
    val expr = parseExpressionSuccess("0.1e-5 + 2")
    assert(expr.tokens === List(Floating(0.1e-5), Integer(2), Operator(Add)))
    assert(expr.evaluate() === List(Floating(2.000001)))
  }

  it should "evaluate a division by zero" in {
    val expr = parseExpressionSuccess("0 / (1 * 0)")
    assert(expr.tokens === List(Integer(0), Integer(1), Integer(0), Operator(Multiply), Operator(Divide)))
    assert(expr.evaluate() === List(DivisionByZero))
  }

  it should "report error for unmatched left parenthesis" in {
    val error = parseExpressionError("(3 * (2 + 1)")
    assert(error === List(UnmatchedLeftParenthesis()))
  }

  it should "report error for unmatched right parenthesis" in {
    val error = parseExpressionError("3 * (2 + 1))")
    assert(error === List(UnmatchedRightParenthesis()))
  }

  it should "evaluate identifier" in {
    val expr = parseExpressionSuccess("1 + x - 3")
    assert(expr.tokens === List(Integer(1), Identifier("x"), Operator(Add), Integer(3), Operator(Subtract)))
    val block = Block(List(Assignment(Identifier("x"), Expression(List(Integer(100))))))
    assert(expr.evaluate(block) === List(Integer(98)))
  }

  it should "evaluate class field" in {
    val expr = parseExpressionSuccess("1 + x.a - 3")
    assert(expr.tokens === List(Integer(1), Identifier("x"), Identifier("a"), Operator(Dot), Operator(Add), Integer(3), Operator(Subtract)))
    // assert(expr.evaluate() === List(Integer(-1)))
  }

}
