package compiler

import compiler.Errors.{UnmatchedLeftParenthesis, UnmatchedRightParenthesis}
import compiler.Expression.FullEvaluation
import compiler.Tokens._
import compiler.elements.Parameters.Parameter
import compiler.elements.{Assignment, Block, Class, Definition, Parameters}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ExpressionTest extends CompilerSpecs with ScalaCheckPropertyChecks {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSize = 1, sizeRange = 1000, minSuccessful = 1000)

  it should "evaluate random expressions with parentheses" in {
    import arbitrary.ArbitraryExpression._
    forAll { tree: Tree =>
      val treeAsString = tree.toString
      val prodResult = parseExpressionSuccess(treeAsString).evaluate() match {
        case Floating(value) if value.isNaN => Identifier("NaN")
        case x => x
      }
      val testResult = tree.evaluate match {
        case IntValue(value) => Integer(value)
        case FloatValue(value) if value.isNaN => Identifier("NaN")
        case FloatValue(value) => Floating(value)
        case Error("Division by zero") => EvaluationError(DivisionByZero)
        case Error(error) => fail(error)
      }

      assert(prodResult === testResult, s"Expected $testResult, got $prodResult for $treeAsString")
    }
  }

  it should "evaluate random string expressions with parentheses" in {
    import arbitrary.ArbitraryString._
    forAll { tree: Tree =>
      val treeAsString = tree.toString
      val prodResult = parseExpressionSuccess(treeAsString).evaluate()
      val testResult = tree.evaluate match {
        case StringValue(value) => StringLiteral(value)
      }

      assert(prodResult === testResult, s"Expected $testResult, got $prodResult for $treeAsString")
    }
  }

  it should "evaluate unary minus" in {
    val expr = parseExpressionSuccess("-1")
    assert(expr.tokens === List(Integer(1), Operator(Negate)))
    assert(expr.evaluate() === Integer(-1))
  }

  it should "evaluate a simple addition" in {
    val expr = parseExpressionSuccess("1 + 1")
    assert(expr.tokens === List(Integer(1), Integer(1), Operator(Add)))
    assert(expr.evaluate() === Integer(2))
  }

  it should "evaluate a negative number" in {
    val expr = parseExpressionSuccess("2 + -5")
    assert(expr.tokens === List(Integer(2), Integer(5), Operator(Negate), Operator(Add)))
    assert(expr.evaluate() === Integer(-3))
  }

  it should "evaluate a simple addition and subtraction" in {
    val expr = parseExpressionSuccess("1 - 2 + 3")
    assert(expr.tokens === List(Integer(1), Integer(2), Operator(Subtract), Integer(3), Operator(Add)))
    assert(expr.evaluate() === Integer(2))
  }

  it should "evaluate a parenthesis" in {
    val expr = parseExpressionSuccess("3 + 4 * (2 - 1)")
    assert(expr.tokens === List(Integer(3), Integer(4), Integer(2), Integer(1), Operator(Subtract), Operator(Multiply), Operator(Add)))
    assert(expr.evaluate() === Integer(7))
  }

  it should "evaluate long expression" in {
    val expr = parseExpressionSuccess("20 / 2 - (4+3) * 2")
    assert(expr.tokens === List(Integer(20), Integer(2), Operator(Divide), Integer(4), Integer(3), Operator(Add), Integer(2), Operator(Multiply), Operator(Subtract)))
    assert(expr.evaluate() === Integer(-4))
  }

  it should "evaluate from left to right" in {
    val expr = parseExpressionSuccess("0 / 1 * 0")
    assert(expr.tokens === List(Integer(0), Integer(1), Operator(Divide), Integer(0), Operator(Multiply)))
    assert(expr.evaluate() === Integer(0))
  }

  it should "evaluate exponential float" in {
    val expr = parseExpressionSuccess("0.1e-5 + 2")
    assert(expr.tokens === List(Floating(0.1e-5), Integer(2), Operator(Add)))
    assert(expr.evaluate() === Floating(2.000001))
  }

  it should "evaluate a division by zero" in {
    val expr = parseExpressionSuccess("0 / (1 * 0)")
    assert(expr.tokens === List(Integer(0), Integer(1), Integer(0), Operator(Multiply), Operator(Divide)))
    assert(expr.evaluate() === EvaluationError(DivisionByZero))
  }

  it should "evaluate an unary operator error" in {
    val expr = parseExpressionSuccess("- \"a\"")
    assert(expr.tokens === List(StringLiteral("a"), Operator(Negate)))
    assert(expr.evaluate() === EvaluationError(UnaryOperatorError(Negate, StringLiteral("a"))))
  }

  it should "evaluate an operator error" in {
    val expr = parseExpressionSuccess("2 + \"a\"")
    assert(expr.tokens === List(Integer(2), StringLiteral("a"), Operator(Add)))
    assert(expr.evaluate() === EvaluationError(OperatorError(Add, Integer(2), StringLiteral("a"))))
  }

  it should "report error for unmatched left parenthesis" in {
    val error = parseExpressionError("(3 * (2 + 1)")
    assert(error === List(UnmatchedLeftParenthesis()))
  }

  it should "report error for unmatched right parenthesis" in {
    val error = parseExpressionError("3 * (2 + 1))")
    assert(error === List(UnmatchedRightParenthesis()))
  }

  it should "report error for invalid identifier" in {
    val expr = parseExpressionSuccess("1 + x - 3")
    assert(expr.tokens === List(Integer(1), Identifier("x"), Operator(Add), Integer(3), Operator(Subtract)))
    val block = Block(
      List.empty,
      None
    )
    val evaluationError = EvaluationError(UnexpectedEvaluation(List(Identifier("x"), Integer(1)), Operator(Add)))
    assert(expr.evaluate(block) === evaluationError)
    assert(expr.evaluate(block, FullEvaluation) === evaluationError)
  }

  it should "evaluate identifier" in {
    val expr = parseExpressionSuccess("1 + x - 3")
    assert(expr.tokens === List(Integer(1), Identifier("x"), Operator(Add), Integer(3), Operator(Subtract)))
    val block = Block(
      List(Assignment(Identifier("x"), None, Expression(List(Integer(100))))),
      None
    )
    assert(expr.evaluate(block) === Integer(98))
  }

  it should "evaluate definition" in {
    val expr = parseExpressionSuccess("1 + x - 3")
    assert(expr.tokens === List(Integer(1), Identifier("x"), Operator(Add), Integer(3), Operator(Subtract)))
    val block = Block(
      List(Definition(Identifier("x"), Parameters.empty, None, Some(Block(List.empty, Some(Expression(List(Integer(100)))))))),
      None
    )
    assert(expr.evaluate(block, FullEvaluation) === Integer(98))
  }

  it should "evaluate class field" in {
    val expr = parseExpressionSuccess("1 + x.a - 3")
    assert(expr.tokens === List(Integer(1), Identifier("x"), Identifier("a"), Operator(Dot), Operator(Add), Integer(3), Operator(Subtract)))
    val cls = Class(Identifier("A"), Parameters(List(Parameter(Identifier("a"), Types.Integer))), Block.empty)
    val block = Block(
      List(
        cls,
        Assignment(Identifier("x"), None, Expression(List(ClassInstance(cls, List(Integer(1))))))
      ),
      None
    )
    assert(expr.evaluate(block) === Integer(-1))
  }

}
