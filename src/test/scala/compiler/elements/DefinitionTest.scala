package compiler.elements

import compiler.Errors.{ExpectedIdentifier, UnexpectedToken, UnparsedTokens}
import compiler.Expression.{FullEvaluation, SimpleEvaluation}
import compiler.Tokens.{Add, CallDefinition, Colon, Comma, EvaluationError, Floating, Identifier, Integer, LeftParenthesis, Operator, ParameterTypeError, ParameterTypeMismatchError, ParsedCall, RightParenthesis, StringLiteral, Subtract}
import compiler.elements.Parameters.Parameter
import compiler.{CompilerSpecs, Expression, Types, elements}

class DefinitionTest extends CompilerSpecs {

  it should "parse simple definition" in {
    val block = compileSuccess("def x()")
    assert(block === Block(
      List(
        Definition(Identifier("x"), Parameters(List()), None, None)
      ),
      None
    ))
  }

  it should "parse simple definition with implementation" in {
    val block = compileSuccess("def x(): Int = 2")
    assert(block === Block(
      List(
        elements.Definition(
          Identifier("x"),
          Parameters(List()),
          Some(Types.Integer),
          Some(Block(List.empty, Some(Expression(List(Integer(2))))))
        )
      ),
      None
    ))
  }

  it should "report error for bad name" in {
    val block = compileError("def :()")
    assert(block === List(UnexpectedToken(Colon), UnparsedTokens(List(LeftParenthesis, RightParenthesis))))
  }

  it should "report error for no name" in {
    val block = compileError("def ")
    assert(block === List(ExpectedIdentifier(None)))
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

  it should "report not enough parameters in definition" in {
    val expr = parseExpressionSuccess("1 + x")
    assert(expr.tokens === List(Integer(1), Identifier("x"), Operator(Add)))
    val block = Block(
      List(Definition(Identifier("x"), Parameters(List(Parameter(Identifier("int"), Types.Integer))), None, Some(Block(List.empty, Some(Expression(List(Integer(100)))))))),
      None
    )
    assert(expr.evaluate(block, FullEvaluation) === EvaluationError(ParameterTypeMismatchError(List(Parameter(Identifier("int"), Types.Integer)), List())))
  }

  it should "report mismatched parameters in definition" in {
    val expr = parseExpressionSuccess("1 + x(1.2)")
    assert(expr.tokens === List(Integer(1), ParsedCall(Identifier("x"), Expression(List(Floating(1.2)))), Operator(Add)))
    val block = Block(
      List(Definition(Identifier("x"), Parameters(List(Parameter(Identifier("int"), Types.Integer))), None, Some(Block(List.empty, Some(Expression(List(Integer(100)))))))),
      None
    )
    assert(expr.evaluate(block, FullEvaluation) === EvaluationError(ParameterTypeError(Types.Integer, Types.Floating)))
  }

  it should "report too many parameters in definition" in {
    val expr = parseExpressionSuccess("1 + x(2, 3)")
    assert(expr.tokens === List(Integer(1), ParsedCall(Identifier("x"), Expression(List(Integer(2), Comma, Integer(3)))), Operator(Add)))
    val block = Block(
      List(Definition(Identifier("x"), Parameters(List(Parameter(Identifier("int"), Types.Integer))), None, Some(Block(List.empty, Some(Expression(List(Integer(100)))))))),
      None
    )
    assert(expr.evaluate(block, FullEvaluation) === EvaluationError(ParameterTypeMismatchError(List(Parameter(Identifier("int"), Types.Integer)), List(Integer(2), Integer(3)))))
  }

  it should "parse call of a definition" in {
    val evaluated = evaluateBlock(compileSuccess(
      """
        def x(n: Int) = 3 + n
      """))
    val expr = parseExpressionSuccess("x(4)")
    val definitino = Definition(Identifier("x"), Parameters(List(Parameter(Identifier("n"), Types.Integer))), None, Some(Block(List(), Some(Expression(List(Integer(3), Identifier("n"), Operator(Add)))))))
    assert(expr.evaluate(evaluated, SimpleEvaluation) === CallDefinition(definitino, List(Integer(4))))
    assert(expr.evaluate(evaluated, FullEvaluation) === Integer(7))
  }

  it should "full evaluate definition" in {
    val evaluated = evaluateBlock(compileSuccess(
      """
        def incByThree(n: Int) = 3 + n
      """))
    val expr = parseExpressionSuccess("incByThree(5) + 4")
    assert(expr.evaluate(evaluated, FullEvaluation) === Integer(12))
  }

  it should "full evaluate transitive definition" in {
    val evaluated = evaluateBlock(compileSuccess(
      """
        def a(str: String) = "a" + str
        def b(str: String) = a("b" + str)
        def c(str: String) = b("c" + str)
      """))
    val expr = parseExpressionSuccess("c(\"d\") + \"e\"")
    assert(expr.evaluate(evaluated, FullEvaluation) === StringLiteral("abcde"))
  }

}
