package compiler.elements

import compiler.Errors.{Redefinition, TypeError}
import compiler.Expression.{FullEvaluation, SimpleEvaluation}
import compiler.Tokens.{Add, CallDefinition, ClassInstance, Comma, Floating, Identifier, Integer, Multiply, Operator, ParsedCall, StringLiteral}
import compiler.Types.UnknownType
import compiler.elements.Parameters.Parameter
import compiler.{CompilerSpecs, Expression, Types, elements}

class BlockTest extends CompilerSpecs {

  val exampleCode =
    """
       x = 1
       y = 3.14
       z = "hello"

       class A(z: String)

       def function(parameter: Int): Int =
         6 * 2
     """

  it should "be compiled" in {
    val block = compileSuccess(exampleCode)
    assert(block === Block(
      List(
        Assignment(Identifier("x"), None, Expression(List(Integer(1)))),
        Assignment(Identifier("y"), None, Expression(List(Floating(3.14)))),
        Assignment(Identifier("z"), None, Expression(List(StringLiteral("hello")))),
        Class(Identifier("A"), Parameters(
          List(Parameter(Identifier("z"), Types.String))
        ), Block.empty),
        Definition(
          Identifier("function"), Parameters(
            List(Parameter(Identifier("parameter"), Types.Integer))
          ),
          Some(Types.Integer),
          Some(Block(List.empty, Some(Expression(List(Integer(6), Integer(2), Operator(Multiply))))))
        )
      ),
      None
    ))
  }

  it should "not allow reassign to value" in {
    val block = compileError(
      """
      x = 2
      x = 5
    """)
    assert(block === List(Redefinition("x")))
  }

  it should "parse assignment of a class" in {
    val block = compileSuccess(
      """
        class A(n: Int, s: String)
        a = A(33, "str")
      """)
    assert(block === Block(
      List(
        elements.Class(Identifier("A"), Parameters(List(Parameter(Identifier("n"), Types.Integer), Parameter(Identifier("s"), Types.String))), Block.empty),
        elements.Assignment(Identifier("a"), None, Expression(List(ParsedCall(Identifier("A"), Expression(List(Integer(33), Comma, StringLiteral("str")))))))
      ),
      None
    ))
    val evaluated = evaluateBlock(block)
    val cls = elements.Class(Identifier("A"), Parameters(List(Parameter(Identifier("n"), Types.Integer), Parameter(Identifier("s"), Types.String))), Block.empty)
    assert(evaluated === Block(
      List(
        cls,
        elements.Assignment(Identifier("a"), Some(UnknownType("A")), Expression(List(ClassInstance(cls, List(Integer(33), StringLiteral("str"))))))),
      None
    ))
    val expr = parseExpressionSuccess("a.n")
    assert(expr.evaluate(evaluated) === Integer(33))
  }

  it should "parse assignment of a class of class" in {
    val evaluated = evaluateBlock(compileSuccess(
      """
        class A(n: Int)
        class B(a: A, m: Int)
        a = A(33)
        b = B(a, 2)
      """))
    val expr = parseExpressionSuccess("b.a.n * b.m")
    assert(expr.evaluate(evaluated) === Integer(66))
  }

  it should "parse inline assignment of a class of class" in {
    val evaluated = evaluateBlock(compileSuccess(
      """
        class A(n: Int)
        class B(a: A)
        class C(b: B)
        x = C(B(A(121)))
      """))
    val expr = parseExpressionSuccess("x.b.a.n")
    assert(expr.evaluate(evaluated) === Integer(121))
  }

  it should "parse assignment of a class aliased" in {
    val evaluated = evaluateBlock(compileSuccess(
      """
        class A(n: Int)
        a = A(2)
        b = a
      """))
    val expr = parseExpressionSuccess("b.n")
    assert(expr.evaluate(evaluated) === Integer(2))
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

  it should "check types in assignment" in {
    val evaluated = evaluateBlockError(compileSuccess(
      """
        x: Int = "must be type error"
      """))
    assert(evaluated === List(TypeError(Types.String, Types.Integer)))
  }

}
