package compiler.elements

import compiler.Errors.{ExpectedIdentifier, UnexpectedReturnType, UnparsedTokens}
import compiler.Tokens.{Add, ClassInstance, Colon, Comma, Dot, Identifier, Integer, LeftParenthesis, Operator, ParsedCall, RightParenthesis, StringLiteral, Subtract}
import compiler.Types.UnknownType
import compiler.elements.Parameters.Parameter
import compiler.{CompilerSpecs, Expression, Types, elements}

class ClassTest extends CompilerSpecs {

  it should "parse empty class" in {
    val block = compileSuccess("class A")
    assert(block === Block(
      List(
        Class(Identifier("A"), Parameters.empty, Block.empty)
      ),
      None
    ))
  }

  it should "parse simple class" in {
    val block = compileSuccess("class A(x: Int)")
    assert(block === Block(
      List(
        Class(Identifier("A"), Parameters(List(Parameter(Identifier("x"), Types.Integer))), Block.empty)
      ),
      None
    ))
  }

  it should "parse simple class with line break" in {
    val block = compileSuccess(
      """
      class A(
        value: String
      )
      """
    )
    assert(block === Block(
      List(
        Class(Identifier("A"), Parameters(List(Parameter(Identifier("value"), Types.String))), Block.empty)
      ),
      None
    ))
  }

  it should "report error when class has invalid identifier" in {
    val block = compileError("class +A(a: Float)")
    assert(block === List(
      ExpectedIdentifier(Some(Operator(Add))),
      UnparsedTokens(List(Identifier("A"), LeftParenthesis, Identifier("a"), Colon, Identifier("Float"), RightParenthesis))
    ))
  }

  it should "report error when class has return type" in {
    val block = compileError("class A(a: Float): MyClass")
    assert(block === List(UnexpectedReturnType(UnknownType("MyClass"))))
  }

  it should "report error for no name" in {
    val block = compileError("class ")
    assert(block === List(ExpectedIdentifier(None)))
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

  it should "parse definition inside a class" in {
    val evaluated = evaluateBlock(compileSuccess(
      """
        class A(n: Int)
          def x(m: Int): Int = m + n + 1
        a = A(20)
      """))
    val expr = parseExpressionSuccess("a.x(300)")
    assert(expr.evaluate(evaluated) === Integer(321))
  }

}
