package compiler.elements

import compiler.Elements._
import compiler.Errors.{ExpectedIdentifier, UnexpectedReturnType, UnparsedTokens}
import compiler.Expression.FullEvaluation
import compiler.Parameters.Parameter
import compiler.Tokens.{Add, CallDefinition, ClassInstance, ClassStatic, Colon, Comma, Dot, EvaluatedDot, Identifier, Integer, LeftParenthesis, Operator, ParsedCall, RightParenthesis, StringLiteral, Subtract}
import compiler.Types.UnknownType
import compiler.{Block, CompilerSpecs, Expression, Parameters, Types}

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
    assert(expr.evaluate(block, FullEvaluation) === Integer(-1))
  }

  it should "parse assignment of a class" in {
    val block = compileSuccess(
      """
        class A(n: Int, s: String)
        a = A(33, "str")
      """)
    assert(block === Block(
      List(
        Class(Identifier("A"), Parameters(List(Parameter(Identifier("n"), Types.Integer), Parameter(Identifier("s"), Types.String))), Block.empty),
        Assignment(Identifier("a"), None, Expression(List(ParsedCall(Identifier("A"), Expression(List(Integer(33), Comma, StringLiteral("str")))))))
      ),
      None
    ))
    val evaluated = evaluateBlock(block)
    val cls = Class(Identifier("A"), Parameters(List(Parameter(Identifier("n"), Types.Integer), Parameter(Identifier("s"), Types.String))), Block(List(), None, Some(Block(List(), None, None))))
    assert(evaluated === Block(
      List(
        cls,
        Assignment(Identifier("a"), Some(UnknownType("A")), Expression(List(ClassInstance(cls, List(Integer(33), StringLiteral("str"))))))),
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
    assert(expr.evaluate(evaluated, FullEvaluation) === Integer(66))
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
    val definition = Definition(
      Identifier("x"),
      Parameters(List(Parameter(Identifier("m"), Types.Integer))),
      Some(Types.Integer),
      Some(Block(List(), Some(Expression(List(Identifier("m"), Identifier("n"), Operator(Add), Integer(1), Operator(Add)))), None))
    )
    val cls = Class(
      Identifier("A"),
      Parameters(List(Parameter(Identifier("n"), Types.Integer))),
      Block(List(definition), None, Some(Block(List(), None, None)))
    )
    val classInstance = ClassInstance(cls, List(Integer(20)))

    assert(expr.evaluate(evaluated) === EvaluatedDot(classInstance, CallDefinition(definition, List(Integer(300)), Some(classInstance))))

    assert(expr.evaluate(evaluated, FullEvaluation) === Integer(321))
  }

  it should "parse parameterless definition inside a class" in {
    val evaluated = evaluateBlock(compileSuccess(
      """
        class A(n: Int)
          def x: Int = n + 1
        a = A(20)
      """))
    val expr = parseExpressionSuccess("a.x")
    val definition = Definition(
      Identifier("x"),
      Parameters.empty,
      Some(Types.Integer),
      Some(Block(List(), Some(Expression(List(Identifier("n"), Integer(1), Operator(Add)))), None))
    )
    val cls = Class(
      Identifier("A"),
      Parameters(List(Parameter(Identifier("n"), Types.Integer))),
      Block(List(definition), None, Some(Block(List(), None, None)))
    )
    val classInstance = ClassInstance(cls, List(Integer(20)))

    assert(expr.evaluate(evaluated) === EvaluatedDot(classInstance, CallDefinition(definition, Nil, Some(classInstance))))

    assert(expr.evaluate(evaluated, FullEvaluation) === Integer(21))
  }

  it should "parse static definition inside a class" in {
    val evaluated = evaluateBlock(compileSuccess(
      """
        class A
          def x: Int = 323
      """))
    val expr = parseExpressionSuccess("A.x")
    val definition = Definition(
      Identifier("x"),
      Parameters.empty,
      Some(Types.Integer),
      Some(Block(List(), Some(Expression(List(Integer(323)))), None))
    )
    val cls = Class(
      Identifier("A"),
      Parameters.empty,
      Block(List(definition), None, Some(Block(List(), None, None)))
    )

    assert(expr.evaluate(evaluated) === EvaluatedDot(ClassStatic(cls), CallDefinition(definition, Nil, Some(ClassStatic(cls)))))

    assert(expr.evaluate(evaluated, FullEvaluation) === Integer(323))
  }

  it should "parse static assignment inside a class" in {
    val evaluated = evaluateBlock(compileSuccess(
      """
        class A
          x: Int = 324
      """))
    val expr = parseExpressionSuccess("A.x")
    val assignment = Assignment(Identifier("x"), Some(Types.Integer), Expression(List(Integer(324))))
    val cls = Class(
      Identifier("A"),
      Parameters.empty,
      Block(List(assignment), None, Some(Block(List(), None, None)))
    )

    assert(expr.evaluate(evaluated) === EvaluatedDot(ClassStatic(cls), Integer(324)))

    assert(expr.evaluate(evaluated, FullEvaluation) === Integer(324))
  }

  it should "parse multiple assignments inside a class" in {
    val evaluated = evaluateBlock(compileSuccess(
      """
        class A
          x = 1
          y: Int = 30
          z: Int = 500
      """))
    val expr = parseExpressionSuccess("A.x + A.y + A.z")
    assert(expr.evaluate(evaluated, FullEvaluation) === Integer(531))
  }

  it should "parse multiple expressions inside a class" in {
    val evaluated = evaluateBlock(compileSuccess(
      """
        class A
          def x = 1
          y: Int = 30
          def z(n: Int): Int = 500 + n
      """))
    val expr = parseExpressionSuccess("A.x + A.y + A.z(7000)")
    assert(expr.evaluate(evaluated, FullEvaluation) === Integer(7531))
  }

  it should "parse multiple classes without parameters" in {
    val evaluated = compileSuccess(
      """
        class A
        class B
      """)
    assert(evaluated === Block(List(
      Class(Identifier("A"), Parameters.empty, Block.empty),
      Class(Identifier("B"), Parameters.empty, Block.empty),
    ), None, None))
  }
}
