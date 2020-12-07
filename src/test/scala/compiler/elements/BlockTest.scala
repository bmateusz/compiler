package compiler.elements

import compiler.Errors.Redefinition
import compiler.Tokens.{ClassInstance, Comma, Floating, Identifier, Integer, Multiply, Operator, ParsedCall, StringLiteral}
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
        Assignment(Identifier("x"), Expression(List(Integer(1)))),
        Assignment(Identifier("y"), Expression(List(Floating(3.14)))),
        Assignment(Identifier("z"), Expression(List(StringLiteral("hello")))),
        Class(Identifier("A"), Parameters(
          List(Parameter(Identifier("z"), Types.String)),
          None
        )),
        Definition(Identifier("function"), Parameters(
          List(Parameter(Identifier("parameter"), Types.Integer)),
          Some(Types.Integer)
        ), Some(Block(List.empty, Some(Expression(List(Integer(6), Integer(2), Operator(Multiply)))))))
      ),
      None
    ))
  }

  it should "not allow reassign to value" in {
    val block = compileError("""
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
        elements.Class(Identifier("A"), Parameters(List(Parameter(Identifier("n"), Types.Integer), Parameter(Identifier("s"), Types.String)), None)),
        elements.Assignment(Identifier("a"), Expression(List(ParsedCall(Identifier("A"), Expression(List(Integer(33), Comma, StringLiteral("str")))))))
      ),
      None
    ))
    val evaluated = evaluateBlock(block)
    assert(evaluated === Block(
      List(
        elements.Class(Identifier("A"), Parameters(List(Parameter(Identifier("n"), Types.Integer), Parameter(Identifier("s"), Types.String)), None)),
        elements.Assignment(Identifier("a"), Expression(List(ClassInstance(Identifier("A"),List(List(Integer(33)), List(StringLiteral("str")))))))),
      None
    ))
    val expr = parseExpressionSuccess("a.n")
    assert(expr.evaluate(evaluated) === List(Integer(33)))
  }

}
