package compiler.elements

import compiler.Errors.Redefinition
import compiler.Tokens.{Floating, Identifier, Integer, Multiply, Operator, StringLiteral}
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
      Map(
        "x" -> Assignment(Identifier("x"), Expression(List(Integer(1)))),
        "y" -> elements.Assignment(Identifier("y"), Expression(List(Floating(3.14)))),
        "z" -> elements.Assignment(Identifier("z"), Expression(List(StringLiteral("hello")))),
        "A" -> Class(Identifier("A"), Parameters(
          List(Parameter(Identifier("z"), Types.String)),
          None
        )),
        "function" -> Definition(Identifier("function"), Parameters(
          List(Parameter(Identifier("parameter"), Types.Integer)),
          Some(Types.Integer)
        ), Some(Block(Map.empty, Some(Expression(List(Integer(6), Integer(2), Operator(Multiply)))))))
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

}
