package compiler

import compiler.Errors.Redefinition
import compiler.Parameters.Parameter
import compiler.Tokens.{Floating, Identifier, Integer, SimpleTokens, StringLiteral}

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
        "y" -> Assignment(Identifier("y"), Expression(List(Floating(3.14)))),
        "z" -> Assignment(Identifier("z"), Expression(List(StringLiteral("hello")))),
        "A" -> Class(Identifier("A"), Parameters(
          List(Parameter(Identifier("z"), Types.String)),
          None
        )),
        "function" -> Definition(Identifier("function"), Parameters(
          List(Parameter(Identifier("parameter"), Types.Integer)),
          Some(Types.Integer)
        ), Some(Block(Map.empty, List(Expression(List(Integer(6), Integer(2), SimpleTokens.`*`))))))
      ),
      List.empty
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
