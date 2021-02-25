package compiler

import compiler.Elements._
import compiler.Errors.{Redefinition, TypeError}
import compiler.Parameters.Parameter
import compiler.Tokens.{Floating, Identifier, Integer, Multiply, Operator, StringLiteral}

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
          Some(Block(Nil, Some(Expression(List(Integer(6), Integer(2), Operator(Multiply))))))
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

  it should "check types in assignment" in {
    val evaluated = evaluateBlockError(compileSuccess(
      """
        x: Int = "must be type error"
      """))
    assert(evaluated === List(TypeError(Types.String, Types.Integer)))
  }

  it should "handle indentation of classes and defs" in {
    val evaluated = evaluateBlock(compileSuccess(
      """class A(x: Int)
        |  def t() = 3 + x
        |a = A(3)
        |""".stripMargin))
    assert(evaluated.elements.map(_.name.value) === List("A", "a"))
  }

}
