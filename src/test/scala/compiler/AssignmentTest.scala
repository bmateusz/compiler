package compiler

import compiler.Errors.UnexpectedToken
import compiler.Tokens.{Add, Def, Identifier, Integer, Operator, Equals}

class AssignmentTest extends CompilerSpecs {

  it should "parse simple assignment" in {
    val block = compileSuccess("x = 3")
    assert(block === Block(
      List(
        Assignment(Identifier("x"), Expression(List(Integer(3)))),
      )
    ))
  }

  it should "parse assignment with line break" in {
    val block = compileSuccess(
      """
        x =
          3 + 3
      """)
    assert(block === Block(
      List(
        Assignment(Identifier("x"), Expression(List(Integer(3), Integer(3), Operator(Add)))),
      )
    ))
  }

  it should "report error if equals is not after identifier" in {
    val block = compileError(
      """
        x
          = 3 + 3
      """
    )
    assert(block === List(UnexpectedToken(Equals)))
  }

  it should "report error if expression is bad" in {
    val block = compileError("x = def 3")
    assert(block === List(UnexpectedToken(Def)))
  }

}
