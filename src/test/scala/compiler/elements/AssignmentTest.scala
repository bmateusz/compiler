package compiler.elements

import compiler.Errors.{UnexpectedToken, UnparsedTokens}
import compiler.Tokens.{Add, Comma, Def, Equals, Identifier, Indentation, Integer, Operator, StringLiteral}
import compiler.elements.Parameters.Parameter
import compiler.{CompilerSpecs, Expression, Types, elements}

class AssignmentTest extends CompilerSpecs {

  it should "parse simple assignment" in {
    val block = compileSuccess("x = 3")
    assert(block === Block(
      List(
        Assignment(Identifier("x"), None, Expression(List(Integer(3))))
      ),
      None
    ))
  }

  it should "parse simple assignment with type hint" in {
    val block = compileSuccess("x: Int = 3")
    assert(block === Block(
      List(
        Assignment(Identifier("x"), Some(Types.Integer), Expression(List(Integer(3))))
      ),
      None
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
        elements.Assignment(Identifier("x"), None, Expression(List(Integer(3), Integer(3), Operator(Add))))
      ),
      None
    ))
  }

  it should "report error if equals is not after identifier" in {
    val block = compileError(
      """
        x
          = 3 + 3
      """
    )
    assert(block === List(UnparsedTokens(List(Indentation(10), Equals, Integer(3), Operator(Add), Integer(3)))))
  }

  it should "report error if expression is bad" in {
    val block = compileError("x = def 3")
    assert(block === List(UnexpectedToken(Def), UnparsedTokens(List(Integer(3)))))
  }

}
