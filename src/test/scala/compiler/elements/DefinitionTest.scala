package compiler.elements

import compiler.Errors.{ExpectedIdentifier, UnexpectedToken, UnparsedTokens}
import compiler.Tokens.{Colon, Identifier, Integer, LeftParenthesis, RightParenthesis}
import compiler.{CompilerSpecs, Expression, Types, elements}

class DefinitionTest extends CompilerSpecs {

  it should "parse simple definition" in {
    val block = compileSuccess("def x()")
    assert(block === Block(
      List(
        Definition(Identifier("x"), Parameters(List(), None), None)
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
          Parameters(List(), Some(Types.Integer)),
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

}