package compiler

import compiler.Errors.{ExpectedIdentifier, UnexpectedToken, UnparsedTokens}
import compiler.Tokens.{Colon, Identifier, Integer, LeftParenthesis, RightParenthesis}

class DefinitionTest extends CompilerSpecs {

  it should "parse simple definition" in {
    val block = compileSuccess("def x()")
    assert(block === Block(
      Map(
        "x" -> Definition(Identifier("x"), Parameters(List(), None), None)
      ),
      List.empty
    ))
  }

  it should "parse simple definition with implementation" in {
    val block = compileSuccess("def x(): Int = 2")
    assert(block === Block(
      Map(
        "x" -> Definition(
        Identifier("x"),
        Parameters(List(), Some(Types.Integer)),
        Some(Block(Map.empty, List(Expression(List(Integer(2))))))
      )
      ),
      List.empty
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
