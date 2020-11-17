package compiler

import compiler.Errors.{ExpectedIdentifier, UnexpectedToken}
import compiler.Tokens.{Colon, Identifier, Integer}

class DefinitionTest extends CompilerSpecs {

  it should "parse simple definition" in {
    val block = compileSuccess("def x()")
    assert(block === Block(
      List(Definition(Identifier("x"), Parameters(List(), None), None))
    ))
  }

  it should "parse simple definition with implementation" in {
    val block = compileSuccess("def x(): Int = 2")
    assert(block === Block(
      List(Definition(
        Identifier("x"),
        Parameters(List(), Some(Types.Integer)),
        Some(Block(List(Expression(List(Integer(2))))))
      ))
    ))
  }

  it should "report error for bad name" in {
    val block = compileError("def :()")
    assert(block === List(UnexpectedToken(Colon)))
  }

  it should "report error for no name" in {
    val block = compileError("def ")
    assert(block === List(ExpectedIdentifier(None)))
  }

}