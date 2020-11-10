package compiler

import compiler.Errors.{ExpectedIdentifier, UnexpectedToken}
import compiler.Tokens.{Colon, Identifier}

class DefinitionTest extends CompilerSpecs {

  it should "parse simple definition" in {
    val block = compileSuccess("def x()")
    assert(block === Block(
      List.empty,
      List(Definition(Identifier("x"), Parameters(List(), None))),
      List.empty
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
