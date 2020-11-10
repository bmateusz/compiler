package compiler

import compiler.Errors.UnexpectedToken
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

  it should "report error for missing name" in {
    val block = compileError("def :()")
    assert(block === List(UnexpectedToken(Colon)))
  }

}
