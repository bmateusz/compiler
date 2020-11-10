package compiler

import compiler.Errors.InvalidToken

class SourceFileTest extends CompilerSpecs {

  it should "be tokenized" in {
    val source = parseSuccess(exampleCode)
    assert(source.tokens.length === 30)
  }

  it should "report token error" in {
    val errors = parseError("x = 1\n∂ = 2\nz = 3")
    assert(errors === List(InvalidToken(1, 0, "∂ = 2")))
  }

}
