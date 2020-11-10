package compiler

import compiler.Errors.InvalidToken

class SourceFileTest extends CompilerSpecs {

  it should "be tokenized" in {
    val source = parseSuccess(exampleCode)
    assert(source.tokens.length === 34)
  }

  it should "report token error" in {
    val errors = parseError("x = 1\n∂ = 2\nz = 3")
    assert(errors === List(InvalidToken(1, 0, "∂ = 2")))
  }

  it should "report token error truncated" in {
    val errors = parseError("x = 1\n∂ = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9\nz = 3")
    assert(errors === List(InvalidToken(1, 0, "∂ = 1 + 2 + 3 + 4 + 5 + 6 +...")))
  }

}
