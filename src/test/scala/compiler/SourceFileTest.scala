package compiler

import compiler.Errors.InvalidToken

class SourceFileTest extends CompilerSpecs {

  val example =
    """
       x = 1
       y = 2

       def function(parameter: Int)
     """

  it should "be tokenized" in {
    val source = parseSuccess(example)
    assert(source.tokens.length === 19)
  }

  it should "be compiler" in {
    val block = compileSuccess(example)
    println(block)
    assert(block.assignments.length === 2)
    assert(block.definitions.length === 1)
  }

  it should "report token error" in {
    val errors = parseError("x = 1\n∂ = 2\nz = 3")
    assert(errors === List(InvalidToken(1, 0, "∂ = 2")))
  }

}
