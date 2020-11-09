package compiler

import compiler.Errors.InvalidToken

class SourceFileTest extends CompilerSpecs {

  val example =
    """
       testIntegers = 23
       testStrings = "hello"

       def function(parameter: Int): String = ???
     """

  it should "be tokenized" in {
    val source = parseSuccess(example)
    assert(source.tokens.length === 23)
  }

  it should "report token error" in {
    val errors = parseError("x = 1\n∂ = 2\nz = 3")
    assert(errors === List(InvalidToken(1, 0, "∂ = 2")))
  }

}
