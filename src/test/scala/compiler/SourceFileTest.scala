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
    val errors = parseError("∂")
    assert(errors === List(InvalidToken(0, 0, "∂")))
  }

}
