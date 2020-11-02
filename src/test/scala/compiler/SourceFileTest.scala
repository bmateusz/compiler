package compiler

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec

class SourceFileTest extends AnyFlatSpec with EitherValues {

  private def parseSuccess(string: String): SourceFile = SourceFile.parse(string).right.value

  private def parseError(string: String): List[CompilerError] = SourceFile.parse(string).left.value

  val example = """
    testIntegers = 23
    testStrings = "hello"

    def function(parameter: Int): String = ???
  """

  it should "be tokenized" in {
    val source = parseSuccess(example)
    assert(source.nonEmptyTokens.length === 17)
  }

  it should "report token error" in {
    val errors = parseError("∂")
    assert(errors === List(InvalidToken(0, 0, "∂")))
  }

}
