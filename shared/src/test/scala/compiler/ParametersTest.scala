package compiler

import compiler.Errors.{ExpectedColon, ExpectedIdentifier, ExpectedRightParenthesis, ExpectedType, NotUniqueParameters}
import compiler.Parameters.Parameter
import compiler.Tokens.{Colon, Equals, Identifier, Indentation, Integer, RightParenthesis}
import compiler.Types.UnknownType

class ParametersTest extends CompilerSpecs {

  it should "parse parameters separated by comma" in {
    val source = parseSuccess("(a: Float, b: MyClass)")
    assert(source.tokens.length === 10)
    val params = Parameters.parse(source.tokens)
    assert(params.right.value === (Parameters(List(
      Parameter(Identifier("a"), Types.Floating),
      Parameter(Identifier("b"), UnknownType("MyClass"))
    )), None))
  }

  it should "parse parameters separated by newline" in {
    val source = parseSuccess(
      """
          (
            a: Int
            b: String
          )""")
    assert(source.tokens.length === 12)
    val params = Parameters.parse(source.tokens)
    assert(params.right.value === (Parameters(List(
      Parameter(Identifier("a"), Types.Integer),
      Parameter(Identifier("b"), Types.String)
    )), None))
  }

  it should "parse empty parameter list" in {
    val source = parseSuccess(
      """()""")
    assert(source.tokens.length === 3)
    val params = Parameters.parse(source.tokens)
    assert(params.right.value === (Parameters(Nil), None))
  }

  it should "report error for not unique parameter" in {
    val source = parseSuccess("""(a: Int, b: Int, a: String)""")
    assert(source.tokens.length === 14)
    val params = Parameters.parse(source.tokens)
    assert(params.left.value === List(NotUniqueParameters(List("a"))))
  }

  it should "recognize missing right parenthesis" in {
    val source = parseSuccess("""(a: MyClass = 2""")
    assert(source.tokens.length === 7)
    val params = Parameters.parse(source.tokens)
    assert(params.left.value === List(ExpectedRightParenthesis(None)))
  }

  it should "pass on missing parameter list" in {
    val source = parseSuccess("""a: Float)""")
    assert(source.tokens.length === 5)
    val params = Parameters.parse(source.tokens)
    assert(params.right.value === (Parameters.empty, None))
    assert(params.rest === List(Indentation(0), Identifier("a"), Colon, Identifier("Float"), RightParenthesis))
  }

  it should "parse type hint only" in {
    val source = parseSuccess(""": Int = 2""")
    assert(source.tokens.length === 5)
    val params = Parameters.parse(source.tokens)
    assert(params.right.value === (Parameters.empty, Some(Types.Integer)))
    assert(params.rest === List(Equals, Integer(2)))
  }

  it should "recognize missing identifier" in {
    val source = parseSuccess("""(: Int)""")
    assert(source.tokens.length === 5)
    val params = Parameters.parse(source.tokens)
    assert(params.left.value === List(ExpectedIdentifier(Some(Colon))))
  }

  it should "recognize missing colon" in {
    val source = parseSuccess("""(a Int)""")
    assert(source.tokens.length === 5)
    val params = Parameters.parse(source.tokens)
    assert(params.left.value === List(ExpectedColon(Some(Identifier("Int")))))
  }

  it should "recognize missing type" in {
    val source = parseSuccess("""(a:)""")
    assert(source.tokens.length === 5)
    val params = Parameters.parse(source.tokens)
    assert(params.left.value === List(ExpectedType(None)))
  }

}
