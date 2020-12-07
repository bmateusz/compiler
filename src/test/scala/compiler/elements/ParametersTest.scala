package compiler.elements

import compiler.Errors.{ExpectedColon, ExpectedIdentifier, ExpectedLeftParenthesis, ExpectedRightParenthesis, ExpectedType, NotUniqueParameters}
import compiler.Tokens.{Colon, Identifier}
import compiler.Types.UnknownType
import compiler.elements.Parameters.Parameter
import compiler.{CompilerSpecs, Types}

class ParametersTest extends CompilerSpecs {

  it should "parse parameters separated by comma" in {
    val source = parseSuccess("(a: Float, b: MyClass)")
    assert(source.tokens.length === 10)
    val params = Parameters.parse(source.tokens)
    assert(params.right.value === Parameters(List(
      Parameter(Identifier("a"), Types.Floating),
      Parameter(Identifier("b"), UnknownType("MyClass"))
    ), None))
  }

  it should "parse parameters separated by newline" in {
    val source = parseSuccess(
      """
          (
            a: Int
            b: String
          )""")
    assert(source.tokens.length === 13)
    val params = Parameters.parse(source.tokens)
    assert(params.right.value === Parameters(List(
      Parameter(Identifier("a"), Types.Integer),
      Parameter(Identifier("b"), Types.String)
    ), None))
  }

  it should "parse empty parameter list" in {
    val source = parseSuccess(
      """()""")
    assert(source.tokens.length === 3)
    val params = Parameters.parse(source.tokens)
    assert(params.right.value === Parameters(List.empty, None))
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

  it should "recognize missing left parenthesis" in {
    val source = parseSuccess("""a: Float)""")
    assert(source.tokens.length === 5)
    val params = Parameters.parse(source.tokens)
    assert(params.left.value === List(ExpectedLeftParenthesis(Some(Identifier("a")))))
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
