package compiler

import compiler.ParameterList.Parameter
import compiler.Tokens.Identifier
import compiler.Types.UnknownType

class ParameterListTest extends CompilerSpecs {

  it should "parse parameters separated by comma" in {
    val source = parseSuccess("(a: Float, b: MyClass)")
    assert(source.tokens.length === 10)
    val params = ParameterList.parseParameterList(source.tokens)
    assert(params.right.value === ParameterList(List(
      Parameter(Identifier("a"), Types.Floating),
      Parameter(Identifier("b"), UnknownType("MyClass")),
    )))
  }

  it should "parse parameters separated by newline" in {
    val source = parseSuccess(
      """
          (
            a: Int
            b: String
          )""")
    assert(source.tokens.length === 13)
    val params = ParameterList.parseParameterList(source.tokens)
    assert(params.right.value === ParameterList(List(
      Parameter(Identifier("a"), Types.Integer),
      Parameter(Identifier("b"), Types.String),
    )))
  }

  it should "parse empty parameter list" in {
    val source = parseSuccess(
      """()""")
    assert(source.tokens.length === 3)
    val params = ParameterList.parseParameterList(source.tokens)
    assert(params.right.value === ParameterList(List.empty))
  }

  it should "recognize missing left parenthesis" in {
    val source = parseSuccess("""(a: MyClass = 2""")
    assert(source.tokens.length === 7)
    val params = ParameterList.parseParameterList(source.tokens)
    assert(params.left.value === Errors.ExpectedRightParenthesis(None))
  }

  it should "recognize missing right parenthesis" in {
    val source = parseSuccess("""a: Float)""")
    assert(source.tokens.length === 5)
    val params = ParameterList.parseParameterList(source.tokens)
    assert(params.left.value === Errors.ExpectedParameterList(Some(Identifier("a"))))
  }

}
