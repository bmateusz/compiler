package compiler

import compiler.ParameterList.Parameter
import compiler.Tokens.Identifier
import compiler.Types.UnknownType

class ParameterListTest extends CompilerSpecs {

  it should "parse parameters separated by comma" in {
    val source = parseSuccess("(a: Int, b: String)")
    assert(source.nonEmptyTokens.length === 9)
    val params = ParameterList.parseParameterList(source.tokens)
    assert(params.right.value === ParameterList(List(
      Parameter(Identifier("a"), UnknownType("Int")),
      Parameter(Identifier("b"), UnknownType("String")),
    )))
  }

  it should "parse parameters separated by newline" in {
    val source = parseSuccess(
      """
          (
            a: Int
            b: String
          )""")
    assert(source.nonEmptyTokens.length === 8)
    val params = ParameterList.parseParameterList(source.tokens)
    assert(params.right.value === ParameterList(List(
      Parameter(Identifier("a"), UnknownType("Int")),
      Parameter(Identifier("b"), UnknownType("String")),
    )))
  }

}
