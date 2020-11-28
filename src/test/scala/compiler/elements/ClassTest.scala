package compiler.elements

import compiler.Errors.{ExpectedIdentifier, UnexpectedReturnType, UnparsedTokens}
import compiler.Tokens.{Add, Colon, Identifier, LeftParenthesis, Operator, RightParenthesis}
import compiler.Types.UnknownType
import compiler.elements.Parameters.Parameter
import compiler.{CompilerSpecs, Types, elements}

class ClassTest extends CompilerSpecs {

  it should "parse simple class" in {
    val block = compileSuccess("class A(x: Int)")
    assert(block === Block(
      Map(
        "A" -> Class(Identifier("A"), Parameters(List(Parameter(Identifier("x"), Types.Integer)), None))
      ),
      List.empty
    ))
  }

  it should "parse simple class with line break" in {
    val block = compileSuccess(
      """
      class A(
        value: String
      )
      """
    )
    assert(block === Block(
      Map(
        "A" -> elements.Class(Identifier("A"), Parameters(List(Parameter(Identifier("value"), Types.String)), None))
      ),
      List.empty
    ))
  }

  it should "report error when class has invalid identifier" in {
    val block = compileError("class +A(a: Float)")
    assert(block === List(
      ExpectedIdentifier(Some(Operator(Add))),
      UnparsedTokens(List(Identifier("A"), LeftParenthesis, Identifier("a"), Colon, Identifier("Float"), RightParenthesis))
    ))
  }

  it should "report error when class has return type" in {
    val block = compileError("class A(a: Float): MyClass")
    assert(block === List(UnexpectedReturnType(UnknownType("MyClass"))))
  }

  it should "report error for no name" in {
    val block = compileError("class ")
    assert(block === List(ExpectedIdentifier(None)))
  }

}
