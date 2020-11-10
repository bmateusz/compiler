package compiler

import compiler.Errors.{UnexpectedReturnType, UnexpectedToken}
import compiler.Parameters.Parameter
import compiler.Tokens.{Add, Identifier, Operator}
import compiler.Types.UnknownType

class ClassTest extends CompilerSpecs {

  it should "parse simple class" in {
    val block = compileSuccess("class A(x: Int)")
    assert(block === Block(
      List.empty,
      List.empty,
      List(Class(Identifier("A"), Parameters(List(Parameter(Identifier("x"), Types.Integer)), None)))
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
      List.empty,
      List.empty,
      List(Class(Identifier("A"), Parameters(List(Parameter(Identifier("value"), Types.String)), None)))
    ))
  }

  it should "report error when class has invalid identifier" in {
    val block = compileError("class +A(a: Float)")
    assert(block === List(UnexpectedToken(Operator(Add))))
  }

  it should "report error when class has return type" in {
    val block = compileError("class A(a: Float): MyClass")
    assert(block === List(UnexpectedReturnType(UnknownType("MyClass"))))
  }

}
