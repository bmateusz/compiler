package compiler

import compiler.Errors.ExpectedIdentifier
import compiler.Tokens.{Add, Identifier, Operator}

class EnumTest extends CompilerSpecs {

  it should "parse simple enum" in {
    val block = compileSuccess("enum Boolean(True, False)")
    assert(block === Block(
      List(Enum(Identifier("Boolean"), List(Identifier("True"), Identifier("False"))))
    ))
  }

  it should "parse empty enum" in {
    val block = compileSuccess("enum Empty()")
    assert(block === Block(
      List(Enum(Identifier("Empty"), List()))
    ))
  }

  it should "parse simple class with line break" in {
    val block = compileSuccess(
      """
      enum A(
        X
        Y
      )
      """
    )
    assert(block === Block(
      List(Enum(Identifier("A"), List(Identifier("X"), Identifier("Y"))))
    ))
  }

  it should "report error when class has invalid identifier" in {
    val block = compileError("enum +A()")
    assert(block === List(ExpectedIdentifier(Some(Operator(Add)))))
  }

  it should "report error for no name" in {
    val block = compileError("enum ")
    assert(block === List(ExpectedIdentifier(None)))
  }

}
