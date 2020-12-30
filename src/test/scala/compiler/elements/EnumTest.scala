package compiler.elements

import compiler.CompilerSpecs
import compiler.Errors.{EmptyEnum, ExpectedIdentifier, ExpectedLeftParenthesis, ExpectedRightParenthesis, NotUniqueEnumValues}
import compiler.Tokens.{Add, Identifier, Operator}

class EnumTest extends CompilerSpecs {

  it should "parse simple enum" in {
    val block = compileSuccess("enum Boolean(True, False)")
    assert(block === Block(
      List(
        Enum(Identifier("Boolean"), List(Identifier("True"), Identifier("False")))
      ),
      None
    ))
  }

  it should "parse simple enum with line break" in {
    val block = compileSuccess(
      """
      enum A(
        X
        Y
      )
      """
    )
    assert(block === Block(
      List(
        Enum(Identifier("A"), List(Identifier("X"), Identifier("Y")))
      ),
      None
    ))
  }

  it should "parse empty enum" in {
    val block = compileError("enum Empty()")
    assert(block === List(EmptyEnum("Empty")))
  }

  it should "parse not unique enum values" in {
    val block = compileError("enum NotUnique(a, b, b, c, d, e, e, e, f, c)")
    assert(block === List(NotUniqueEnumValues("NotUnique", List("b", "c", "e"))))
  }

  it should "report error when enum name is not identifier" in {
    val block = compileError("enum +A()")
    assert(block === List(ExpectedIdentifier(Some(Operator(Add)))))
  }

  it should "report error when enum value is not identifier" in {
    val block = compileError("enum A(First, +)")
    assert(block === List(ExpectedIdentifier(Some(Operator(Add)))))
  }

  it should "report error when enum does not start with left parenthesis" in {
    val block = compileError("enum A B (First, Second)")
    assert(block === List(ExpectedLeftParenthesis(Some(Identifier("B")))))
  }

  it should "report error when enum value has no right parenthesis" in {
    val block = compileError("enum A(First, Second")
    assert(block === List(ExpectedRightParenthesis(None)))
  }

  it should "report error for no name" in {
    val block = compileError("enum ")
    assert(block === List(ExpectedIdentifier(None)))
  }

}
