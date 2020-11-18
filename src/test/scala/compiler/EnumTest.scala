package compiler

import compiler.Errors.{EmptyEnum, ExpectedIdentifier, NotUniqueEnumValues}
import compiler.Tokens.{Add, Identifier, Operator}

class EnumTest extends CompilerSpecs {

  it should "parse simple enum" in {
    val block = compileSuccess("enum Boolean(True, False)")
    assert(block === Block(
      Map(
        "Boolean" -> Enum(Identifier("Boolean"), List(Identifier("True"), Identifier("False")))
      ),
      List.empty
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
      Map(
        "A" -> Enum(Identifier("A"), List(Identifier("X"), Identifier("Y")))
      ),
      List.empty
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

  it should "report error when class has invalid identifier" in {
    val block = compileError("enum +A()")
    assert(block === List(ExpectedIdentifier(Some(Operator(Add)))))
  }

  it should "report error for no name" in {
    val block = compileError("enum ")
    assert(block === List(ExpectedIdentifier(None)))
  }

}
