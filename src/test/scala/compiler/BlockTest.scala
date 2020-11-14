package compiler

import compiler.Parameters.Parameter
import compiler.Tokens.{Floating, Identifier, Integer, SimpleTokens, StringLiteral}

class BlockTest extends CompilerSpecs {

  it should "be compiled" in {
    val block = compileSuccess(exampleCode)
    assert(block === Block(
      List(
        Assignment(Identifier("x"), Expression(List(Integer(1)))),
        Assignment(Identifier("y"), Expression(List(Floating(3.14)))),
        Assignment(Identifier("z"), Expression(List(StringLiteral("hello")))),
        Class(Identifier("A"), Parameters(
          List(Parameter(Identifier("z"), Types.String)),
          None
        )),
        Definition(Identifier("function"), Parameters(
          List(Parameter(Identifier("parameter"), Types.Integer)),
          Some(Types.Integer)
        ), Some(Block(List(Expression(List(Integer(6), Integer(2), SimpleTokens.`*`))))))
      )))
  }

}
