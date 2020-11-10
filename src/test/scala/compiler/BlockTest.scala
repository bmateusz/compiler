package compiler

import compiler.Parameters.Parameter
import compiler.Tokens.{Identifier, Integer}

class BlockTest extends CompilerSpecs {

  it should "be compiled" in {
    val block = compileSuccess(exampleCode)
    assert(block === Block(
      List(
        Assignment(Identifier("x"), Expression(List(Integer(1)))),
        Assignment(Identifier("y"), Expression(List(Integer(2))))
      ),
      List(Definition(Identifier("function"), Parameters(
        List(Parameter(Identifier("parameter"), Types.Integer)),
        Some(Types.Integer)
      ))),
      List(Class(Identifier("A"), Parameters(
        List(Parameter(Identifier("z"), Types.String)),
        None
      )))
    ))
  }

}
