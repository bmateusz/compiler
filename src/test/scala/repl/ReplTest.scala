package repl

import compiler._

import scala.collection.mutable

class ReplTest extends CompilerSpecs {

  case class MockRepl(messages: mutable.Queue[String],
                      results: mutable.Queue[String] = mutable.Queue.empty) extends Repl {

    override def read(): String =
      if (messages.isEmpty) "" else messages.dequeue()

    override def println(elements: List[Element]): Unit =
      results.addOne(elements.toString)

    override def printlnEvaluation(tokens: List[Tokens.EvaluatedToken]): Unit =
      results.addOne(tokens.toString)

    override def printlnError(errors: List[Errors.CompilerError]): Unit =
      results.addOne(errors.toString)
  }

  it should "parse expression" in {
    val repl = MockRepl(mutable.Queue("12 + 4"))
    repl.repl()
    assert(repl.results.toList === List("List(Integer(16))"))
  }

  it should "parse assignment" in {
    val repl = MockRepl(mutable.Queue("x = 1"))
    repl.repl()
    assert(repl.results.toList === List("List(Assignment(Identifier(x),Expression(List(Integer(1)))))"))
  }

  it should "parse invalid block" in {
    val repl = MockRepl(mutable.Queue("x , ="))
    repl.repl()
    assert(repl.results.toList === List("List(UnexpectedToken(Comma))"))
  }

  it should "parse token error" in {
    val repl = MockRepl(mutable.Queue("x = ß"))
    repl.repl()
    assert(repl.results.toList === List("List([0:3] Invalid token:  ß)"))
  }

}
