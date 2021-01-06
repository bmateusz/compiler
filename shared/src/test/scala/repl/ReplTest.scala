package repl

import compiler._
import compiler.elements.Element

import scala.collection.mutable

class ReplTest extends CompilerSpecs {

  case class MockRepl(messages: mutable.Queue[String],
                      results: mutable.Queue[String] = mutable.Queue.empty) extends Repl {

    override def read(): String =
      if (messages.isEmpty) "" else messages.dequeue()

    override def println(elements: List[Element]): Unit =
      results.addOne(elements.toString)

    override def printlnEvaluation(token: Tokens.EvaluatedToken): Unit =
      results.addOne(token.toString)

    override def printlnError(errors: List[Errors.CompilerError]): Unit =
      results.addOne(errors.toString)
  }

  it should "parse expression" in {
    val repl = MockRepl(mutable.Queue("12 + 4"))
    repl.repl()
    assert(repl.results.toList === List("Integer(16)"))
  }

  it should "parse assignment" in {
    val repl = MockRepl(mutable.Queue("x = 1"))
    repl.repl()
    assert(repl.results.toList === List("List(Assignment(Identifier(x),Some(Integer),Expression(List(Integer(1)))))"))
  }

  it should "evaluate assignments" in {
    val repl = MockRepl(mutable.Queue("x = 12", "y = 24", "x + y"))
    repl.repl()
    assert(repl.results.lastOption === Some("Integer(36)"))
  }

  it should "evaluate class assignments" in {
    val repl = MockRepl(mutable.Queue("class A(x:Int)", "a = A(232)", "a.x"))
    repl.repl()
    assert(repl.results.lastOption === Some("Integer(232)"))
  }

  it should "evaluate multiple class assignments" in {
    val repl = MockRepl(mutable.Queue("class A(x:Int)", "a = A(232)", "b = A(444)", "b.x"))
    repl.repl()
    assert(repl.results.lastOption === Some("Integer(444)"))
  }

  it should "parse invalid block" in {
    val repl = MockRepl(mutable.Queue("x . ="))
    repl.repl()
    assert(repl.results.toList === List("List(UnexpectedToken(Equals))"))
  }

  it should "parse token error" in {
    val repl = MockRepl(mutable.Queue("x = ß"))
    repl.repl()
    assert(repl.results.toList === List("List([0:4] Invalid token: ß)"))
  }

}
