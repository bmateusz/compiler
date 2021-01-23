package repl

import compiler.CompilerSpecs
import compiler.Expression.{FullEvaluation, SimpleEvaluation}
import compiler.Tokens.Integer

class EvaluatorSpecs extends CompilerSpecs {

  val example =
    """
class A(x: Int)
  aValue: Int = 50000
  def afn: Int = 600000
  def million(n: Int): Int = 1000000 * n

class B(a: A)
  bValue: Int = 20
  def bfn(addition: Int): Int =
    y = 1
    y + bValue + addition + a.x + a.aValue + a.afn + a.million(7)

class C
  x = 80000000
  def cc(mult: Int) = 100000000 * mult

B(A(4000)).bfn(300) + C.x + C.cc(9)
    """

  it should "simple evaluate example" in {
    assert(
      Evaluator.evaluate(example, SimpleEvaluation).token.isDefined
    )
  }

  it should "full evaluate example" in {
    assert(
      Evaluator.evaluate(example, FullEvaluation).token === Some(Integer(987654321))
    )
  }

}
