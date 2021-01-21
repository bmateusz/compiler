import repl.Evaluator

object Main {

  def main(args: Array[String]): Unit = {
    val input = args.mkString("")
    println(s"> Evaluate $input")
    Evaluator.evaluate(input) match {
      case Evaluator.EvaluationSuccess(elements, token, source) =>
        println(elements.mkString(" ") + token.map(_.toString).getOrElse(""))
      case Evaluator.EvaluationFailure(errors, source) =>
        println(errors.mkString(" "))
    }
  }

}
