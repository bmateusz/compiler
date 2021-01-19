import compiler.Errors.CompilerError
import compiler.elements.Element
import compiler.{SourceFile, Tokens}
import repl.Evaluator

object Main {

  def main(args: Array[String]): Unit = {
    val evaluator = new Evaluator {
      override def setOutput(elements: List[Element], token: Option[Tokens.EvaluatedToken], source: SourceFile): Unit =
        println(elements.mkString(" ") + token.map(_.toString).getOrElse(""))

      override def setOutputError(errors: List[CompilerError], source: Option[SourceFile]): Unit =
        println(errors.mkString(" "))
    }
    val input = args.mkString("")
    println(s"> Evaluate $input")
    evaluator.evaluate(input)
  }

}
