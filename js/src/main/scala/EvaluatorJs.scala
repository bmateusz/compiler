import compiler.elements.Element
import compiler.{Errors, SourceFile, Tokens}
import org.scalajs.dom
import org.scalajs.dom.html
import repl.Evaluator

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("EvaluatorJs")
object EvaluatorJs {

  @JSExport
  def main(in: html.Input,
           out: html.Div): Unit = {

    def setTextOutput(string: String): Unit =
      out.innerHTML = s"<pre>$string</pre>"

    def evaluator: Evaluator = new Evaluator {
      val newline = "\n"

      override def setOutput(elements: List[Element], token: Option[Tokens.EvaluatedToken]): Unit =
        setTextOutput(
          elements.mkString("", newline, newline) +
            token.map(newline + _ + newline).getOrElse("")
        )

      override def setOutputError(errors: List[Errors.CompilerError], source: Option[SourceFile]): Unit =
        setTextOutput(
          errors.mkString("", newline, newline) +
            source.map(_.tokens.mkString("Tokens: ", " ", newline)).getOrElse("")
        )
    }

    in.onkeyup = { (e: dom.Event) =>
      evaluator.evaluate(in.value)
    }

  }

}
