import compiler.Expression.{FullEvaluation, SimpleEvaluation}
import compiler.elements.Element
import compiler.{Errors, SourceFile, Tokens}
import org.scalajs.dom
import org.scalajs.dom.{document, html, window}
import repl.Evaluator

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("EvaluatorJs")
object EvaluatorJs {

  @JSExport
  def main(in: html.Input,
           out: html.Div,
           evaluationMode: html.Input): Unit = {

    def listToUl(list: List[_]): String =
      list.map(e => s"<li>$e</li>").mkString("<ul>", "", "</ul>")

    def setTextOutput(string: String): Unit =
      out.innerHTML = s"$string"

    def evaluator: Evaluator = new Evaluator {

      override def setOutput(elements: List[Element], token: Option[Tokens.EvaluatedToken], source: SourceFile): Unit =
        setTextOutput(
          "<p>Elements:</p>" +
            listToUl(elements) +
            token.map(t => "<p>Result:</p>" + listToUl(t :: Nil)).getOrElse("") +
            "Tokens:" + listToUl(source.tokens)
        )

      override def setOutputError(errors: List[Errors.CompilerError], source: Option[SourceFile]): Unit =
        setTextOutput(
          "<p>Errors:</p>" +
            listToUl(errors) +
            source.map(s => "<p>Tokens:</p>" + listToUl(s.tokens)).getOrElse("")
        )

    }

    def evaluate(): Unit = {
      evaluator.evaluate(
        in.value,
        if (evaluationMode.checked) FullEvaluation else SimpleEvaluation
      )
    }

    evaluationMode.onchange = { (e: dom.Event) =>
      evaluate()
    }

    in.oninput = { (e: dom.Event) =>
      evaluate()
    }

    window.onload = { (e: dom.Event) =>
      evaluate()
    }

  }

}
