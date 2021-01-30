import compiler.Expression.{FullEvaluation, SimpleEvaluation}
import org.scalajs.dom.{Event, html, window}
import repl.Evaluator

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("EvaluatorJs")
object EvaluatorJs {

  @js.native
  trait MonacoEditor extends js.Object {
    def getValue(): String = js.native

    def onDidChangeModelContent(fn: js.Function1[Event, _]): Unit = js.native
  }

  @JSExport
  def main(in: MonacoEditor,
           out: html.Div,
           evaluationMode: html.Input): Unit = {

    def listToUl(list: List[_]): String =
      list.map(e => s"<li>$e</li>").mkString("<ul>", "", "</ul>")

    def setTextOutput(string: String): Unit =
      out.innerHTML = s"$string"

    def evaluate: Event => Unit = { (e: Event) =>
      Evaluator.evaluate(
        in.getValue(),
        if (evaluationMode.checked) FullEvaluation else SimpleEvaluation
      ) match {
        case Evaluator.EvaluationSuccess(elements, token, source) =>
          setTextOutput(
            "<p>Elements:</p>" +
              listToUl(elements) +
              token.map(t => "<p>Result:</p>" + listToUl(t :: Nil)).getOrElse("") +
              "Tokens:" + listToUl(source.tokens)
          )
        case Evaluator.EvaluationFailure(errors, source) =>
          setTextOutput(
            "<p>Errors:</p>" +
              listToUl(errors) +
              source.map(s => "<p>Tokens:</p>" + listToUl(s.tokens)).getOrElse("")
          )
      }
    }

    evaluationMode.onchange = evaluate
    in.onDidChangeModelContent(evaluate)
    window.onload = evaluate

  }

}
