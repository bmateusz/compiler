package repl

import compiler.Errors.CompilerError
import compiler.Expression.FullEvaluation
import compiler.Tokens.EvaluatedToken
import compiler.elements.{Block, Element}
import compiler.{Expression, SourceFile}

import java.awt.event._
import java.awt.{Font, GridBagConstraints, GridBagLayout}
import javax.swing._
import javax.swing.event.{DocumentEvent, DocumentListener}

object Swing {

  private val newline = "\n"

  private def createAndShowGUI(): Unit = { //Create and set up the window.
    val frame = new JFrame("CompilerUi")
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.add(new CompilerUi)
    frame.pack()
    frame.setVisible(true)
  }

  def main(args: Array[String]): Unit = {
    javax.swing.SwingUtilities.invokeLater(new Runnable() {
      override def run(): Unit = {
        createAndShowGUI()
      }
    })
  }

  class CompilerUi() extends JPanel(new GridBagLayout) with ActionListener {
    protected var inputTextArea = new JTextArea(25, 120)
    inputTextArea.setFont(new Font("monospaced", Font.PLAIN, 12))
    val inputScrollPane = new JScrollPane(inputTextArea)
    inputTextArea.getDocument.addDocumentListener(new MyDocumentListener());

    protected var outputTextArea = new JTextArea(25, 120)
    outputTextArea.setEditable(false)
    outputTextArea.setFont(new Font("monospaced", Font.PLAIN, 12))
    val outputScrollPane = new JScrollPane(outputTextArea)

    val c = new GridBagConstraints
    c.gridwidth = GridBagConstraints.REMAINDER
    c.fill = GridBagConstraints.BOTH
    c.weightx = 1.0
    c.weighty = 1.0
    add(inputScrollPane, c)
    add(outputScrollPane, c)

    def setOutput(elements: List[Element], token: Option[EvaluatedToken]): Unit =
      outputTextArea.setText(elements.mkString("", newline, newline) + token.map(newline + _ + newline).getOrElse(""))

    def setOutputError(errors: List[CompilerError]): Unit = outputTextArea.setText(errors.mkString("", newline, newline))

    class MyDocumentListener extends DocumentListener {
      val newline = "\n"

      def insertUpdate(e: DocumentEvent): Unit = {
        updateLog(e, "inserted into")
      }

      def removeUpdate(e: DocumentEvent): Unit = {
        updateLog(e, "removed from")
      }

      def changedUpdate(e: DocumentEvent): Unit = {}

      def updateLog(e: DocumentEvent, action: String): Unit = {
        val text = inputTextArea.getText

        if (text.nonEmpty) {
          SourceFile.parse(text) match {
            case Right(source) =>
              source.compile(Block.empty).value match {
                case Right(newBlock) =>
                  newBlock.evaluate().value match {
                    case Left(evaluationError) =>
                      setOutputError(evaluationError)
                    case Right(evaluatedBlock) =>
                      newBlock.expression match {
                        case Some(expr: Expression) =>
                          setOutput(evaluatedBlock.sortedElements, Some(expr.evaluate(evaluatedBlock, FullEvaluation)))
                        case _ =>
                          setOutput(evaluatedBlock.sortedElements, None)
                      }
                  }
                case Left(compileError) =>
                  setOutputError(compileError)
              }
            case Left(compileError) =>
              setOutputError(compileError)
          }
        } else {
          setOutput(List.empty, None)
        }
      }
    }

    override def actionPerformed(evt: ActionEvent): Unit = {}
  }

}
