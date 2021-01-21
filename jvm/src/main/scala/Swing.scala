import repl.Evaluator

import java.awt.event._
import java.awt.{Font, GridBagConstraints, GridBagLayout}
import javax.swing._
import javax.swing.event.{DocumentEvent, DocumentListener}

object Swing {

  private def createAndShowGUI(): Unit = {
    val frame = new JFrame("CompilerUi")
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
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

    class MyDocumentListener extends DocumentListener {
      val newline = "\n"

      override def insertUpdate(e: DocumentEvent): Unit = {
        update(e)
      }

      override def removeUpdate(e: DocumentEvent): Unit = {
        update(e)
      }

      override def changedUpdate(e: DocumentEvent): Unit = {}

      def update(e: DocumentEvent): Unit =
        Evaluator.evaluate(inputTextArea.getText) match {
          case Evaluator.EvaluationSuccess(elements, token, source) =>
            outputTextArea.setText(
              elements.mkString("", newline, newline) +
                token.map(newline + "Result: " + _ + newline).getOrElse("") +
                source.tokens.mkString(newline + "Tokens: ", " ", newline)
            )
          case Evaluator.EvaluationFailure(errors, source) =>
            outputTextArea.setText(
              errors.mkString("", newline, newline) +
                source.map(_.tokens.mkString("Tokens: ", " ", newline)).getOrElse("")
            )
        }
    }

    override def actionPerformed(evt: ActionEvent): Unit = {}
  }

}
