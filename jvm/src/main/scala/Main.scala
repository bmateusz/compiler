import compiler.Errors.{CompilerError, FileError}
import compiler.Elements.Element
import compiler.{SourceFile, Tokens}
import repl.Repl

import scala.io.{Source, StdIn}
import scala.util.chaining.scalaUtilChainingOps
import scala.util.{Failure, Success, Using}

object Main {

  def main(args: Array[String]): Unit =
    if (args.isEmpty) repl() else compileFiles(args)

  def compileFiles(args: Array[String]): Unit =
    args
      .toList
      .map { arg =>
        Using(Source.fromFile(arg)) {
          SourceFile.parse
        } match {
          case Success(value) => value
          case Failure(exception) => Left(List(FileError(arg, exception)))
        }
      }
      .pipe { parsedSourceFile: List[Either[List[CompilerError], SourceFile]] =>
        parsedSourceFile.map {
          _.map { sourceFile =>
            sourceFile.compile()
          }
        }
      }

  def repl(): Unit =
    new Repl {
      override def read(): String =
        Option(StdIn.readLine("> ")).getOrElse("")

      override def println(elements: List[Element]): Unit =
        Console.out.println(elements)

      override def printlnEvaluation(token: Tokens.EvaluatedToken): Unit =
        Console.out.println(token)

      override def printlnError(errors: List[CompilerError]): Unit =
        Console.err.println(errors.mkString("\n"))
    }.repl()

}
