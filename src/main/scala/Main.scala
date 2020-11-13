import compiler.Errors.{CompilerError, FileError}
import compiler.{Block, SourceFile, Tokens}
import repl.Repl

import scala.io.{Source, StdIn}
import scala.util.{Failure, Success, Using}

object Main {

  def main(args: Array[String]): Unit =
    if (args.isEmpty) repl() else compileFiles(args)

  def compileFiles(args: Array[String]): Unit = {
    val parsedSourceFiles: List[Either[List[CompilerError], SourceFile]] =
      args.toList.map { arg =>
        Using(Source.fromFile(arg)) {
          SourceFile.parse
        } match {
          case Success(value) => value
          case Failure(exception) => Left(List(FileError(arg, exception)))
        }
      }

    parsedSourceFiles.map {
      _.map { sourceFile =>
        sourceFile.compile
      }
    }
  }

  def repl(): Unit =
    new Repl {
      override def read() =
        StdIn.readLine("> ")

      override def println(block: Block): Unit =
        Console.out.println(block)

      override def printlnEvaluation(tokens: List[Tokens.EvaluatedToken]): Unit =
        Console.out.println(tokens.mkString(" "))

      override def printlnError(errors: List[CompilerError]): Unit =
        Console.err.println(errors.mkString("\n"))
    }.repl()

}
