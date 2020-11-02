import compiler.{CompilerError, FileError, SourceFile}

import scala.io.Source
import scala.util.{Failure, Success, Using}

object Main {
  def main(args: Array[String]): Unit = {

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
}
