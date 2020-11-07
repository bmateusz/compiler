package compiler

import compiler.Tokens.Token

object Errors {

  sealed trait CompilerError

  case class InvalidToken(row: Int, col: Int, invalid: String) extends CompilerError {
    override def toString: String = s"[$row:$col] Invalid token: $invalid"
  }

  object InvalidToken {
    def apply(row: Int, lineString: String, rest: String): InvalidToken = {
      val maxLength = 30

      new InvalidToken(
        row,
        lineString.length - rest.length,
        if (rest.length > maxLength) rest.take(maxLength - 3) + "..." else rest)
    }
  }

  case class ExpectedRightParenthesis(got: Option[Token]) extends CompilerError

  case class ExpectedParameterList(got: Option[Token]) extends CompilerError

  case class FileError(arg: String, throwable: Throwable) extends CompilerError

}
