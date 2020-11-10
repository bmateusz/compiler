package compiler

import compiler.Tokens.Token
import compiler.Types.Type

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

  case class UnexpectedToken(token: Token) extends CompilerError

  case class UnexpectedReturnType(typ: Type) extends CompilerError

  case class UnmatchedLeftParenthesis() extends CompilerError

  case class ExpectedRightParenthesis(got: Option[Token]) extends CompilerError

  case class ExpectedParameters(got: Option[Token]) extends CompilerError

  case class FileError(arg: String, throwable: Throwable) extends CompilerError

}
