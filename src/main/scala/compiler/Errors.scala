package compiler

import compiler.Tokens.{EvaluatedToken, EvaluationErrorToken, Token}
import compiler.Types.Type

object Errors {

  sealed trait CompilerError

  case class InvalidToken(row: Int, col: Int, invalid: String) extends CompilerError {
    override def toString: String = s"[$row:$col] Invalid token: $invalid"
  }

  object InvalidToken {
    val maxLength = 30

    def apply(row: Int, lineString: String, rest: String): InvalidToken =
      new InvalidToken(
        row,
        lineString.length - rest.length,
        if (rest.length > maxLength) rest.take(maxLength - 3) + "..." else rest
      )
  }

  case class UnexpectedToken(token: Token) extends CompilerError

  case class UnexpectedReturnType(typ: Type) extends CompilerError

  case class UnmatchedRightParenthesis() extends CompilerError

  case class UnmatchedLeftParenthesis() extends CompilerError

  case class ExpectedLeftParenthesis(got: Option[Token]) extends CompilerError

  case class ExpectedRightParenthesis(got: Option[Token]) extends CompilerError

  case class EmptyEnum(name: String) extends CompilerError

  case class NotUniqueEnumValues(name: String, notUnique: List[String]) extends CompilerError

  case class NotUniqueParameters(notUnique: List[String]) extends CompilerError

  case class ExpectedIdentifier(got: Option[Token]) extends CompilerError

  case class ExpectedColon(got: Option[Token]) extends CompilerError

  case class ExpectedType(got: Option[Token]) extends CompilerError

  case class Redefinition(name: String) extends CompilerError

  case class DefinitionWithoutBody(name: String) extends CompilerError

  case class AssignmentError(token: EvaluationErrorToken) extends CompilerError

  case class TypeError(calculated: Type, provided: Type) extends CompilerError

  case class FileError(arg: String, throwable: Throwable) extends CompilerError

  case class UnparsedTokens(tokens: List[Token]) extends CompilerError

}
