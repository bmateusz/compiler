package compiler

import compiler.Errors.{CompilerError, ExpectedColon, ExpectedIdentifier, ExpectedParameters, ExpectedRightParenthesis, ExpectedType}
import compiler.Parameters.Parameter
import compiler.Tokens._
import compiler.Types.Type

import scala.annotation.tailrec

case class Parameters(value: List[Parameter], returnType: Option[Type]) {
  def addParameter(name: String, typ: String): Parameters =
    copy(value = value :+ Parameter(Identifier(name), Types.parse(typ)))

  def addReturnType(typ: String): Parameters =
    copy(returnType = Some(Types.parse(typ)))
}

object Parameters {

  case class Parameter(identifier: Identifier, typ: Type)

  val empty: Parameters = Parameters(List.empty, None)

  @tailrec
  def parse(tokens: List[Token]): Result[Parameters] = {
    tokens match {
      case Indentation(_) :: xs =>
        parse(xs)
      case LeftParenthesis :: xs =>
        val (left, right) = xs.span(_ != RightParenthesis)
        right match {
          case RightParenthesis :: Colon :: Identifier(returnType) :: rest =>
            Result.eitherSingleError(parseParameters(left, empty).map(_.addReturnType(returnType)), rest)
          case RightParenthesis :: rest =>
            Result.eitherSingleError(parseParameters(left, empty), rest)
          case _ =>
            Result(ExpectedRightParenthesis(None))
        }
      case other =>
        Result(ExpectedParameters(other.headOption))
    }
  }

  @tailrec
  def parseParameters(tokens: List[Token], pl: Parameters): Either[CompilerError, Parameters] =
    tokens match {
      case Indentation(_) :: xs =>
        parseParameters(xs, pl)
      case Identifier(name) :: Colon :: Identifier(typ) :: Indentation(_) :: rest =>
        parseParameters(rest, pl.addParameter(name, typ))
      case Identifier(name) :: Colon :: Identifier(typ) :: Comma :: rest =>
        parseParameters(rest, pl.addParameter(name, typ))
      case Identifier(name) :: Colon :: Identifier(typ) :: Nil =>
        Right(pl.addParameter(name, typ))
      case Nil =>
        Right(pl)
      case Identifier(name) :: Colon :: tokens =>
        Left(ExpectedType(tokens.headOption))
      case Identifier(name) :: tokens =>
        Left(ExpectedColon(tokens.headOption))
      case tokens =>
        Left(ExpectedIdentifier(tokens.headOption))
    }

}
