package compiler

import compiler.Errors.{CompilerError, ExpectedParameterList, ExpectedRightParenthesis}
import compiler.ParameterList.Parameter
import compiler.Tokens._
import compiler.Types.Type

import scala.annotation.tailrec

case class ParameterList(value: List[Parameter]) {
  def addParameter(name: String, typ: String): ParameterList =
    ParameterList(value :+ Parameter(Identifier(name), Types.parse(typ)))
}

object ParameterList {

  val empty: ParameterList = ParameterList(List.empty)

  case class Parameter(identifier: Identifier, typ: Type)

  @tailrec
  def parseParameterList(tokens: List[Token]): Result[ParameterList] = {
    tokens match {
      case Indentation(_) :: xs =>
        parseParameterList(xs)
      case LeftParenthesis :: xs =>
        val (left, right) = xs.span(_ != RightParenthesis)
        right match {
          case RightParenthesis :: rest =>
            Result.eitherSingleError(parseParameters(left, empty), rest)
          case other :: rest =>
            Result(ExpectedRightParenthesis(Some(other)), rest)
          case Nil =>
            Result(ExpectedRightParenthesis(None))
        }
      case other =>
        Result(ExpectedParameterList(other.headOption))
    }
  }

  @tailrec
  def parseParameters(tokens: List[Token], pl: ParameterList): Either[CompilerError, ParameterList] =
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
      case tokens =>
        Left(ExpectedRightParenthesis(tokens.headOption))
    }

}
