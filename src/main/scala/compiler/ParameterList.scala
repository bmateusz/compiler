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
  def parseParameterList(tokens: List[Token]): Either[CompilerError, ParameterList] = {
    tokens match {
      case Indentation(_) :: rest =>
        parseParameterList(rest)
      case LeftParenthesis :: rest =>
        val (left, right) = rest.span(_ != RightParenthesis)
        right.headOption match {
          case Some(RightParenthesis) =>
            parseParameters(left, empty)
          case other =>
            Left(ExpectedRightParenthesis(other))
        }
      case other =>
        Left(ExpectedParameterList(other.headOption))
    }
  }

  @tailrec
  def parseParameters(tokens: List[Token], pl: ParameterList): Either[CompilerError, ParameterList] =
    tokens match {
      case Indentation(_) :: rest =>
        parseParameters(rest, pl)
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
