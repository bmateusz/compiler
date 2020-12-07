package compiler.elements

import compiler.Errors.{CompilerError, ExpectedColon, ExpectedIdentifier, ExpectedLeftParenthesis, ExpectedRightParenthesis, ExpectedType, NotUniqueParameters}
import compiler.Tokens.{TokenListExtension, _}
import compiler.Types.Type
import compiler.elements.Parameters.Parameter
import compiler.{Result, Types}

import scala.annotation.tailrec

case class Parameters(values: List[Parameter], returnType: Option[Type]) {
  def addParameter(name: String, typ: String): Parameters =
    copy(values = values :+ Parameter(Identifier(name), Types.parse(typ)))

  def addReturnType(typ: String): Parameters =
    copy(returnType = Some(Types.parse(typ)))

  def notUniqueIdentifiers(): List[String] =
    Identifier.notUniqueIdentifiers(values.map(_.identifier))

  def findParameter(identifier: Identifier): Option[(Parameter, Int)] =
    values.zipWithIndex.find(_._1.identifier == identifier)
}

object Parameters {

  case class Parameter(identifier: Identifier, typ: Type)

  val empty: Parameters = Parameters(List.empty, None)

  @tailrec
  def parse(tokens: List[Token]): Result[Parameters] =
    tokens match {
      case Indentation(_) :: xs =>
        parse(xs)
      case LeftParenthesis :: xs =>
        val (left, right) = xs.spanMatchingRightParenthesis()
        right match {
          case RightParenthesis :: Colon :: Identifier(returnType) :: rest =>
            Result(parseParameters(left, empty).map(_.addReturnType(returnType)), rest)
          case RightParenthesis :: rest =>
            Result(parseParameters(left, empty), rest)
          case _ =>
            Result(ExpectedRightParenthesis(None))
        }
      case other =>
        Result(ExpectedLeftParenthesis(other.headOption))
    }

  @tailrec
  def parseParameters(tokens: List[Token], pl: Parameters): Either[List[CompilerError], Parameters] =
    tokens match {
      case Indentation(_) :: xs =>
        parseParameters(xs, pl)
      case Identifier(name) :: Colon :: Identifier(typ) :: Indentation(_) :: rest =>
        parseParameters(rest, pl.addParameter(name, typ))
      case Identifier(name) :: Colon :: Identifier(typ) :: Comma :: rest =>
        parseParameters(rest, pl.addParameter(name, typ))
      case Identifier(name) :: Colon :: Identifier(typ) :: Nil =>
        checkParameters(pl.addParameter(name, typ))
      case Nil =>
        checkParameters(pl)
      case Identifier(name) :: Colon :: tokens =>
        Left(List(ExpectedType(tokens.headOption)))
      case Identifier(name) :: tokens =>
        Left(List(ExpectedColon(tokens.headOption)))
      case tokens =>
        Left(List(ExpectedIdentifier(tokens.headOption)))
    }

  def checkParameters(parameters: Parameters): Either[List[CompilerError], Parameters] =
    parameters.notUniqueIdentifiers() match {
      case Nil => Right(parameters)
      case xs => Left(List(NotUniqueParameters(xs)))
    }

}
