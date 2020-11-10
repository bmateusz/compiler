package compiler

import compiler.Errors.CompilerError
import compiler.Tokens.Token

case class Result[+A](value: Either[List[CompilerError], A],
                      rest: List[Token]) {
  def right: Either.RightProjection[List[CompilerError], A] = value.right

  def left: Either.LeftProjection[List[CompilerError], A] = value.left

  def map[B](f: (A, List[Token]) => Result[B]): Result[B] =
    value match {
      case Left(value) => Result(value, rest)
      case Right(value) => f(value, rest)
    }
}

object Result {

  implicit class ResultOps[+A](result: List[Result[A]]) {
    def mapEither[B](mapLefts: List[List[CompilerError]] => List[CompilerError],
                     mapRights: List[A] => B): Either[List[CompilerError], B] = {
      val (lefts, rights) = result.map(_.value).partitionMap(identity)

      if (lefts.isEmpty) {
        Right(mapRights(rights))
      } else {
        Left(mapLefts(lefts))
      }
    }
  }

  def apply[A](value: Either[List[CompilerError], A], rest: List[Token]): Result[A] =
    new Result(value, rest)

  // because of type erasure
  def eitherSingleError[A](value: Either[CompilerError, A], rest: List[Token]): Result[A] =
    new Result(value match {
      case Left(value) => Left(List(value))
      case Right(value) => Right(value)
    }, rest)

  def apply[A](success: A): Result[A] =
    new Result(Right(success), Nil)

  def apply[A](success: A, rest: List[Token]): Result[A] =
    new Result(Right(success), rest)

  def apply[A](error: List[CompilerError]): Result[A] =
    new Result(Left(error), Nil)

  def apply[A](error: List[CompilerError], rest: List[Token]): Result[A] =
    new Result(Left(error), rest)

  def apply[A](error: CompilerError): Result[A] =
    new Result(Left(List(error)), Nil)

  def apply[A](error: CompilerError, rest: List[Token]): Result[A] =
    new Result(Left(List(error)), rest)

}
