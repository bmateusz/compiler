package compiler

import compiler.Tokens.{Class, Def, Enum, Equals, Identifier, Indentation, Token}

import scala.annotation.tailrec

case class Block(elements: List[Element]) {
  def add(element: Element): Block = copy(elements = elements :+ element)
}

object Block {

  val empty: Block =
    Block(List.empty)

  def parse(result: Result[Block], indentation: List[Indentation]): Result[Block] =
    result.map((block, rest) => parse(rest, block, indentation))

  @tailrec
  def parse(tokens: List[Token], block: Block, indentation: List[Indentation]): Result[Block] =
    tokens match {
      case (current: Indentation) :: xs =>
        indentation match {
          case Indentation(length) :: _ if current.length == length =>
            parse(xs, block, indentation)
          case Indentation(length) :: _ if current.length > length =>
            parse(xs, block, indentation :+ current)
          case Indentation(length) :: Nil if current.length < length =>
            Result(block, tokens)
          case Indentation(length) :: indentationRest if current.length < length =>
            parse(xs, block, indentationRest)
          case Nil =>
            parse(xs, block, List(current))
        }
      case Def :: xs =>
        Definition
          .parse(xs)
          .map { (definition, rest) =>
            parse(Result(block.add(definition), rest), indentation)
          }
      case Class :: xs =>
        compiler.Class
          .parse(xs, block)
          .map { (cls, rest) =>
            parse(Result(block.add(cls), rest), indentation)
          }
      case Enum :: xs =>
        compiler.Enum
          .parse(xs)
          .map { (cls, rest) =>
            parse(Result(block.add(cls), rest), indentation)
          }
      case (identifier: Identifier) :: Equals :: xs =>
        Assignment
          .parse(identifier, xs, block)
          .map { (assignment, rest) =>
            parse(Result(block.add(assignment), rest), indentation)
          }
      case Nil =>
        Result(block)
      case others =>
        Expression
          .parse(others, List.empty, List.empty, None)
          .map { (expr, rest) =>
            parse(Result(block.add(expr), rest), indentation)
          }
    }

}
