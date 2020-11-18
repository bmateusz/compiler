package compiler

import compiler.Tokens.{Class, Def, Enum, Equals, Identifier, Indentation, Token}

import scala.annotation.tailrec

case class Block(elements: Map[String, Element],
                 expressions: List[Expression]) {
  def sortedElements: List[Element] =
    elements
      .values
      .toList
      .sortBy(_.name.value)

  def add(element: Element): Block = copy(elements =
    elements + (element.name.value -> element))

  def add(expression: Expression): Block =
    copy(expressions = expressions :+ expression)

  def get(identifier: Identifier): Option[Element] =
    elements.get(identifier.value)
}

object Block {

  val empty: Block =
    Block(Map.empty, List.empty)

  def parse(result: Result[Block], indentation: List[Indentation]): Result[Block] =
    result.flatMap((block, rest) => parse(rest, block, indentation))

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
          .flatMap { (definition, rest) =>
            parse(Result(block.add(definition), rest), indentation)
          }
      case Class :: xs =>
        compiler.Class
          .parse(xs)
          .flatMap { (cls, rest) =>
            parse(Result(block.add(cls), rest), indentation)
          }
      case Enum :: xs =>
        compiler.Enum
          .parse(xs)
          .flatMap { (cls, rest) =>
            parse(Result(block.add(cls), rest), indentation)
          }
      case (identifier: Identifier) :: Equals :: xs =>
        Assignment
          .parse(identifier, xs, block)
          .flatMap { (assignment, rest) =>
            parse(Result(block.add(assignment), rest), indentation)
          }
      case Nil =>
        Result(block)
      case others =>
        Expression
          .parse(others, List.empty, List.empty, None)
          .flatMap { (expr, rest) =>
            parse(Result(block.add(expr), rest), indentation)
          }
    }

}
