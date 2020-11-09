package compiler

import compiler.Errors.UnexpectedToken
import compiler.Tokens._

import scala.annotation.tailrec

case class Block(assignments: List[Assignment],
                 definitions: List[Definition],
                 classes: List[Class]) {
  def addAssignment(assignment: Assignment): Block = copy(assignments = assignments :+ assignment)

  def addDefinition(definition: Definition): Block = copy(definitions = definitions :+ definition)

  def addClass(cls: Class): Block = copy(classes = classes :+ cls)
}

object Block {

  val empty: Block =
    Block(List.empty, List.empty, List.empty)

  def parse(result: Result[Block]): Result[Block] =
    result.map((block, rest) => parse(rest, block))

  @tailrec
  def parse(tokens: List[Token], block: Block): Result[Block] =
    tokens match {
      case Indentation(_) :: xs =>
        parse(xs, block)
      case Def :: xs =>
        Definition
          .parse(xs, block)
          .map { (definition, rest) =>
            parse(Result(block.addDefinition(definition), rest))
          }
      case Class :: xs =>
        compiler.Class
          .parse(xs, block)
          .map { (cls, rest) =>
            parse(Result(block.addClass(cls), rest))
          }
      case (identifier: Identifier) :: xs =>
        Assignment
          .parse(identifier, xs, block)
          .map { (assignment, rest) =>
            parse(Result(block.addAssignment(assignment), rest))
          }
      case Nil =>
        Result(block)
      case other :: xs =>
        Result(UnexpectedToken(other), xs)
    }

}
