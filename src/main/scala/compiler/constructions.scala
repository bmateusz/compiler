package compiler

import scala.annotation.tailrec

case class Assignment(identifier: Identifier,
                      expression: Expression)

case class Value(token: Token)

class Block(val defs: List[String], val identifiers: List[String]) {
  def addIdentifier(identifier: Identifier): Block = new Block(defs, identifiers :+ identifier.value)

  def addDef(identifier: Identifier): Block = new Block(defs :+ identifier.value, identifiers)

  override def toString: String = s"defs: $defs\nidentifiers: $identifiers"
}

object Block {

  val empty: Block = new Block(List.empty, List.empty)

  @tailrec
  def parse(tokens: List[Token], block: Block): Block = {
    tokens match {
      case token :: rest =>
        token match {
          case Indentation(_) =>
            parse(rest, block)
          case Def =>
            parseDef(rest, block)
          case identifier@Identifier(_) =>
            parseAssignment(identifier, rest, block)
          case Class =>
            ???
          case NotImplemented =>
            block
          case unexpected =>
            throw new RuntimeException(unexpected.toString)
        }
      case Nil =>
        block
    }
  }

  def parseDef(rest: List[Token], block: Block): Block = {
    rest.headOption match {
      case Some(value) =>
        value match {
          case identifier@Identifier(_) =>
            parse(rest, block.addDef(identifier))
          case unexpected =>
            throw new RuntimeException(unexpected.toString)
        }
      case None =>
        throw new RuntimeException("unexpected")
    }
  }

  @tailrec
  def parseAssignment(identifier: Identifier, tokens: List[Token], block: Block): Block = {
    tokens match {
      case Indentation(_) :: xs =>
        parseAssignment(identifier, xs, block)
      case Equals :: xs =>
        val expr = Expression.parse(xs, List.empty, List.empty, None)
        ???
      case other =>
        ???
    }
  }
}
