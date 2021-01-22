package compiler

import compiler.Tokens.EvaluatedToken

object Types {

  def parse(name: String): Type =
    simpleTypesMap.getOrElse(name, UnknownType(name))

  def fromEvaluatedToken(token: EvaluatedToken): Type =
    token match {
      case evaluated: Tokens.EvaluatedIdentifier => UnknownType(evaluated.identifier.value)
      case token@Tokens.CallDefinition(definition, values) => definition.returnType.getOrElse(Error(token))
      case Tokens.StringLiteral(value) => String
      case Tokens.Integer(value) => Integer
      case Tokens.Floating(value) => Floating
      case token => Error(token)
    }

  sealed trait Type {
    def name: String
  }

  case class Error(token: EvaluatedToken) extends Type {
    override val name: String = s"Error(${token.value})"
  }

  case object Initial extends Type {
    override val name: String = "Any"
  }

  case class UnknownType(override val name: String) extends Type

  sealed trait Number extends Type

  case object Integer extends Number {
    override val name: String = "Int"
  }

  case object Floating extends Number {
    override val name: String = "Float"
  }

  case object String extends Type {
    override val name: String = "String"
  }

  val simpleTypes = List(
    Integer,
    Floating,
    String
  )

  val simpleTypesMap: Map[String, Type] = simpleTypes.map(t => t.name -> t).toMap

}
