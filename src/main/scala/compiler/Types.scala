package compiler

object Types {

  sealed trait Type {
    val name: String
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

  def parse(name: String): Type =
    simpleTypesMap.getOrElse(name, UnknownType(name))

}