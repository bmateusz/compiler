package compiler

object Types {

  sealed trait Type

  case class UnknownType(value: String) extends Type

}
