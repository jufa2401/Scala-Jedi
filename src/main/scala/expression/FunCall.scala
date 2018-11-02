package expression

import context.{Environment, alu}
import value.Value

case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression {

  def execute(env: Environment): Value = {
    val arguments = operands.map(_.execute(env))
    alu.execute(operator, arguments)
  }
}