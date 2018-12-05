package expression

import context.{Environment, TypeException, UndefinedException, alu}
import value.{Closure, Value}

case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression {

  def execute(env: Environment): Value = {
    val arguments: List[Value] = operands.map(_.execute(env))
    try {
      val isThisAClosure = operator.execute(env)
      if(!isThisAClosure.isInstanceOf[Closure]) throw new TypeException("Not a closure") // Professor asked for throwing TypeException. Guessing alu.execute should happen in the catch then?
      val closure = isThisAClosure.asInstanceOf[Closure] // this is unnecessary??
      closure(arguments)
    } catch {
      case _: UndefinedException => alu.execute(operator, arguments)
    }
  }
}