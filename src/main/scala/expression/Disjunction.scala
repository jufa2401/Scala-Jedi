package expression

import context._
import value._

case class Disjunction(val operands: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value = {
    try {
      operands.foreach(op => if (op.execute(env).asInstanceOf[Boole].value)
        return Boole(true))
      Boole(false)
    } catch {
      case _: TypeException => throw new TypeException("operands must be of type Boole")
    }
  }
}