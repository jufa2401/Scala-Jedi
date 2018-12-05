package expression
import context.{Environment, TypeException}
import value.{Boole, Value}

case class Iteration(var condition: Expression, var body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    var result: Value = null
    try {
      while (condition.execute(env).asInstanceOf[Boole].value) result = body.execute(env)
      result
    }
    catch {
      case _: ClassCastException => throw new TypeException("Condition must be of type Boole")
    }
  }
}
