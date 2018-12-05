package expression
import context.Environment
import value.{Closure, Value}

case class Lambda(val params: List[Identifier], val body: Expression) extends SpecialForm {
  def execute(env: Environment): Value = new Closure(params, body, env)
}