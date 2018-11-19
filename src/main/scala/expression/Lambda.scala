package expression
import context.Environment
import value.{Value,Closure}

case class Lambda(val params: List[Identifier], val body: Expression) extends SpecialForm {
  def execute(env: Environment): Value = new Closure(params, body, env)

}