package value
import context.Environment
import expression.{Expression, Identifier}
// The defining environment is the extension of the defining environment
class Closure(val params: List[Identifier], val body: Expression, val env: Environment) extends Value {
  def apply(args: List[Value]): Value =
    Some(new Environment(env))
      .map(tap(_.bulkPut(params, args)))
      .map(body.execute)
      .get


  def tap[T](effect: T => Unit)(x: T): T = {
    effect(x)
    x
  }
}

object Closure {
  def apply(params: List[Identifier], body: Expression, env: Environment) = new Closure(params, body, env)
}

