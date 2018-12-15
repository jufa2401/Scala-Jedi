package expression
import context._
import value._

case class Iteration(condition: Expression, body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    var break = false
    while(condition.execute(env) == Boole(true)) {
      try {
        if(!break) body.execute(env)
      } catch {
        case exception: BreakException => println(exception.gripe); break = true
      }
    }
    Notification.DONE
  }
}