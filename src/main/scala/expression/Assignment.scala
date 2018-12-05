package expression
import context.{Environment, TypeException}
import value.{Notification, Variable}
// vbl = variable
case class Assignment(val vbl: Identifier, val update: Expression) extends SpecialForm {
  def execute(env: Environment) = {
//    If it's not a variable an exception will be thrown
    if(!vbl.execute(env).isInstanceOf[Variable]) throw new TypeException("Can only assign to variables")
    else
      vbl.execute(env).asInstanceOf[Variable].content = update.execute(env)
    Notification.DONE
  }
}