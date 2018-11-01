package expression

import context._
import value._

case class Declaration(val id: Identifier, val exp: Expression) extends SpecialForm {
  def execute(env: Environment): Notification = {
    env(id) = exp.execute(env)
    Notification.OK
  }
}