package expression
import context._
import value._

case class Break() extends SpecialForm {
  def execute(env: Environment): Value = {
    throw new BreakException("DONE")
  }
}