package expression
import context.Environment
import value.{Thunk, Value}

class MakeThunk(body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = Thunk(body, env)
}

object MakeThunk {
  def apply(body: Expression): MakeThunk = new MakeThunk(body)
}