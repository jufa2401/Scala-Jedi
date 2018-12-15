package expression
import value._
case class Loop(count: Expression, body: Expression) extends SpecialForm {

  import context.{Environment, TypeException}
  import value.Value

  override def execute(env: Environment): Value = {
    var result: Value = null
    if(count.execute(env).asInstanceOf[Integer].value>0) {
      for (_ <- 1 to count.execute(env).asInstanceOf[Integer].value) result = body.execute(env)
      result
    } else {
      throw new TypeException("Count must be positive")
    }
  }
}

