package expression
class Switch(cond: Expression, expressions: List[Expression]) extends SpecialForm {

  import context.Environment
  import value.{Integer, Notification, Value}

  override def execute(env: Environment): Value = {
    import context.TypeException
    val value = cond.execute(env)
    if (!value.isInstanceOf[Integer]) throw new TypeException("Not an integer")
    val test = value.asInstanceOf[Integer]
    if (expressions.length < test.value - 1) Notification.UNSPECIFIED
    expressions(test.value).execute(env)
  }
}

object Switch {
  def apply(cond: Expression, expressions: List[Expression]) = new Switch(cond, expressions)
}