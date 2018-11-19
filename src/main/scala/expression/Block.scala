
package expression
import context._
import value.Value

case class Block(val expressions: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value = {
    val tempEnv = new Environment(env)
    expressions.map(_.execute(tempEnv)).last
  }
}