package value

import context.Environment
import expression.{Expression, Identifier}
// The defining environment is the extension of the defining environment
class Closure(val params: List[Identifier], val body: Expression, val defEnv: Environment) extends Value {
  def apply(args: List[Value]): Value = {
    //     create tempenv extending defenv
    val tempEnv = new Environment(defEnv)
    //    bulk put params and args into tempEnv
    tempEnv.bulkPut(params,args)
    body.execute(tempEnv)
  }

}
