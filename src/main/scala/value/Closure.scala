package value

import context.{Environment, TypeException}
import expression.{Expression, Identifier}
// The defining environment is the extension of the defining environment
class Closure(val params: List[Identifier], val body: Expression, val defEnv: Environment) extends Value {
  def apply(args: List[Value], callEnv: Environment = null): Value = {
    //     create tempenv extending defenv
    val tempEnv = new Environment(defEnv)
    if(args.size != params.size) throw new TypeException("parameters and args lengths don't match")
    //    bulk put params and args into tempEnv
    tempEnv.bulkPut(params,args)
    body.execute(tempEnv)
  }

}
