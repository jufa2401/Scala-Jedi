package expression

import context.Environment
import value.Value

trait Literal extends Value with Expression{
    def execute(env: Environment): Value = this
}