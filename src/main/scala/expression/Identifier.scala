package expression

import context.Environment
import value._

case class Identifier(name: String) extends Expression {
   override def toString: String = name

   def execute(env: Environment): Value =
      env(this) match {
         case thunk: Thunk => thunk() // calls thunk's apply function
         case value: Value => value
      }
}