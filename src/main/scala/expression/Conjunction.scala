package expression

import context._
import value._

case class Conjunction(val operands: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Boole = {
    if(operands.size < 2) throw new TypeException("Conjunctions need 2 or more arguments")
    val operands2 = operands.map(exp => exp.execute(env)).filter(value => value.isInstanceOf[Boole]) // executes all operands in the environment, and filters operands which are boolean

    def helper(result: Boole, unseen: List[Value]): Boole = {
      if(unseen == Nil) result else unseen.head match {
        case boole: Boole if !boole.value => helper(Boole(false), Nil)
        case _ => helper(result, unseen.tail)
      }
    }
    helper(Boole(true), operands2)
  }
}
