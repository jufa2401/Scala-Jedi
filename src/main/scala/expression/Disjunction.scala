package expression

import context._
import value._

case class Disjunction(val operands: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Boole = {
    if(operands.size < 2) throw new TypeException("Disjunctions need 2 or more arguments")

    def helper(result: Boole, unseen: List[Expression]): Boole = {
      if (unseen == Nil) result
      else if (unseen.head.execute(env).isInstanceOf[Boole]) {
        unseen.head match {
          case _: Boole if unseen.head.execute(env).asInstanceOf[Boole].value => helper(Boole(true), Nil)
          case _ => helper(result, unseen.tail)
        }
      } else throw new TypeException("Disjunctions require Boole expressions")
    }
    helper(Boole(false), operands)
  }
}