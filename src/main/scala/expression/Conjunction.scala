package expression

import context._
import value._

case class Conjunction(val operands: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Boole = {
    if(operands.size < 2) throw new TypeException("Conjunctions need 2 or more arguments")

   def helper(result:Boole, unseen: List[Expression]): Boole = {
     if (unseen == Nil) result
     else if (unseen.head.execute(env).isInstanceOf[Boole]) {
       unseen.head.execute(env) match {
         case boole: Boole if !boole.value => helper(Boole(false),Nil)
         case _ => helper(result,unseen.tail)
       }
     } else throw new TypeException("Conjunctions require boolean expressions")
   }

    helper(Boole(true), operands)
  }
}

