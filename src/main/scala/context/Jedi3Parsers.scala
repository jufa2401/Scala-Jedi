package context

import expression._

class Jedi3Parsers extends Jedi2Parsers {


  // assignment ::= identifier ~ "=" ~ expression
  def assignment: Parser[Expression] = identifier ~ "=" ~ expression ^^ {
    case id ~ "=" ~ exp => Assignment(id, exp)
  }

  // iteration ::= "while" ~ "(" ~ expression ~ ")" ~ expression
  def iteration: Parser[Expression] = "while" ~ "(" ~> expression ~ ")" ~ expression ^^ {
    case condition ~ ")" ~ body => Iteration(condition, body)
  }

  // dereference ::= "[" ~ expression ~ "]"
  def dereference: Parser[Expression] = "[" ~> expression <~ "]" ^^ {
    case exp => FunCall(Identifier("dereference"), List(exp))
    case _ => throw new JediException("Error dereferencing")
  }
  // SWITCH ::= "switch" ~ EXPRESSION ~ "{" ~ EXPRESSION~ rep(";" ~ EXPRESSION) ~ "}"
  def switch: Parser[Switch] = "switch" ~ expression ~ "{" ~ expression ~ rep(";" ~> expression) ~"}" ^^ {
    case "switch" ~ condition ~ "{" ~ expression ~ Nil ~ "}" => Switch (condition, expression :: Nil)
    case "switch" ~ condition ~ "{" ~ expression ~ expressions ~ "}" => Switch (condition, expression :: expressions)
  }
  //LOOP ::= "loop"~"["~EXPRESSION~"]"~EXPRESSION
  def loop: Parser[Loop] = "loop" ~"[" ~ expression ~ "]" ~ expression ^^ {
    case "loop" ~ "[" ~ count ~ "]" ~ expression => Loop(count,expression)
//      ~ "{" ~ expression ~ expressions ~ "}" => Switch (condition, expression :: expressions)
  }
//  break ::= "break".
def break: Parser[Break] = "break" ^^ {
  case None => Nil
  case  "break" =>  break(Identifier("break"), exp::Nil)
  case _ => Break(Nil)
}

  override def term: Parser[Expression]  = loop | switch | thunk | lambda | funCall | block | assignment | dereference | literal | "("~>expression<~")"

//  def switch: Parser[Expression] = "switch" ~ expression ~ "{" ~ expression ~ rep(";" ~ expression) ~ "}" ^^{
//    case expression =>
//  }

  override def expression: Parser[Expression] = declaration | conditional | iteration | disjunction | failure("Invalid expression")

}