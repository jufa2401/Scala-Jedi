package context

import expression._
import value._

import scala.util.parsing.combinator._

/*
 * Notes:
 * disjunction reduces to conjunction reduces to equality ... reduces to term
 * if A reduces to B, then B will have higher precedence than A
 * Example: sum reduces to product, so a + b * c = a + (b * c)
 * Had to make some big corrections to numeral regex
 * This could probably have been a singleton
 */

class Jedi1Parsers extends RegexParsers {

  //  def expression: Parser[Expression] = declaration | term | failure("Invalid")
  def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")

  def declaration: Parser[Declaration] = "def" ~ identifier ~ "=" ~ expression ^^ {
    case "def"~id~"="~exp => Declaration(id, exp)
  }

  def conditional: Parser[Conditional] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^ {
    case "if"~"("~cond~")"~cons ~ None => Conditional(cond, cons)
    case "if"~"("~cond~")"~cons~Some("else"~alt) => Conditional(cond, cons, alt)
  }

  def  disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^ {
    case con ~ Nil => con
    case con ~ more => Disjunction(con::more)
  }

  // conjunction ::= equality ~ ("&&" ~ equality)*
  def conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality) ^^ {
    case con ~ Nil => con
    case con ~ more => Conjunction(con::more)
  }

  // equality ::= inequality ~ ("==" ~ inequality)*
  def equality: Parser[Expression] = inequality ~ rep("==" ~> inequality) ^^ {
    case con ~ Nil => con
    case con ~ more => FunCall(Identifier("equals"), con::more)
  }

  // inequality ::= sum ~ (("<" | ">" | "!=") ~ sum)?
  def inequality: Parser[Expression] = sum ~ opt(("<" | ">" | "!=") ~ sum) ^^ {
    case con ~ None => con
    case (t ~ blah) => parseInequality(t, blah.get)
  }

  private def parseInequality(t: Expression, terms: ~[String, Expression]) = {
    terms match {
      case ~("<", t1)=> FunCall(Identifier("less"), List(t, t1))
      case ~(">", t1)=> FunCall(Identifier("more"), List(t, t1))
      case ~("!=", t1)=> FunCall(Identifier("unequals"), List(t, t1))
    }
  }

  // negate(exp) = 0 - exp
  private def negate(exp: Expression): Expression = {
    val sub = Identifier("sub")
    val zero = Integer(0)
    FunCall(sub, List(zero, exp))
  }

  // sum ::= product ~ ("+" | "-") ~ product)*  
  def sum: Parser[Expression] = product ~ rep(("+"|"-") ~ product ^^ {
    case "+"~s=>s
    case "-"~s=> negate(s)
  })^^{
    case p~Nil=> p
    case p~rest=>FunCall(Identifier("add"), p::rest)
  }

  // product ::= term ~ (("*" | "/") ~ term)*
  def product: Parser[Expression] = term ~ rep(("*"|"/") ~ term) ^^ {
    case t ~ term => parseProduct(t, term)
  }
  //TODO: Is there something wrong with this method??? Yup, fixed
  // generates left-to-right calls to mul and div:
  private  def parseProduct(t: Expression, terms: List[~[String, Expression]]): Expression = {
    terms match {
      case Nil => t
      case ~("*", t1)::more => parseProduct(FunCall(Identifier("mul"), List(t, t1)), more)
      case ~("/", t1)::more => parseProduct(FunCall(Identifier("div"), List(t, t1)), more)
      //
      case List(_) => t

    }
  }

  def term: Parser[Expression]  = funCall | literal | "("~>expression<~")"

  def literal: Parser[Expression] = boole | real | integer | chars | identifier

  // chars ::= any characters bracketed by quotes
  def chars: Parser[Chars] = {
    """\"[^"]+\"""".r ^^ (characters => Chars(characters.substring(1, characters.length - 1)))
  }

  // integer ::= 0|(\+|-)?[1-9][0-9]*
  def integer: Parser[Integer] = {
    """0|(\+|-)?[1-9][0-9]*""".r ^^ (integers => Integer(integers.toInt))
  }

  // real ::= (\+|-)?[0-9]+\.[0-9]+
  def real: Parser[Real] = {
    """(\+|-)?[0-9]+\.[0-9]+""".r ^^ (reals => Real(reals.toDouble))
  }

  // boole ::= true | false
  def boole: Parser[Boole] = {
    """true|false""".r ^^ (booles => Boole(booles.toBoolean))
  }

  // identifier ::= [a-zA-Z][a-zA-Z0-9]*
  def identifier : Parser[Identifier] = {
    """[a-zA-Z][a-zA-Z0-9]*""".r ^^ (identifiers => Identifier(identifiers))
  }

  // funCall ::= identifier ~ operands
  def funCall: Parser[FunCall] = identifier ~ operands ^^ {
    case identifiers ~ operands => FunCall(identifiers, operands)
  }

  // operands ::= "(" ~ (expression ~ ("," ~ expression)*)? ~ ")"
  def operands: Parser[List[Expression]] = "(" ~> opt(expression ~ rep("," ~> expression)) <~ ")" ^^ {
    case None => Nil
    case Some(con ~ Nil) => List(con)
    case Some(con ~ more) => con::more
  }

}
