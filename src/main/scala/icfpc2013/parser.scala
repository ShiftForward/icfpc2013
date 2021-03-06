package icfpc2013

import scala.util.parsing.combinator.RegexParsers

object BvParser extends RegexParsers {
  def program = ("(" ~> "lambda" ~> "(" ~> id <~ ")") ~ exp <~ ")" ^^ {
    case id ~ expression =>
      Program(id, expression)
  }

  def exp: Parser[Expression] = zero | one | id | iff | fold | op

  def zero = "0" ^^^ Zero
  def one = "1" ^^^ One
  def id = "[a-z][a-z_0-9]*".r ^^ { Id(_) }
  def iff = "(" ~> "if0" ~> exp ~ exp ~ exp <~ ")" ^^ {
    case cond ~ tthen ~ eelse =>
      If(cond, tthen, eelse)
  }
  def fold = "(" ~> "fold" ~> exp ~ exp ~ ("(" ~> "lambda" ~> "(" ~> id) ~ (id <~ ")") ~ exp <~ ")" <~ ")" ^^ {
    case xs ~ z ~ x ~ acc ~ exp =>
      Fold(xs, z, x, acc, exp)
  }
  def op =
    "(" ~> op1 ~ exp <~ ")" ^^ { case op ~ x => Op1(op, x) } |
    "(" ~> op2 ~ exp ~ exp <~ ")" ^^ { case op ~ x ~ y => Op2(op, x, y) }

  def op1 = not | shl1 | shr16 | shr4 | shr1
  def not = "not" ^^^ Not
  def shl1 = "shl1" ^^^ Shl1
  def shr1 = "shr1" ^^^ Shr1
  def shr4 = "shr4" ^^^ Shr4
  def shr16 = "shr16" ^^^ Shr16

  def op2 = and | or | xor | plus
  def and = "and" ^^^ And
  def or = "or" ^^^ Or
  def xor = "xor" ^^^ Xor
  def plus = "plus" ^^^ Plus

  def apply(input: String): Option[Program] = parseAll(program, input) match {
    case Success(result, _) => Some(result)
    case failure: NoSuccess =>
      println("error: " + failure.msg)
      println(input.split("\n").map("\t" + _).mkString("\n"))
      print("\t")
      println((1 until failure.next.pos.column).map(_ => " ").mkString + "^")
      None
  }
}
