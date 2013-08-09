package icfpc2013

case class Program(id: Id, e: Expression) {
  override def toString = s"(lambda ($id) $e)"
  def size = 1 + e.size
  def operators = e match {
    case Fold(id, Zero, _, _, e) => Set[Operator](Tfold) union e.operators
    case _ => e.operators
  }
}

sealed trait Expression extends Any {
  def size: Int
  def operators: Set[Operator]
}
object Zero extends Expression {
  override def toString = "0"
  def size = 1
  def operators = Set()
}
object One extends Expression {
  override def toString = "1"
  def size = 1
  def operators = Set()
}
case class Id(s: String) extends AnyVal with Expression {
  override def toString = s
  def size = 1
  def operators = Set()
}
case class If(cond: Expression, tthen: Expression, eelse: Expression) extends Expression {
  override def toString = s"(if0 $cond $tthen $eelse)"
  def size = 1 + cond.size + tthen.size + eelse.size
  def operators = Set[Operator](If0) union cond.operators union tthen.operators union eelse.operators
}
case class Fold(xs: Expression, z: Expression, acc: Id, x: Id, exp: Expression) extends Expression {
  override def toString = s"(fold $xs $z (lambda ($acc $x) $exp))"
  def size = 2 + xs.size + z.size + exp.size
  def operators = Set[Operator](Fold0) union xs.operators union z.operators union exp.operators
}
case class Op1(op: Operator1, x: Expression) extends Expression {
  override def toString = s"($op $x)"
  def size = 1 + x.size
  def operators = Set[Operator](op) union x.operators
}
case class Op2(op: Operator2, x: Expression, y: Expression) extends Expression {
  override def toString = s"($op $x $y)"
  def size = 1 + x.size + y.size
  def operators = Set[Operator](op) union x.operators union y.operators
}

sealed trait Operator
object If0 extends Operator {
  override def toString = "if0"
}
object Tfold extends Operator {
  override def toString = "tfold"
}
object Fold0 extends Operator {
  override def toString = "fold"
}
sealed trait Operator1 extends Operator
object Not extends Operator1 {
  override def toString = "not"
}
object Shl1 extends Operator1 {
  override def toString = "shl1"
}
object Shr1 extends Operator1 {
  override def toString = "shr1"
}
object Shr4 extends Operator1 {
  override def toString = "shr4"
}
object Shr16 extends Operator1 {
  override def toString = "shr16"
}

sealed trait Operator2 extends Operator
object And extends Operator2 {
  override def toString = "and"
}
object Or extends Operator2 {
  override def toString = "or"
}
object Xor extends Operator2 {
  override def toString = "xor"
}
object Plus extends Operator2 {
  override def toString = "plus"
}

object Bv extends App {
  val program =
    Program(Id("x"), Fold(Id("x"), Zero, Id("y"), Id("z"), Op2(Or, Id("y"), Id("z"))))
  println("Program: " + program)
  println("Size: " + program.size)
  println("Operators: " + program.operators)
}
