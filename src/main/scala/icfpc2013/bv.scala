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
  def operatorIds: Int
}

object Zero extends Expression {
  override def toString = "0"
  def size = 1
  lazy val operators = Set[Operator]()
  lazy val operatorIds = 0
}

object One extends Expression {
  override def toString = "1"
  def size = 1
  lazy val operators = Set[Operator]()
  lazy val operatorIds = 0
}

case class Id(s: String) extends Expression {
  override def toString = s
  def size = 1
  lazy val operators = Set[Operator]()
  lazy val operatorIds = 0
}

case class If(cond: Expression, tthen: Expression, eelse: Expression) extends Expression {
  override def toString = s"(if0 $cond $tthen $eelse)"
  def size = 1 + cond.size + tthen.size + eelse.size
  lazy val operators = Set[Operator](If0) union cond.operators union tthen.operators union eelse.operators
  lazy val operatorIds = If0.id | cond.operatorIds | tthen.operatorIds | eelse.operatorIds
}

case class Fold(xs: Expression, z: Expression, x: Id, acc: Id, exp: Expression) extends Expression {
  override def toString = s"(fold $xs $z (lambda ($x $acc) $exp))"
  def size = 2 + xs.size + z.size + exp.size
  lazy val operators = Set[Operator](Fold0) union xs.operators union z.operators union exp.operators
  lazy val operatorIds = Fold0.id | xs.operatorIds | z.operatorIds | exp.operatorIds
}

case class Op1(op: Operator1, x: Expression) extends Expression {
  override def toString = s"($op $x)"
  def size = 1 + x.size
  lazy val operators = Set[Operator](op) union x.operators
  lazy val operatorIds = op.id | x.operatorIds
}

case class Op2(op: Operator2, x: Expression, y: Expression) extends Expression {
  override def toString = s"($op $x $y)"
  def size = 1 + x.size + y.size
  lazy val operators = Set[Operator](op) union x.operators union y.operators
  lazy val operatorIds = op.id | x.operatorIds | y.operatorIds
}

sealed trait Operator {
  def staticSize: Int
  def id: Int
}

object If0 extends Operator {
  override def toString = "if0"
  val staticSize = 1
  val id = 0
}

object Tfold extends Operator {
  override def toString = "tfold"
  val staticSize = 2
  val id = 1
}

object Fold0 extends Operator {
  override def toString = "fold"
  val staticSize = 2
  val id = 2
}

sealed trait Operator1 extends Operator {
  val staticSize = 2
}

object Not extends Operator1 {
  override def toString = "not"
  val id = 3
}

object Shl1 extends Operator1 {
  override def toString = "shl1"
  val id = 4
}

object Shr1 extends Operator1 {
  override def toString = "shr1"
  val id = 5
}

object Shr4 extends Operator1 {
  override def toString = "shr4"
  val id = 6
}

object Shr16 extends Operator1 {
  override def toString = "shr16"
  val id = 7
}

sealed trait Operator2 extends Operator {
  val staticSize = 2
}

object And extends Operator2 {
  override def toString = "and"
  val id = 8
}

object Or extends Operator2 {
  override def toString = "or"
  val id = 9
}

object Xor extends Operator2 {
  override def toString = "xor"
  val id = 10
}

object Plus extends Operator2 {
  override def toString = "plus"
  val id = 11
}

object Operator {
  val N_OPERATORS = 12

  def apply(str: String): Operator = str match {
    case "if0" => If0
    case "fold" => Fold0
    case "not" => Not
    case "shl1" => Shl1
    case "shr1" => Shr1
    case "shr4" => Shr4
    case "shr16" => Shr16
    case "and" => And
    case "or" => Or
    case "xor" => Xor
    case "plus" => Plus
    case "tfold" => Tfold
  }

  def apply(id: Int): Operator = id match {
    case If0.id => If0
    case Fold0.id => Fold0
    case Not.id => Not
    case Shl1.id => Shl1
    case Shr1.id => Shr1
    case Shr4.id => Shr4
    case Shr16.id => Shr16
    case And.id => And
    case Or.id => Or
    case Xor.id => Xor
    case Plus.id => Plus
    case Tfold.id => Tfold
  }

  implicit def idsToSet(ids: Int): Set[Operator] = {
    (0 until N_OPERATORS).foldLeft(Set[Operator]()) { (set, i) =>
      if ((ids & (1 << i)) > 0)
        set + Operator(i)
      else
        set
    }
  }
}

object Bv extends App {
  val program =
    Program(Id("x"), Fold(Id("x"), Zero, Id("y"), Id("z"), Op2(Or, Id("y"), Id("z"))))
  println("Program: " + program)
  println("Size: " + program.size)
  println("Operators: " + program.operators)
}
