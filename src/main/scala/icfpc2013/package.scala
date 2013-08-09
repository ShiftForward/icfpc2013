
package object icfpc2013 {

  implicit class PimpedString(val str: String) extends AnyVal {
    def asBvProgram: Program = BvParser(str).get
  }

  implicit class PimpedProgram(val prog: Program) extends AnyVal {
    def run(input: Long): Long = BvCompiler(prog)(input)
  }
}
