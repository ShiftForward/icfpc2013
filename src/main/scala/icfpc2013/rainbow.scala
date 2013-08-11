package icfpc2013

import scala.slick.driver.SQLiteDriver.simple._
import Database.threadLocalSession


object RainbowTables extends Table[(String, Array[Byte], String)]("RAINBOW_TABLES") {
  def problemId = column[String]("PROBLEM_ID")
  def program = column[Array[Byte]]("PROGRAM")
  def outputHash = column[String]("OUTPUT_BITSET")
  def * = problemId ~ program ~ outputHash

  def idx = index("idx_a", outputHash)
  // def idx = index("idx_a", (problemId, outputHash))
}

case class RainbowTable(dbName: String) {
  import Program._

  val ARG_NAME = "x"
  val db = Database.forURL(s"jdbc:sqlite:$dbName.db", driver = "org.sqlite.JDBC")
  val windowSize = 10000

  def create = db withSession { RainbowTables.ddl.create }

  def generate(problemId: String, size: Int, ops: Set[Operator], useAllOperators: Boolean = false) = db withSession {
    def programs =
      ProgramGenerator.getPrograms(size, ops, Id(ARG_NAME), useAllOperators).map { p =>
        (p, BvCompiler(p))
      }

    def hashes = programs.map { program =>
      StaticInput.long.map(program._2(_)).##
    }

    var i = 0
    programs.zip(hashes).sliding(windowSize).foreach { st =>
      RainbowTables.insertAll(
        st.map { case ((program, _), hash) =>
          (problemId, program: Array[Byte], hash.toString)
        }: _*
      )
    }
  }
}
