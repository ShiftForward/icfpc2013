package icfpc2013

import scala.slick.driver.SQLiteDriver.simple._
import Database.threadLocalSession

object RainbowTables extends Table[(String, String, String)]("RAINBOW_TABLES") {
  def problemId = column[String]("PROBLEM_ID")
  def program = column[String]("PROGRAM")
  def outputHash = column[String]("OUTPUT_BITSET")
  def * = problemId ~ program ~ outputHash
}


object RainbowTable {
  val ARG_NAME = "x"
  val DB_NAME = "rainbow"
  val db = Database.forURL(s"jdbc:sqlite:$DB_NAME.db", driver = "org.sqlite.JDBC")

  def create = db withSession { RainbowTables.ddl.create }

  def generate(problemId: String, size: Int, ops: Set[Operator]) = db withSession {
    val programs =
      ProgramGenerator.getPrograms(size, ops, Id(ARG_NAME), true).map { p =>
        (p, BvCompiler(p))
      }

    val hashes = programs.map { program =>
      StaticInput.long.map(program._2(_)).##
    }

    programs.zip(hashes).foreach { case ((program, _), hash) =>
      RainbowTables.insert(problemId, program.toString, hash.toString)
    }
  }
}
