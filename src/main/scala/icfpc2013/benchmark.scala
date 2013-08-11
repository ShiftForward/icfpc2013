package icfpc2013

/**
 * Utility object for measuring the running time of a block of code.
 */
object Benchmark {

  /**
   * Runs a block of code and prints its running time to stdout.
   * @param name a name that identifies this benchmark
   * @param block the block of code to run
   * @tparam T the return type of the block
   * @return the value returned by the code block.
   */
  def apply[T](name: String)(block: => T) = {
    val start = System.currentTimeMillis
    try {
      block
    }
    finally {
      val diff = System.currentTimeMillis - start
      println("# Block \"" + name + "\" completed, time taken: " + diff + " ms (" + diff / 1000.0 + " s)")
    }
  }
}
