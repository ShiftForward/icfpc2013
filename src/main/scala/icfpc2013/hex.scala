package icfpc2013

object HexString {

  private[this] val HEX = Array[Char](
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F')

  def toLong(hex: String) = {
    var c: Char = ' '
    var value = 0L
    var i = 0
    val b: Byte = 0

    val chars = hex.toUpperCase.toCharArray

    while (i < 16) {
      c = chars(i)
      if (c >= '0' && c <= '9') {
        value = ((value << 4) | (0xff & (c - '0')))
      } else if (c >= 'A' && c <= 'F') {
        value = ((value << 4) | (0xff & (c - 'A' + 10)))
      } else {
        throw new NumberFormatException("Invalid hex character: " + c)
      }

      i += 1
    }

    value
  }

  def fromLong(l: Long) = {
    var value = l
    val hexs = new Array[Char](16)
    var i = 0
    var c: Int = 0

    while (i < 16) {
      c = (value & 0xf).toInt
      hexs(16 - i - 1) = HEX(c)
      value = value >> 4
      i += 1
    }

    new String(hexs)
  }
}
