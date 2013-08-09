package icfpc2013

import java.math.BigInteger

object BvCompiler {
  type Context = Map[Id, Long]

  def apply(p: Program): Long => Long =
    l => apply(p.e)(Map(p.id -> l))

  def apply(e: Expression): Context => Long = e match {
    case Zero => _ => 0
    case One => _ => 1
    case id: Id => ctx => ctx(id)
    case If(cond, tthen, eelse) =>
      ctx => if (apply(cond)(ctx) != 0L) apply(tthen)(ctx) else apply(eelse)(ctx)
    case Fold(xs, z, x, acc, exp) =>
      ctx =>
        val base = apply(xs)(ctx)
        val zero = apply(z)(ctx)

        val mask = 0x00000000000000FF

        val ctx2 = ctx + (acc -> zero)

        (0 until 8).foldLeft(zero) { case (accc, i) =>
          val shift = i * 8
          val ctx3 = ctx2 + (acc -> accc) + (x -> ((base >> shift) & mask))
          apply(exp)(ctx3)
        }
    case Op1(op, x) =>
      op match {
        case Not =>
          ctx => ~apply(x)(ctx)
        case Shl1 =>
          ctx => apply(x)(ctx) << 1
        case Shr1 =>
          ctx => apply(x)(ctx) >>> 1
        case Shr4 =>
          ctx => apply(x)(ctx) >>> 4
        case Shr16 =>
          ctx => apply(x)(ctx) >>> 16
      }
    case Op2(op, x, y) =>
      op match {
        case And =>
          ctx => apply(x)(ctx) & apply(y)(ctx)
        case Or =>
          ctx => apply(x)(ctx) | apply(y)(ctx)
        case Xor =>
          ctx => apply(x)(ctx) ^ apply(y)(ctx)
        case Plus =>
          ctx => (BigInt(apply(x)(ctx).toHexString, 16) +
            BigInt(apply(y)(ctx).toHexString, 16)).longValue()
      }
  }

  def hexToLong(hex: String): Long = new BigInteger(hex.drop(2), 16).longValue
  def longToHex(l: Long): String = "0x" + "%1$16s".format(l.toHexString).replace(' ', '0')
}
