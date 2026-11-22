import common.*

import scala.collection.parallel.CollectionConverters.*
import scala.language.implicitConversions

def fact(n: Int) = (BigInt(2) to BigInt(n)).product

trait Factorial:
  def prod(start: Int, end: Int): BigInt

  def apply(n: Int): BigInt = prod(2, n + 1)
  def factorial = apply

object FactSeq extends Factorial:
  def prod(start: Int, end: Int): BigInt =
    var i = BigInt(start)
    var res = BigInt(1)
    while i < end do
      res *= i
      i += 1
    res

object FactPar extends Factorial:
  private val THRESHOLD = 100

  def prod(start: Int, end: Int): BigInt =
    prod(THRESHOLD)(start, end)

  def prod(threshold: Int)(start: Int, end: Int) =
    def aux(start: Int, end: Int): BigInt =
      if (end - start < threshold)
        FactSeq.prod(start, end)
      else
        val mid = (start + end) / 2
        val (a, b) = parallel(aux(start, mid), aux(mid, end))
        a * b
    aux(start, end)

object FactParAgg extends Factorial:
  private val THRESHOLD = 100

  type BigIntLong = (BigInt, Long)

  def sop(x: BigIntLong, p: Long): BigIntLong =
    val (n, r) = x
    if (r > (1 << 32))
      (n * r, p)
    else
      (n, r * p)

  def pop(x: BigIntLong, y: BigIntLong): BigIntLong =
    val (xn, xr) = x
    val (yn, yr) = y
    ((xn * xr) * (yn * yr), 1)

  def prod(start: Int, end: Int): BigInt =
    (start.toLong until end).par.aggregate((BigInt(1), 1L))(sop, pop)._1

@main def FactParMeter(): Unit =
  val N: Int = 100000
  var res1 = BigInt(0)
  var res2 = BigInt(0)

  def time[A](block: => A): Double =
    val start = System.nanoTime()
    block
    (System.nanoTime() - start) / 1_000_000.0

  val tFactPar = time:
    res1 = FactPar(N)
  val tFactParAgg = time:
    res2 = FactParAgg(N)

  println(f"Parallel recursive:      $tFactPar%.3f ms")
  println(f"Parallel with aggregate: $tFactParAgg%.3f ms")
  assert(res1 == res2, "Factorial results are different")
