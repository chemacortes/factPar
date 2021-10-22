import org.scalameter._
import common._

object FactParMeter extends App {

  def fact(n: Int) = (BigInt(2) to BigInt(n)).product

  trait Factorial {
    def prod(start: Int, end: Int): BigInt

    def apply(n: Int): BigInt = prod(2, n + 1)
    def factorial = apply _
  }

  object FactSeq extends Factorial {

    def prod(start: Int, end: Int): BigInt = {
      var i = BigInt(start)
      var res = BigInt(1)
      while (i < end) {
        res *= i
        i += 1
      }
      res
    }

  }

  object FactPar extends Factorial {

    private val THRESHOLD = 100

    def prod(start: Int, end: Int): BigInt =
      prod(THRESHOLD)(start, end)

    def prod(threshold: Int)(start: Int, end: Int) = {

      def aux(start: Int, end: Int): BigInt = {
        if (end - start < threshold)
          FactSeq.prod(start, end)
        else {
          val mid = (start + end) / 2
          val (a, b) = parallel(aux(start, mid), aux(mid, end))
          a * b
        }
      }
      aux(start, end)
    }
  }

  object FactParAgg extends Factorial {

    private val THRESHOLD = 100

    type BigIntLong = (BigInt, Long)

    def sop(x: BigIntLong, p: Long): BigIntLong = {
      val (n, r) = x
      if (r > (1 << 32))
        (n * r, p)
      else
        (n, r * p)
    }

    def pop(x: BigIntLong, y: BigIntLong): BigIntLong = {
      val (xn, xr) = x
      val (yn, yr) = y
      ((xn * xr) * (yn * yr), 1)
    }

    def prod(start: Int, end: Int): BigInt =
      (start.toLong until end).par.aggregate((BigInt(1), 1L))(sop, pop)._1

  }

  /*
  var res:BigInt = 0
  val N:Int = 100000
  val tFactOriginal = measure {
    fact(N/5) //000)
  }
  val tFactSeq = measure {
    FactSeq(N/5)
  }
  val tFactPar = measure{
    FactPar(N)
  }
  val tFactParAgg = measure{
    FactParAgg(N)
  }


  //println(res)
  println("Factorial times")
  println(s"Original:   $tFactOriginal")
  println(s"Sequential: $tFactSeq")
  println(s"Parallel:   $tFactPar")
   */

  val N: Int = 100000
  var res1 = BigInt(0)
  var res2 = BigInt(0)
  val tFactPar = config(
    Key.exec.benchRuns -> 20,
    Key.verbose -> true
  ) withWarmer {
    new Warmer.Default
  } measure {
    res1 = FactPar(N)
  }
  val tFactParAgg = config(
    Key.exec.benchRuns -> 20,
    Key.verbose -> true
  ) withWarmer {
    new Warmer.Default
  } measure {
    res2 = FactParAgg(N)
  }

  println(s"Parallel recursive:      $tFactPar")
  println(s"Parallel with aggregate: $tFactParAgg")
  assert(res1 == res2, "Factorial results are different")
}
