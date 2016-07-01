import org.scalameter._
import common._

object FactParMeter extends App {

  def fact(n:Int) = (BigInt(2) to BigInt(n)).product

  trait Factorial {
    def prod(start:Int, end:Int): BigInt

    def apply(n:Int):BigInt = prod(2, n+1)
    def factorial = apply _
  }

  object FactSeq extends Factorial {

    def prod(start:Int, end:Int):BigInt = {
      var i=BigInt(start)
      var res=BigInt(1)
      while(i < end){
        res*=i
        i += 1
      }
      res
    }

  }

  object FactPar extends Factorial {

    private val THRESHOLD = 100

    def prod(start:Int, end:Int):BigInt =
      prod(THRESHOLD)(start, end)

    def prod(threshold:Int)(start:Int, end:Int) = {

      def aux(start:Int, end:Int):BigInt =
      {
        if (end-start < threshold)
          FactSeq.prod(start,end)
        else{
          val mid = (start+end)/2
          val (a,b) = parallel(aux(start,mid), aux(mid,end))
          a*b
        }
      }
      aux(start, end)
    }
  }

  object FactParAgg extends Factorial {

    private val THRESHOLD = 100

    case class BigIntAgg(val n:BigInt, var r:Int=1) {

      def +=(x:Int):BigIntAgg =
        if (r > (1<<32))
          BigIntAgg(n*r, x)
        else
          BigIntAgg(n, r*x)

      def toBigInt:BigInt = n*r

      def *(that:BigIntAgg) = new BigIntAgg(this.n*that.n*(this.r*that.r))

    }

    def prod(start:Int, end:Int):BigInt =
      (start until end).par.aggregate(BigIntAgg(1))(_ += _, _ * _).toBigInt

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

  val N:Int = 100000
  var res1=BigInt(0)
  var res2=BigInt(0)
  val tFactPar = measure{
    res1=FactPar(N)
  }
  val tFactParAgg = measure{
    res2=FactParAgg(N)
  }

  println(s"Parallel:   $tFactPar")
  println(s"Parallel:   $tFactParAgg")
  println(res1==res2)
  //println(res1)
  //println(res2)
}
