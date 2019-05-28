package parallelism

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}


object Par {

  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](a: A) extends Future[A] {
    def isDone = true
    def get(): A = a
    def get(timeout: Long, units: TimeUnit): A = a
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }
  //extracts a value from a  by actually performing the computation
  // 这个时候才得到执行器 执行
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // spawns a parallel computation. The computation will not be spawned until forced by run .
  def fork[A](a: => Par[A]): Par[A] = {
    es=>es.submit(() => a)
  }

  def map[A,B](pa: Par[A])(f: A => B): Par[B] ={
    map2(pa, unit(()))((a,_) => f(a))
  }

  // combines the results of two parallel computations with a binary function.
  // 里面没有submit
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  //injects a constant into a parallel computation.
  // 里面没有submit
  def unit[A](a: A): Par[A] = _=>UnitFuture(a)

  def async[A](a: => A): Par[A] = fork(unit(a))

  // 原普通函数转成 返回Par
  def asyncF[A,B](f: A => B): A => Par[B]={
    a=>{es=>{es.submit(()=>f(a))}}
  }

  def parMap[A,B](l: List[A])(f: A => B): Par[List[B]]={

    //  asyncF : A => B A => Par[B]  parallel computation to produce the result.
    val fbs: List[Par[B]] = l.map(asyncF(f))
    sequence(fbs)
  }


  def sequence[A](l: List[Par[A]]): Par[List[A]]={

    def f( zz:Par[List[A]],e:Par[A]):Par[List[A]]={
      map2(e,zz)(_::_)
    }
    val z:Par[List[A]]=unit(List[A]())
    l.foldLeft(z)(f)
  }








  def sortPar(l: Par[List[Int]]): Par[List[Int]]={
    map(l)(_.sorted)
  }

def sum(as: List[Int]): Par[Int] = {
    if (as.size <= 1) Par.unit(as.headOption getOrElse 0)
    else {
      val (l,r) = as.splitAt(as.length/2)
      Par.map2(sum(l), sum(r))(_ + _)
    }
  }

  def sum2(as: IndexedSeq[Int]): Par[Int] =
    if (as.isEmpty) Par.unit(0)
    else {
      val (l,r) = as.splitAt(as.length/2)
      Par.map2(Par.fork(sum2(l)), Par.fork(sum2(r)))(_ + _)
    }



  def main(args: Array[String]): Unit = {
   val a: Par[Int]=sum2(IndexedSeq(1,5,6,4,2))

  }
}



