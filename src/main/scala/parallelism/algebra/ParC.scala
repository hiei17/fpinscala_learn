package parallelism.algebra

import java.util.concurrent.{ExecutorService, Future, TimeUnit}


object Par {

  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](a: A) extends Future[A] {
    def isDone = true
    def get(): A = a
    def get(timeout: Long, units: TimeUnit): A = a
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }



  // spawns a parallel computation. The computation will not be spawned until forced by run .
  def fork[A](a: => Par[A]): Par[A] = {
    es=>es.submit(() => a)
  }

  def map[A,B](pa: Par[A])(f: A => B): Par[B] ={
    map2(pa, unit(()))((a,_) => f(a))
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }


  def unit[A](a: A): Par[A] = _=>UnitFuture(a)

  def async[A](a: => A): Par[A] = fork(unit(a))

  def choice[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A]={
/*    es=>{
      val bool = a(es).get()
      if(bool){
        ifTrue(es)
      }else{
        ifFalse(es)
      }
    }*/
    def f(a1:A,a2:A)={
      val bool = a(es).get()
      if(bool){
        ifTrue(es)
      }else{
        ifFalse(es)
      }
    }
    map2(ifFalse,ifFalse)(f)
  }

  def sequence[A](l: List[Par[A]]): Par[List[A]]={

    def f( zz:Par[List[A]],e:Par[A]):Par[List[A]]={
      map2(e,zz)(_::_)
    }
    val z:Par[List[A]]=unit(List[A]())
    l.foldLeft(z)(f)
  }




  def main(args: Array[String]): Unit = {
   val a: Par[Int]=sum2(IndexedSeq(1,5,6,4,2))

  }
}



