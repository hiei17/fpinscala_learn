package parallelism.algebra

import java.util.concurrent.{Callable, ExecutorService, Executors, Future, TimeUnit}


object Par {

  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](a: A) extends Future[A] {
    def isDone = true
    def get(): A = a
    def get(timeout: Long, units: TimeUnit): A = a
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)


  def fork[A](a: => Par[A]): Par[A] = {
    es=>es.submit(() => a)
  }

  def delay[A](fa: => Par[A]): Par[A] ={
    es => fa(es)
  }

  def unit[A](a: A): Par[A] = _=>UnitFuture(a)

  def async[A](a: => A): Par[A] = fork(unit(a))

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def main(args: Array[String]): Unit = {
    val a = async(42 + 1)
    val es = Executors.newFixedThreadPool(1)// 只给了一个线程
    //a, fork(a) 本来应该等价 设计时想要的性质 是 :x 等价 f(x) 这里本来应该是true
    println(Par.equal(es)(a, fork(a)))

    //但是 fork(async(42 + 1))(e).get 会卡死 因为 async(42 + 1)提交后,要计算出结果,get返回, 必须把42 + 1 提交到线程池, 而线程池满了,在计算async(42 + 1),
  }
}



