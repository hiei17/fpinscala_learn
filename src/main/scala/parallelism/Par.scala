package parallelism


object Par {
  def fork[A](value: => Par[A]): Par[A] = ???


  def map2[A,B,C](value:  Par[A], value1:  Par[B])(f:(A,B)=>C):Par[C] = ???

  def unit[A](i: A): Par[A] = {
    Par(i,isOver = true)
  }
  def run[A](a: Par[A]): A = {}

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
   var r:Int= run(a)
  }
}

case class Par[A](a:A,isOver:Boolean) {


}
