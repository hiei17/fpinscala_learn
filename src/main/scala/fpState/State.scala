package fpState
import State._
case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = flatMap(a=> unit(f(a)))


  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] ={
    State(s=>{
      val (a,s1) = this.run(s)
      val (b,s2) = sb.run(s1)
      val c:C = f(a,b)
      (c,s2)
    })
  }
  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s=>{
      val (s1,a) = this.run(s)
      val value :State[S,B]= f(a)
      value.run(s1)
    })
  }
}
object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]]={
    def f(z:State[S, List[A]],e:State[S, A]):State[S, List[A]]={
      z.map2(e)((list,b)=>b::list)
    }
    sas.foldLeft(unit[S, List[A]](List[A]()))(f)
  }

  def main(args: Array[String]): Unit = {
    def f(i:Int):Int={
      i+2
    }

    get[Int].flatMap(s=>{set[Int](f(s)).map(_=>())})

    State[Int,Int](a => (a, a))
      .flatMap(
        s=>{
          State[Int,Unit](_ => ((), f(s)))
            .map(_=>())
        }
    )

    State(s=>((),f(s)))
  }
}
