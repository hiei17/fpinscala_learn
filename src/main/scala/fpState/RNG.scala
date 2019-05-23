package fpState
import State._


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }
  type Rand[A] = State[RNG, A]

  val int: Rand[Int] = State(r=>r.nextInt)



  // We need to be quite careful not to skew the generator.
  // Since `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)`,
  // it suffices to increment the negative numbers by 1 and make them positive.
  // This maps Int.MinValue to Int.MaxValue and -1 to 0.
  val nonNegativeInt:Rand[Int] =int.map(i=>{if (i < 0) -(i + 1) else i})


  val boolean:Rand[Boolean]=int.map(i=>{i%2==0})



  val double: Rand[Double] =nonNegativeInt.map(_ / (Int.MaxValue.toDouble + 1))



  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = ra.map2(rb)((_, _))

  val intDouble: Rand[(Int, Double)] =
    both(int, double)

  val doubleInt: Rand[(Double, Int)] =
    both(double, int)



  // It's interesting that we never actually need to talk about the `RNG` value
  // in `sequence`. This is a strong hint that we could make this function
  // polymorphic in that type.

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))



  def nonNegativeLessThan(n: Int): Rand[Int] = {
    nonNegativeInt.flatMap { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }


}
