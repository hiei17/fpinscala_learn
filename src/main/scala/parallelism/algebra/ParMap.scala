package parallelism.algebra

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}




object ParMap {

  type Par[A] = ExecutorService => Future[A]

  def xA2PA[A]:A=>Future[A] = ???


  def map[A,B](pa: Par[A])(f: A => B): Par[B] ={

    es=>{
      val a: Future[A] = pa(es) //必须先用pa得到a 才能用f
      val b:B = f(a.get())// 到这步是确定的, 这2步都是签名确定的实现

      xA2PA(b)// 不一定 签名要求需要一个 B=>Future[B]的函数
    }
  }

// 限制: map(unit(x))(f) == unit(f(x))
  def unit[A](a: A): Par[A] = {
    _ =>{
      xA2PA(a)// 签名确定这里 需要一个 B=>Future[B] 的函数
    }
  }

  /**
    * 一个law: map(unit(1))(_ + 1) == unit(2)
    * law进程这样开始,作为一个等价的具体实例,
    * 什么是等价呢, 一样的入参结果总额相等 这2个函数就相等 这里是
    * def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    * p(e).get == p2(e).get
    *
    * 泛化这个law就是
    * map(unit(x))(f) == unit(f(x))
    * 对任何x f成立
    * 这个law 限制了实现
    *
    */

    //map(unit(x))(f):
    def left[A,B](x:A)(f: A => B): Par[B]={
      es:ExecutorService=>{
        val b:B = f(unit(x)(es).get()) // 对unit 限制 unit(任意x)(任意es).get() ==x

        xA2PA(b)//随便一个 B=>Future[B]
      }
    }

    // unit(f(x))
    def right[A,B](x:A)(f: A => B): Par[B]={
      es:ExecutorService=>{
        val b:B = f(x)//到这确定

        xA2PA(b)//map里面一样的一个  B=>Future[B]
      }
    }

    // 所以 map(unit(x))(f) == unit(f(x)) 对任何x f成立,对unit的限制是
    // 1.里面 B=>Future[B]和map里面一样
    // 2.unit(任意x)(任意es).get() ==x  unit是透明的 不能碰x, 对x包上又拆出来一样
    // unit: A=>Future[A], 有对应的一个函数 Future[A]=>A, 就是 unit有逆运算


    /**
      * map(unit(x))(f) == unit(f(x)) 对任何x f成立  ,可以推出  map(y)(id) == y 对任意y成立
      * 一一往下推导
      * map(unit(x))(f) == unit(f(x)) 对任何x f成立 , 那么对id这个f也成立 代入  推出=>
      * map(unit(x))(id) == unit(id(x))  右边的id算掉  推出
      * map(unit(x))(id) == unit(x) 对任意x成立, 那么unit(x)可以是任意值 推出=>
      * map(y)(id) == y 对任意y:Future[B]成立
      *
      */
    //id(x)==x
    def id[A](a: A): A = a


  //  map(y)(id) == y 对任意y:Future[B]成立 能看出对map里面 B=>Future[B](我取名为xA2PA的)的限制是 透明  不做其他事 xA2PA里面做任何事 只是把get掉的Future包回去
    // 说明map能保持外面的结构 只是对里面的值进行映射
   // 1.
   // map(y)(id):
    def left2[A](y:Par[A])={

      es:ExecutorService=>{
        xA2PA(y(es).get())
      }
    }
   /*
   2.
    es:ExecutorService=>{
      xA2PA(y(es).get())
    }
    等价
    es:ExecutorService=>{
     y(ex)
    }

    3.xA2PA 就是一模一样包回去
    xA2PA(y(es).get()) 等价  y(ex)

    */


    //这种透明叫  p structure-preserving
    //the parallel computation, only the value "inside" the computation.

    /**
      * 往上推也成立
      *
      * law1: map(y)(id) == y 对任意y:A=>Future[A]成立  保证law2: map(unit(x))(f) == unit(f(x)) 对任何x:A ,f:A=>B 成立  推导过程如下:
      *
      *依据law1 , law2右边=map(unit(f(x)))(id)
      * 那么就是要证明 map(unit(x))(f)
      *             ==map(unit(f(x)))(id)
      *
      *
      *
      *
      *
      *
      */





    /**
      *  map(y)(id) == y  对任何 y:Future[A] 都成立
      *  推出map(map(y)(g))(f) == map(y)(f(g)) 对任何 y g f 都成立
      *
      * 推理过程:
      *   设y=q(x), x:A ,q:A=>Future[A]
      *
      *   左边= map(map(q(x))(g))(f)
      *   =map(map(q(g(x)))(id))(f)
      *   =map(q(g(x)))(f)
      *   =map(q(f(g(x))))(id)
      *   =q(f(g(x)))
      *
      *   右边= map(q(x))(f(g))
      *   =map(q(f(g(x))))(id)
      *   =q(f(g(x)))
      *
      */

}



