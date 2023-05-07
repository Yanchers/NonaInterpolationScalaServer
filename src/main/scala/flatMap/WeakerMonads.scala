package flatMap

import cats.{Applicative, Apply}

object WeakerMonads {

  trait MyFlatMap[M[_]] extends Apply[M] {
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    // TODO, hint: Apply extends Functor
    def ap[A, B](mf: M[A => B])(wa: M[A]): M[B] =
      flatMap(wa)(a => map(mf)(f => f(a)))
  }
  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M] {
    def pure[A](value: A): M[A]
    override def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(a => pure(f(a)))
  }

  import cats.FlatMap
  import cats.syntax.flatMap._ // flatMap extension method
  import cats.syntax.functor._ // map extension method

  def getPairs[M[_]: FlatMap, A, B](numbers: M[A], chars: M[B]): M[(A, B)] =
    for {
      n <- numbers
      c <- chars
    } yield (n, c)

  def main(args: Array[String]): Unit = {

  }
}
