package folding

import cats.Monoid

object Folding {

  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] = ???
    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = ???
    def filter[A](list: List[A])(predicate: A => Boolean): List[A] = ???
    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A = ???
  }

}
