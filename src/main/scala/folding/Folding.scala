package folding

import cats.{Eval, Monoid}

object Folding {

  // TODO - implement all in terms of foldLeft
  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] =
      list.foldLeft(List.empty[B])(_ :+ f(_))
    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
      list.foldLeft(List.empty[List[B]])(_ :+ f(_)).flatten
    def filter[A](list: List[A])(predicate: A => Boolean): List[A] =
      list.foldLeft(List.empty[A]) { (list, a) =>
        if (predicate(a))
          list :+ a
        else
          list
      }
    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldLeft(monoid.empty)(monoid.combine)
  }

  import cats.Foldable
  import cats.instances.list._
  val sum = Foldable[List].foldLeft(List(1,2,3), 0)(_ + _) // 6
  import cats.instances.option._
  val sumOption = Foldable[Option].foldLeft(Option(2), 30)(_ + _) // 32

  //  foldRight is stack-safe regardless of your container
  val sumRight: Eval[Int] = Foldable[List].foldRight(List(1,2,3), Eval.now(0)) { (num, eval) =>
    eval.map(_ + num)
  }
  val anotherSum = Foldable[List].combineAll(List(1,2,3)) // implicit Monoid[Int]
  val mappedConcat = Foldable[List].foldMap(List(1,2,3))(_.toString) // implicit Monoid[String]

  // nesting
  import cats.instances.vector._
  val intNested = List(Vector(1,2,3), Vector(4,5,6))
  (Foldable[List] compose Foldable[Vector]).combineAll(intNested)

  // extension methods
  import cats.syntax.foldable._
  val sum3 = List(1,2,3).combineAll // required Foldable[List], Monoid[Int]
  val mappedConcat2 = List(1,2,3).foldMap(_.toString)


  def main(args: Array[String]): Unit = {
    import ListExercises._
    println(map((1 to 10).toList)(_ + 1))
    println(flatMap((1 to 10).toList)(x => (1 to x).toList))
    println(filter((1 to 10).toList)(_ % 2 != 0))
    println(combineAll((1 to 10).toList))
  }
}
