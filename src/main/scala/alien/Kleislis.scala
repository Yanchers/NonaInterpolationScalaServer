package alien

import cats.data.Reader

object Kleislis {

  val func1: Int => Option[String] = x => if (x % 2 == 0) Some(s"$x is even") else None
  val func2: Int => Option[Int] = x => Some(x * 3)

  // func3 = func2 andThen func1
  import cats.data.Kleisli
  import cats.instances.option._ // implicit FlatMap[Option]
  val func1K: Kleisli[Option, Int, String] = Kleisli(func1)
  val func2K: Kleisli[Option, Int, Int] = Kleisli(func2)
  val func3K: Kleisli[Option, Int, String] = func2K andThen func1K

  // convenience
  val multiply = func2K.map(_ * 2) // x => Option(...).map(_ * 2)
  val chain = func2K.flatMap(x => func1K)

  // TODO
  import cats.Id
  type InterestingKleisli[A, B] = Kleisli[Id, A, B] // wrapper over A => Id[B]
  val times2 = Reader[Int, Int](x => x * 2) // Kleisli == Reader[A, B]


  def main(args: Array[String]): Unit = {

  }

}
