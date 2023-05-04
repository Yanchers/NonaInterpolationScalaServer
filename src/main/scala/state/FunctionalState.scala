package state


object FunctionalState {

  type MyState[S, A] = S => (S, A)
  import cats.data.State

  case class ShoppingCart(items: List[String], total: Double)
  def addToCart(item: String, price: Double): State[ShoppingCart, Double] =
    State { cart =>
      (cart.copy(cart.items :+ item, cart.total + price), cart.total + price)
    }

  val yanCart: State[ShoppingCart, Double] = for {
    _ <- addToCart("Fender guitar", 500)
    _ <- addToCart("Elixir strings", 19)
    total <- addToCart("Electric cable", 8)
  } yield total

  def inspect[A, B](f: A => B): State[A, B] = State(a => (a, f(a)))
  def get[A]: State[A, A] = State(s => (s, s))
  def set[A](value: A): State[A, Unit] = State(_ => (value, ()))
  def modify[A](f: A => A): State[A, Unit] = State(s => (f(s), ()))

  // methods available
  import cats.data.State._

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 43)
    c <- inspect[Int, Int](_ * 2)
  } yield (a, b, c)

  def main(args: Array[String]): Unit = {
    println(yanCart.run(ShoppingCart(Nil, 0)).value)
  }

}
