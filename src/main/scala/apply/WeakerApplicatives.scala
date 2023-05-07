package apply

import cats.{Functor, Semigroupal}

object WeakerApplicatives {
  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {
    override def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] = {
      val functionWrapper: W[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }

    // TODO
    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] = {
      def variant1 = tuple match {
        case (wa, wb) =>
          val functionWrapper = map(wa)(a => (b: B) => f(a, b))
          ap(functionWrapper)(wb)
      }
      def variant2 = map(product(tuple._1, tuple._2)) {
        case (a, b) => f(a, b)
      }
      variant2
    }

    def ap[B, T](wf: W[B => T])(wb: W[B]): W[T] // fundamental
  }
  trait MyApplicative[W[_]] extends MyApply[W] {
    def pure[A](x: A): W[A] // fundamental
  }

  import cats.Apply
  import cats.instances.option._ // implicit Apply[Option]
  val applyOption = Apply[Option]
  val funcApp = applyOption.ap(Some((x: Int) => x + 1))(Some(2)) // Some(3)

  import cats.syntax.apply._ // extension methods from Apply
  val tupleOfOptions = (Option(1), Option(2), Option(3))
  val optionOfTuple = tupleOfOptions.tupled // Some((1,2,3))
  val sumOption = tupleOfOptions.mapN(_ + _ + _) // Some(6)
}