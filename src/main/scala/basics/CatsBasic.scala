package basics

object CatsBasic {
    import cats.Eq
    import cats.instances.int._

    val aComparison = 2 == "a string"
    val intEquality = Eq[Int]
    val typeSafeComparison = intEquality.eqv(2, 3) // type safe, because we cant equal int with different type


    // use extension methods (if applicable)
    import cats.syntax.eq._
    val anotherTypeSafeComp = 2 === 3

    import cats.instances.list._
    val listComp = List(2) === List(3)


   // ### SEMIGROUP
    import cats.Semigroup
    import cats.instances.int._
    val naturalIntSemigroup = Semigroup[Int]
    val intCombination = naturalIntSemigroup.combine(1, 2) // addition
    import cats.instances.string._
    val naturalStringSemigroup = Semigroup[String]
    val stringCombination = naturalStringSemigroup.combine("scala", "cats") // concatenation

    // specific API
    def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)
    def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemigroup.combine)

    // general API
    def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(semigroup.combine)

    // TODO 1: support a new type
    case class Expense(id: Long, amount: Double)
    val expenseSemigroup = Semigroup.instance[Expense] { (e1, e2) =>
      Expense(Math.max(e1.id, e2.id) + 1, e1.amount + e2.amount) }

    import cats.syntax.semigroup._ // extension methopds from Semigroup - |+|
    val anIntSum = 2 |+| 3

    // TODO 2: implement reduceThins2 with the |+|
    def reduceThings2[T: Semigroup](list: List[T]): T = list.reduce((t1, t2) => t1 |+| t2)

    def main(): Unit = {
      println(intCombination)
      println(stringCombination)

      val numbers = (1 to 10).toList
      println(reduceInts(numbers))
    }


   // ## MONOIDS
    import cats.Semigroup
    import cats.instances.int._
    import cats.syntax.semigroup._

    import cats.Monoid
    val intMonoid = Monoid[Int]
    case class ShoppingCart(items: List[String], total: Double)
    def checkout(carts: List[ShoppingCart]): ShoppingCart = {
      implicit val cartMonoid = Monoid.instance[ShoppingCart](ShoppingCart(Nil, 0),
        (c1, c2) => ShoppingCart(c1.items ++ c2.items, c1.total + c2.total)
      )
      carts.fold(cartMonoid.empty)(_ |+| _)
    }



   // ## FUNCTORS
    import cats.Functor
    import cats.instances.list._
    val listFunctor = Functor[List]

    // generalize API
    def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] =
      functor.map(container)(_ * 10)

    // TODO 1: define your own functor for a binary tree
    trait Tree[+T]
    case class Leaf[+T](value: T) extends Tree[T]
    case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

    implicit object TreeFunctor extends Functor[Tree] {
      override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
        fa match {
          case Branch(value, left, right) =>
            Branch(f(value), map(left)(f), map(right)(f))
          case Leaf(value) =>
            Leaf(f(value))
        }
    }

//    def main(args: Array[String]): Unit = {
//      do10x(List(1,2,3))
//
//      println(
//        do10x[Tree]( // without specifying type, compiler won't compile, because cats Type Classes are INVARIANT
//          Branch(1, Leaf(2), Branch(3, Leaf(4), Leaf(0)))
//        )
//      )
//    }



   // ### MONADS
    trait MyMonad[M[_]] {
      def pure[A](value: A): M[A]
      def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
      def map[A, B](ma: M[A])(f: A => B): M[B] =
        flatMap(ma)(a => pure(f(a)))
    }

    import cats.Monad
    import cats.instances.option._
    val optionMonad = Monad[Option]
    val option = optionMonad.pure(4)
    val transformedOption = optionMonad.flatMap(option)(x => Some(x + 1))

    import cats.instances.list._
    val listMonad = Monad[List]
    val list = listMonad.pure(3)
    val transformedList = listMonad.flatMap(list)(x => List(x, x + 1))

    import cats.syntax.flatMap._
    import cats.syntax.functor._
    def getPairs[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      for {
        a <- fa
        b <- fb
      } yield (a, b)

    def main(args: Array[String]): Unit = {
      println(
        getPairs(
          Option(1),
          Option(1)
        )
      )
      println(
        getPairs(
          List(3),
          List(1, 2),
        )
      )
    }
}
