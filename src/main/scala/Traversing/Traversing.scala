package Traversing

import cats.{Applicative, Foldable, Functor, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Traversing {
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val servers: List[String] = List("server-ci.rockthejvm.com", "test.yan.com", "prod.yan.com")
  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length * 10)

  // manual solution
  val allBandwidths: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) { (acc, hostname) =>
    for {
      b <- getBandwidth(hostname)
      list <- acc
    } yield list :+ b
  }
  // better
  val allBandwidthsTraverse: Future[List[Int]] = Future.traverse(servers)(getBandwidth)
  val allBandwidthsSequence: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))

  // TODO 1
  import cats.syntax.applicative._ // pure
  import cats.syntax.flatMap._ // flatMap
  import cats.syntax.functor._ // map
  import cats.syntax.apply._
  def listTraverse1[F[_]: Applicative, A, B](list: List[A])(f: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (acc, el) =>
      (acc, f(el)).mapN(_ :+ _)
//      for {
//        list <- acc
//        e <- f(el)
//      } yield list :+ e
    }

  // TODO 2
  def listSequence[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] =
    listTraverse1(list)(identity)
//    list.foldLeft(List.empty[A].pure[F]) { (acc, el) =>
//      acc
//    }

  // TODO 3
  import cats.instances.vector._
  val pairs1 = listSequence(List(Vector(1,2), Vector(3,4)))
  val pairs2 = listSequence(List(Vector(1,2), Vector(3,4), Vector(5,6)))

  import cats.instances.option._
  def filterAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] =
    listTraverse1[Option, Int, Int](list)(n => Some(n).filter(predicate))
  // TODO 4
  val allTrue = filterAsOption(List(2,4,6))(_ % 2 == 0)
  val someFalse = filterAsOption(List(1,2,3))(_ % 2 == 0)

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] =
    listTraverse1[ErrorsOr, Int, Int](list) { n =>
      if (predicate(n)) Validated.valid(n)
      else Validated.invalid(List(s"Predicate for $n is false"))
    }

  // TODO 5
  val allTrueValidated = filterAsValidated(List(2,4,6))(_ % 2 == 0)
  val someFalseValidated = filterAsValidated(List(1,2,3))(_ % 2 == 0)

  trait MyTraverse[L[_]] extends Foldable[L] with Functor[L] {
    def traverse[F[_] : Applicative, A, B](container: L[A])(f: A => F[B]): F[L[B]]
    def sequence[F[_] : Applicative, A](container: L[F[A]]): F[L[A]] =
      traverse(container)(identity)

    // TODO 6
    import cats.Id
    def map[A, B](wa: L[A])(f: A => B): L[B] =
      traverse[Id, A, B](wa)(f)
  }

  import cats.Traverse
  import cats.instances.future._
  val allBandwidthsCats = Traverse[List].traverse(servers)(getBandwidth)

  // extension methods
  import cats.syntax.traverse._ // sequence + traverse
  val allBandwidthsCats2 = servers.traverse(getBandwidth)



  def main(args: Array[String]): Unit = {
    println(pairs1)
    println(pairs2)
    println(allTrue)
    println(someFalse)
    println(allTrueValidated)
    println(someFalseValidated)
  }
}
