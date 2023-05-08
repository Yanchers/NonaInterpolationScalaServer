package Traversing

import cats.{Applicative, Monad}

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

  def main(args: Array[String]): Unit = {
    println(pairs1)
    println(pairs2)
  }
}
