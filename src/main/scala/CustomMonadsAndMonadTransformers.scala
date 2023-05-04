import cats.Monad

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object CustomMonadsAndMonadTransformers {

  implicit object OptionMonad extends Monad[Option] {
    override def pure[A](x: A): Option[A] = Option(x)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    @tailrec // tailrecM does NOT stack-overflow
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
      f(a) match {
        case None => None
        case Some(Left(v)) => tailRecM(v)(f)
        case Some(Right(b)) => Some(b)
      }
  }

  // ==========

  import cats.data.OptionT
  import cats.instances.list._ // fetch implicit Monad[List]

  val listOfOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option('c'), Option.empty[Char]))
  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char <- listOfCharOptions
    i <- listOfOptions
  } yield (i, char)

  import cats.data.EitherT
  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Left("something wrong"), Right(43), Right(42)))
  val listOfFutures: EitherT[Future, String, Int] = EitherT.right(Future(45))

  // TODO exercise
  val bandwidths = Map(
    "server1" -> 50,
    "server2" -> 300,
    "server3" -> 170,
  )
  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None => EitherT.left(Future("Server unreachable"))
    case Some(value) => EitherT.right(Future(value))
  }

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    bandWidth1 <- getBandwidth(s1)
    bandWidth2 <- getBandwidth(s2)
    sum = bandWidth1 + bandWidth2
  } yield sum > 250

  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] =
    canWithstandSurge(s1, s2).transform {
      case Left(_) => Left(s"Servers $s1 and $s2 cannot cope")
      case Right(false) => Left(s"Servers $s1 and $s2 cannot cope, not enough total bandwidth")
      case Right(true) => Right("Can cope with traffic spike")
    }

  def main(args: Array[String]): Unit = {
    println(listOfTuples.value)
  }
}
