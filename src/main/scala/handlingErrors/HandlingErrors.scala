package handlingErrors

import cats.{Applicative, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object HandlingErrors {

  trait MyApplciativeError[M[_], E] extends Applicative[M] {
    // pure from Applicative
    def raiseError[A](e: E): M[A]
    def handleErrorWith[A](ma: M[A])(f: E => M[A]): M[A]
    def handleError[A](ma: M[A])(f: E => A): M[A] = handleErrorWith(ma)(e => pure(f(e)))
  }
  trait MyMonadError[M[_], E] extends Monad[M]{
    def ensure[A](ma: M[A])(error: E)(predicate: A => Boolean): M[A]
  }

  import cats.MonadError
  import cats.instances.either._ // implicit MonadError
  type ErrorOr[A] = Either[String, A]
  val monadErrorEither = MonadError[ErrorOr, String]
  val success = monadErrorEither.pure(32) // Either[String, Int] == Right(32)
  val failure = monadErrorEither.raiseError[Int]("something wrong") // Either[String, Int] == Left("something wrong")
  // recover
  val handledError: ErrorOr[Int] = monadErrorEither.handleError(failure) {
    case "jopa" => 404
    case _ => 501
  }
  // recoverWith
  val handledError2: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure) {
    case "badness" => monadErrorEither.pure(44) // ErrorOr[Int]
    case _ => Left("Something else") // ErrorOr[Int]
  }
  // "filter"
  val filteredSuccess = monadErrorEither.ensure(success)("Number too small")(_ > 100)

  // Try and Future
  import cats.instances.try_._ // implicit MonadError[Try], E == Throwable
  val exception = new RuntimeException("Really bad")
  val pureException: Try[Int] = MonadError[Try, Throwable].raiseError(exception) // Try[Nothing], Try[Int]

  import cats.instances.future._
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  MonadError[Future, Throwable].raiseError(exception) // future which will complete with a Failure(exception)

  // applicatives => ApplicativeError
  import cats.data.Validated
  import cats.instances.list._ // implicit Semigroup[List] => ApplicativeError[ErrorsOr, List[String]]
  type ErrorsOr[T] = Validated[List[String], T]
  import cats.ApplicativeError
  val applicativeErrorVal = ApplicativeError[ErrorsOr, List[String]]
  // pure, raiseError, handleErrorWith

  // extension methods
  import cats.syntax.applicative._ // pure
  import cats.syntax.applicativeError._ // raiseError, handleError, handleErrorWith
  val extendedSuccess = 42.pure[ErrorsOr] // requires the implicit ApplicativeError[ErrorsOr, List[String]]
  val extendedError = List("Badness").raiseError[ErrorsOr, Int]
  val recoveredError: ErrorsOr[Int] = extendedError.recover {
    case _ => 43
  }

  import cats.syntax.monadError._ // ensure
  val testedSuccess = success.ensure("Smthng bad")(_ > 100)

  def main(args: Array[String]): Unit = {

  }
}
