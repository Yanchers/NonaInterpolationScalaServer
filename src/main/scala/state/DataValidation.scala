package state

import cats.Semigroup

import scala.annotation.tailrec
import scala.util.Try

object DataValidation {
  import cats.data.Validated
  val aValidValue: Validated[String, Int] = Validated.valid(42) // "right" value
  val anInvalidValue: Validated[String, Int] = Validated.invalid("Something went wrong") // "left" value
  val aTest: Validated[String, Int] = Validated.cond(42 > 39, 99, "meaning of life is too small")

  // TODO: use Either
  // n - must be a prime, must be non-negative, n <= 100, n must be even
  def testPrime(n: Int) = {
    @tailrec def tailrecPrime(d: Int): Boolean =
      if (d <= 1) true
      else n % d != 0 && tailrecPrime(d - 1)

    if (n == 0 || n == 1 || n == -1) false
    else tailrecPrime(Math.abs(n / 2))
  }
//  def testNumber(n: Int): Either[List[String], Int] = {
//    val errors = List(
//
//    )
//    Either.cond(n >= 0, "must be non-negative", n)
//  }

  import cats.instances.list._
  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance[Int](Math.max)
  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated.cond(n % 2 == 0, n, List("number must be even"))
      .combine(Validated.cond(n >= 0, n, List("number must be non-negative")))
      // and so on...

  // chain
  aValidValue.andThen(v => anInvalidValue)
  // test a valid value
  aValidValue.ensure(List("something went wrong"))(_ % 2 == 0)
  // transform
  aValidValue.map(_ + 1)
  aValidValue.leftMap(_.length)
  aValidValue.bimap(_.length, _ + 2)
  // interoperate with stdlib
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(42))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(None, List("Nothing present here"))
  val tryToValidate: Validated[Throwable, Int] = Validated.fromTry(Try("something".toInt))
  // backwards
  aValidValue.toOption
  aValidValue.toEither

  // TODO 2 - form validation
  object FormValidation {
    type FormValidation[T] = Validated[List[String], T]

    /*
    fields are name, email, password
    rules are:
    - all are required
    - name must not be blank
    - email must have @
    - password must have >= 10 characters
     */
    def validateForm(form: Map[String, String]): FormValidation[String] = {
      val nameOpt = form.get("name")
      val emailOpt = form.get("email")
      val passOpt = form.get("pass")
      Validated.fromOption(nameOpt, List("name is required")).ensure(List("name must not be blank"))(_.trim.nonEmpty) combine
        Validated.fromOption(emailOpt, List("email is required")).ensure(List("email must contain '@'"))(_.contains('@')) combine
        Validated.fromOption(passOpt, List("password is required")).ensure(List("password must be at least 10 chars"))(_.length >= 10) map
        (_ => "Success")
    }
  }
  val form = Map(
    "name" -> "yan",
    "email" -> "yan@mail",
    "pass" -> "123456789010"
  )

  import cats.syntax.validated._
  val aValidMeaningOfLife: Validated[List[String], Int] = 42.valid[List[String]]
  val anError: Validated[String, Int] = "Something went wrong".invalid[Int]

  def main(args: Array[String]): Unit = {
    println(FormValidation.validateForm(form))
  }

}
