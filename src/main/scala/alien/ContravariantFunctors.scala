package alien

object ContravariantFunctors {

  trait Format[A] {
    def format(value : A): String
  }
  def format[A](value: A)(implicit format: Format[A]) = format.format(value)
  implicit object StringFormat extends Format[String] {
    override def format(value: String): String = "\"" + value + "\""
  }
  implicit object IntFormat extends Format[Int] {
    override def format(value: Int): String = value.toString
  }
  implicit object BooleanFormat extends Format[Boolean] {
    override def format(value: Boolean): String = if (value) "Y" else "N"
  }


  def main(args: Array[String]): Unit = {

  }
}
