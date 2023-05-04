package readers

object Readers {

  case class Configuration(dbname: String, dbpass: String, host: String, port: Int, nThreads: Int, emailReplyTo: String)
  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = "dispatched"
    def getLastOrderId(username: String): Long = 543534
  }

  val config = Configuration("yan", "123", "locahost", 4141, 8, "yan@mail.ru")

  import cats.data.Reader
  val dbReader: Reader[Configuration, DbConnection] = Reader(conf => DbConnection(conf.dbname, conf.dbpass))
  val dbConn = dbReader.run(config)

  val yanOrderIdStatusRead: Reader[Configuration, String] = dbReader.map(_.getOrderStatus(1))
  val yanOrderStatus: String = yanOrderIdStatusRead.run(config)

  def getLastOrderStatus(username: String): String = {
    val res = for {
      usersLastOrderIdReader <- dbReader.map(_.getLastOrderId(username))
      usersLastOrderStatusReader <- dbReader.map(_.getOrderStatus(usersLastOrderIdReader))
    } yield usersLastOrderStatusReader
    res.run(config)
  }

  case class EmailService(reply: String) {
    def sendEmail(address: String, contents: String): String = "Email send"
  }
  def emailUser(username: String, email: String): String = {
    val emailReader = Reader[Configuration, EmailService](conf => EmailService(conf.emailReplyTo))
    val status = getLastOrderStatus(username)
    emailReader.map(_.sendEmail(email, status)).run(config)
  }


  def main(args: Array[String]): Unit = {

  }

}
