object UsingMonads {
  import cats.Monad
  import cats.instances.list._
  val monadList = Monad[List] // fetch the implicit Monad[List]
  val aSimpleList = monadList.pure(1) // List(1)
  val anExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x + 1))

  val aManualEither: Either[String, Int] = Right(42)
  type LoadingOr[T] = Either[String, T]

  import cats.instances.either._
  val loadingMonad = Monad[LoadingOr]
  val anEither = loadingMonad.pure(45) // LoadingOr[Int] == Right(45)
  val aChangedLoading = loadingMonad.flatMap(anEither)(a => if (true) Right(a + 1) else Left("Loading..."))

  case class OrderStatus(orderId: Long, status: String)
  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(OrderStatus(orderId, "Ready to ship"))
  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available yet")
    else Right("Helsinki, Finland")

  val orderId = 457L
  val orderLocation = loadingMonad.flatMap(getOrderStatus(orderId))(trackLocation)
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  val orderLocationBetter = getOrderStatus(orderId).flatMap(trackLocation)
  val orderLocationFor = for {
    os <- getOrderStatus(orderId)
    loc <- trackLocation(os)
  } yield loc

  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }
  object optionService extends HttpService[Option] {
    def getConnection(cfg: Map[String, String]): Option[Connection] =
      for {
        h <- cfg.get("host")
        p <- cfg.get("port")
      } yield Connection(h, p)
//      if (cfg.contains("host") && cfg.contains("port"))
//        Option(Connection(cfg("host"), cfg("port")))
//      else
//        Option.empty[Connection]

    def issueRequest(connection: Connection, payload: String): Option[String] = {
      if (payload.length >= 20) None
      else Some(s"Request ($payload) has been accepted")
    }
  }


  def main(args: Array[String]): Unit = {
    val responseOption = optionService.getConnection(config)
  }
}
