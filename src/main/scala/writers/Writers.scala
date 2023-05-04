package writers

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Writers {

  import cats.data.Writer
  // 1 - define them at the start
  val aWriter: Writer[List[String], Int] = Writer(List("Started something"), 45)
  // 2 - manipulate them with pure FP
  val increasedWriter = aWriter.map(_ + 1) // value increases, logs stay the same
  val aLogsWriter = aWriter.mapWritten(_ :+ "found something interesting") // value stays the same, logs change
  val aWriterWithBoth = aWriter.bimap(
    _ +: "something 2",
    _ + 2
  )
  val aWriterWithBoth2 = aWriter.mapBoth { (logs, value) =>
    (logs :+ "found something interesting", value + 1)
  }

  import cats.instances.vector._ // imports a Semigroup[Vector]
  val writerA = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB = Writer(Vector("Log B1"), 40)
  val compositeWriter = for {
    a <- writerA
    b <- writerB
  } yield a + b

  // reset the logs
  import cats.instances.list._ // fetch a Monoid[List[Int]]
  val anEmptyWriter = aWriter.reset

  // 3 - dump either the value or the logs
  val desiredValue = aWriter.value
  val logs = aWriter.written
  val (l, v) = aWriter.run

  // TODO 1: rewrite a function wich "prints" things with writers
  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("Starting")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("starting!"), 0)
    else {
      countAndLog(n - 1).flatMap(_ => Writer(Vector(s"$n"), n))
    }
  }

  // TODO 2: rewrite with writers
  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}) = $lowerSum")
      lowerSum + n
    }
  }
  def writerSum(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector.empty, 0)
    else {
      for {
        _ <- Writer(Vector(s"Now at $n"), n)
        lowerSum <- writerSum(n - 1)
        _ <- Writer(Vector(s"Computed sum(${n - 1}) = $lowerSum"), n)
      } yield lowerSum + n

//      Writer(Vector(s"Now at $n"), n)
//      val lowerSum = writerSum(n - 1)
//      lowerSum.bimap(
//        _ +: s"Computed sum(${n - 1}) = $lowerSum",
//        _ => lowerSum.value + n
//      )
    }
  }

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  def main(args: Array[String]): Unit = {
//    println(compositeWriter.run)
//    countAndLog(10).written.foreach(println)
    Future {
      naiveSum(100)
    }
    val sumFuture1 = Future(writerSum(100))
    val sumFuture2 = Future(writerSum(100))
    val logs1 = sumFuture1.map(_.written) // logs from thread 1
    val logs2 = sumFuture2.map(_.written) // logs from thread 2
//    writerSum(100).written.foreach(println)
  }

}
