package evaluation

object Evaluation {

  import cats.Eval
  val instantEval: Eval[Int] = Eval.now {
    println("Computing now")
    64353
  }
  val redoEval = Eval.always {
    println("Computing again")
    4232
  }
  val delayedEval = Eval.later {
    println("Computing later")
    1234
  }

  val composed = instantEval.flatMap(value1 => delayedEval.map(value2 => value1 + value2))

  val dontRecompute = redoEval.memoize

  def main(args: Array[String]): Unit = {
  }

}
